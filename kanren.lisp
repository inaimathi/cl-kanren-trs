;;; Copyright 2008 Matthew Swank
;;;
;;; Redistribution and use in source and binary forms, with or without 
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;    1. Redistributions of source code must retain the above copyright 
;;; notice, this list of conditions and the following disclaimer.
;;;    2. Redistributions in binary form must reproduce the above copyright 
;;; notice, this list of conditions and the following disclaimer in the 
;;; documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;;; THE POSSIBILITY OF SUCH DAMAGE.

(in-package :kanren-trs)

(defmacro defconst (name value &optional (documentation nil docp))
  (let ((global (intern (format nil "%%~A" (symbol-name name)))))
    `(progn
       (defvar ,global ,value ,@(and docp `(,documentation)))
       (define-symbol-macro ,name (load-time-value ,global t)))))

;;;A -> stream
(defmacro unit (a) a)

;;;_ -> stream
(defmacro mzero () '+empty-stream+)

(defmacro all-aux (bnd &rest goals)
  (cond ((null goals)
         '+succeed+)
        ((null (cdr goals))
         (car goals))
        (t 
	 (with-gensyms (goal all-aux-subst)
	   (let ((remaining-goals (cdr goals)))
	     `(let ((,goal ,(car goals)))
		(lambda (,all-aux-subst)
		  (funcall ,bnd
			   (funcall ,goal ,all-aux-subst)
			   (lambda (,all-aux-subst)
			     (funcall (all-aux ,bnd ,@remaining-goals)
				      ,all-aux-subst))))))))))

;;;case statement for streams
;;;a stream is:
;;;  the empty-stream
;;;  a choice --a two member struct with a head and a funcallable tail 
;;;  any other object --counts as a singleton stream
;;;
(defmacro case-inf (expr on-zero ((a) &body on-one) ((ac f) &body on-choice))
  (with-gensyms (e)
    `(let ((,e ,expr))
       (cond ((eq +empty-stream+ ,e) ,on-zero)
	     ((choice-p ,e)
	      (let ((,ac (choice-head ,e))
		    (,f (choice-tail ,e)))
		,@on-choice))
	     (t (let ((,a ,e))
		  ,@on-one))))))

(defmacro ife (goal0 goal1 goal2)
  (with-gensyms (ife-subst)
    `#'(lambda (,ife-subst)
	 (mplus (funcall (all ,goal0 ,goal1) ,ife-subst)
		#'(lambda () (funcall ,goal2 ,ife-subst))))))

(defmacro ifi (goal0 goal1 goal2)
  (with-gensyms (ifi-subst)
    `#'(lambda (,ifi-subst)
	 (mplusi (funcall (all ,goal0 ,goal1) ,ifi-subst)
		 #'(lambda () (funcall ,goal2 ,ifi-subst))))))

(defmacro ifa (goal0 goal1 goal2)
  (with-gensyms (ifa-subst ifa-subst-inf fun)
    `#'(lambda (,ifa-subst)
	 (declare (ignorable ,ifa-subst))
	 (let ((,ifa-subst-inf (funcall ,goal0 ,ifa-subst)))
	   (case-inf ,ifa-subst-inf
		     (funcall ,goal2 ,ifa-subst)
		     ((,ifa-subst)
		      (funcall ,goal1 ,ifa-subst))
		     ((,ifa-subst ,fun)
		      (declare (ignore ,ifa-subst ,fun))
		      (bind ,ifa-subst-inf ,goal1)))))))

(defmacro ifu (goal0 goal1 goal2)
  (with-gensyms (ifu-subst ifu-subst-inf fun)
    `#'(lambda (,ifu-subst)
	 (let ((,ifu-subst-inf (funcall ,goal0 ,ifu-subst)))
	   (case-inf ,ifu-subst-inf
		     (funcall ,goal2 ,ifu-subst)
		     ((,ifu-subst)
		      (funcall ,goal1 ,ifu-subst))
		     ((,ifu-subst ,fun) 
		      (declare (ignore ,fun))
		      (funcall ,goal1 ,ifu-subst)))))))

;; TODO - destructure in the arg line here
(defmacro cond-aux (ifer &body clauses)
  (if (null clauses) 
      '+fail+
      (destructuring-bind ((&rest goals) &rest other-clauses) clauses
	(if (null other-clauses)
	    `(all ,@(if (and goals (eq (car goals) 'else))
			(cdr goals)
			goals))
	    (destructuring-bind (goal0 &rest other-goals) goals
	      `(,ifer ,goal0
		      (all ,@other-goals)
		      (cond-aux ,ifer ,@other-clauses)))))))

(defmacro jog ((var) &body goals)
  (with-gensyms (subst)
    `(let ((,var (id ',var)))
       (declare (ignorable ,var))
       (map-inf 
	#'(lambda (,subst)
	    (reify (walk* ,var ,subst)))
	(funcall (all ,@goals) +empty-subst+)))))

(defmacro run (num (var) &body goals)
  (with-gensyms (gen res res?)
    `(loop with ,gen = (jog (,var) ,@goals) 
	for (,res ,res?) = (multiple-value-list (funcall ,gen))
	,@(when num `(repeat ,num)) while ,res? collect ,res)))

(defmacro run* ((var) &body goals)
  `(run nil (,var) ,@goals))

(defmacro fresh ((&rest vars) &body goals)
  (with-gensyms (subst)
    `(lambda (,subst)
       (let ,(loop for v in vars collect `(,v (id ',v)))
	 (declare (ignorable ,@vars))
	 (funcall (all ,@goals) ,subst)))))

(defmacro conde (&body clauses)
  `(cond-aux ife ,@clauses))

(defmacro condi (&body clauses)
  `(cond-aux ifi ,@clauses))

(defmacro conda (&body clauses)
  `(cond-aux ifa ,@clauses))

(defmacro condu (&body clauses)
  `(cond-aux ifu ,@clauses))

(defmacro all (&rest goals)
  `(all-aux #'bind ,@goals))

(defmacro alli (&rest goals)
  `(all-aux #'bindi ,@goals))

(defconst +empty-subst+ nil)                             

(defconst +empty-stream+ (cons nil nil))

(defconst +succeed+ 
  #'(lambda (subst) 
      (unit subst)))

(defconst +fail+ 
  #'(lambda (subst)
      (declare (ignore subst))
      (mzero)))

(defstruct (id (:constructor id (name)))
  (name nil :read-only t :type symbol))

;;;choice: (T * (_ -> stream) -> stream)
;;;
(defstruct (choice (:constructor choice (head tail)))
  (head) 
  (tail (constantly +empty-stream+) :type function))

;;;(A -> B) * stream -> generator
(defun map-inf (p a-inf)
  (let ((rest a-inf))
    (lambda ()
      (case-inf rest
		(values nil nil)
		((a)
		 (setf rest +empty-stream+)
		 (values (funcall p a) t))
		((a f)
		 (setf rest (funcall f))
		 (values (funcall p a) t))))))


;;;stream * (_ -> stream) -> stream
(defun mplus (a-inf fun)
  (case-inf a-inf
	    (funcall fun)
	    ((a) (choice a fun))
	    ((a fun0)
	     (choice a #'(lambda ()
			   (mplus (funcall fun0) fun))))))

;;;stream * goal -> stream
;;;where goal <==> (T -> stream)
(defun bind (a-inf goal)
  (case-inf a-inf
	    (mzero)
	    ((a) (funcall goal a))
	    ((a f) (mplus (funcall goal a)
			  #'(lambda () (bind (funcall f) goal))))))

;;;stream * (_ -> stream) -> stream
(defun mplusi (a-inf fun)
  (case-inf a-inf
	    (funcall fun)
	    ((a) (choice a fun))
	    ((a fun0)
	     (choice a #'(lambda ()
			   (mplusi (funcall fun) fun0))))))

;;;stream * goal -> stream
;;;where goal <==> (T -> stream)
(defun bindi (a-inf goal)
  (case-inf a-inf
	    (mzero)
	    ((a) (funcall goal a))
	    ((a f) (mplusi (funcall goal a)
			   #'(lambda () (bindi (funcall f) goal))))))

(defun id-bound-p (id subst)
  (assoc id subst))

(defun binding-val (binding)
  (cdr binding))

(defun binding-id (binding)
  (car binding))

(defun walk (id? subst)
  (if (id-p id?)
      (let ((binding (id-bound-p id? subst)))
        (if binding
            (walk (binding-val binding) subst)
	    id?))
      id?))

(defun walk* (id? subst)
  (let ((id? (walk id? subst)))
    (walk-impl id? subst)))

#+ (or)
(defun walk* (id? subst)
  (let ((id? (walk id? subst)))
    (cond ((id-p id?) id?)
          ((consp id?)
           (cons (walk* (car id?) subst)
                 (walk* (cdr id?) subst)))

          ((vectorp id?)
           (map 'vector (lambda (id?)
			  (walk* id? subst))
                id?))
          (t id?))))

(defun reify-name (n)
  (declare (type integer n))
  (intern (format nil "_.~a" n) :keyword))

(defun reify-subst (id? subst)
  (let ((id? (walk id? subst)))
    (reify-subst-impl id? subst)))

#+ (or)
(defun reify-subst (id? subst)
  (let ((id? (walk id? subst)))
    (cond ((id-p id?)
           (extend-subst id? (reify-name (length subst)) subst))
          ((consp id?)
           (reify-subst (cdr id?) (reify-subst (car id?) subst)))
          (t subst))))

(defun reify (id?)
  (walk* id? (reify-subst id? ())))

(defun extend-subst (rhs lhs subst)
  (cons (cons rhs lhs) subst))

(defun unify (v w subst) 
  (let ((v (walk v subst))
        (w (walk w subst)))
    (if (eq v w) 
        subst
        (unify-impl v w subst))))

#+ (or) 
(defun unify (v w subst) 
  (let ((v (walk v subst))
        (w (walk w subst)))
    (cond ((eq v w) subst)
          ((id-p v)
           (extend-subst v w subst))
          ((id-p w)
           (extend-subst w v subst))
          ((and (consp v) (consp w))
           (let ((subst (unify (car v) (car w) subst)))
             (if (not (eq subst +fail+))
                 (unify (cdr v) (cdr w) subst)
                 +fail+)))
          ((equal v w) subst)
          (t +fail+))))


(defun == (v w)
  #'(lambda (subst)
      (let ((subst-1 (unify v w subst)))
        (if (not (eq subst-1 +fail+))
            (funcall +succeed+ subst-1)
	    (funcall +fail+ subst)))))

;;;public interface to extend unification
(defgeneric equivp (lhs rhs)
  (:method (lhs rhs)
    (eql lhs rhs))
  (:method ((lhs vector) (rhs vector))
    (or (eq lhs rhs)
	(and (eql (length lhs) (length rhs))
             (progn (map nil (lambda (l r)
                               (unless (equivp l r)
                                 (return-from equivp nil)))
                         lhs rhs)
                    t))))
  (:method ((lhs list) (rhs list))
    (or (eq lhs rhs)
        (and lhs rhs
             (equivp (car lhs) (car rhs))
             (equivp (cdr lhs) (cdr rhs))))))


(defgeneric unify-impl (v w subst)
  (:method (v w subst)
    (if (equivp v w) subst +fail+))
  (:method ((v id) w subst)
    (extend-subst v w subst))
  (:method (v (w id) subst)
    (extend-subst w v subst))
  (:method ((v cons) (w cons) subst)
    (let ((subst (unify (car v) (car w) subst)))
      (if (not (eq subst +fail+))
          (unify (cdr v) (cdr w) subst)
          +fail+)))
  (:method ((v vector) (w vector) subst)
    (let ((len (length v)))
      (cond ((= len (length w))
             (map nil (lambda (v w)
                        (setf subst (unify v w subst))
                        (when (eq subst +fail+)
                          (return-from unify-impl +fail+)))
                  v w)
             subst)
            (t +fail+)))))

(defgeneric walk-impl (val subst)
  (:method (val subst) 
    (declare (ignore subst))
    val)
  (:method ((val cons) subst)
    (cons (walk* (car val) subst)
          (walk* (cdr val) subst)))
  (:method ((val vector) subst)
    (map 'vector 
         (lambda (val)
           (walk* val subst))
         val)))

(defgeneric reify-subst-impl (val subst)
  (:method (val subst) 
    (declare (ignore val))
    subst)
  (:method ((val id) subst)
    (extend-subst val (reify-name (length subst)) subst))
  (:method ((val cons) subst)
    (reify-subst (cdr val) (reify-subst (car val) subst)))
  (:method ((val vector) subst)
    (reduce (lambda (subst item)
              (reify-subst item subst))
            val
            :initial-value subst)))
