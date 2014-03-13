;;; Copyright (c) 2008, Matthew Swank
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;;; THE POSSIBILITY OF SUCH DAMAGE.

(in-package :kanren-trs)

(defmacro choice-case (key-term &body cases)
  (let ((kt-name (gensym)))
    `(fresh (,kt-name)
       (== ,key-term ,kt-name)
       (conde ,@(mapcar (lambda (case)
                          (destructuring-bind (keys &rest clauses) case
                            (cond ((eql keys 'else)
                                   clauses)
                                  ((consp keys) 
                                   (if (cdr keys)
                                       `((conde ,@(mapcar (lambda (key)
                                                        `(== ,kt-name ',key))
                                                      keys))
                                         ,@clauses)
                                       `((== ,kt-name ',(car keys))
                                         ,@clauses)))
                                  (t `((== ,kt-name ',keys)
                                       ,@clauses)))))
                        cases)))))

(defun map-choice (fun &rest bindings)
  (labels ((compose-bindings (relation bindings)
             (if (some #'null bindings)
                 relation
                 (let ((terms (mapcar #'car bindings)))
                   (compose-bindings (conde (relation)
                                            ((apply fun terms)))
                                     (mapcar #'cdr bindings))))))
    (compose-bindings +fail+ bindings)))

(defun permute-binary-relation (relation)
  (lambda (a b)
    (conde ((funcall relation a b))
           ((funcall relation b a)))))

(defun make-binary-relation (mapping)
  (lambda (a b)
    (map-choice (lambda (a1 b1)
                  (fresh () 
                    (== a a1)
                    (== b b1)))
                (mapcar #'first mapping)
                (mapcar #'second mapping))))

;;this needs to confirm that compile time evaluation is possible:
;;mapping is a quoted list, n is a number, etc
#+ (or) 
(define-compiler-macro make-nary-relation (n mapping) 
  (let* ((maps (loop :for x :from 0 :below n
                  :collect `',(mapcar (lambda (list)
                                     (nth x list))
                                   mapping)))
         (args (loop :for x :from 0 :below n
                  :collect (gensym)))
         (args1 (loop :for x :from 0 :below n
                   :collect (gensym)))
         (sequence (mapcar (lambda (a a1)
                             `(== ,a ,a1))
                           args
                           args1)))
    `(lambda ,args
       (map-choice (lambda ,args1
                     (fresh ()
                       ,@sequence))
                   ,@maps))))

(defun make-nary-relation (n mapping)
  (let ((maps (loop :for x :from 0 :below n
                 :collect (mapcar (lambda (list)
                                    (nth x list))
                                  mapping))))
    (lambda (&rest args)
      (unless (= (length args) n)
        (error "invalid number of arguments"))
      (apply #'map-choice 
             (lambda (&rest args1)
               (let ((sequence nil))
                 (map nil (lambda (a a1)
                            (unless sequence 
                              (setf sequence (== a a1)))
                            ;; we don't want to capture the binding
                            ;; (this should be a fold)
                            (let ((seq sequence))
                              (setf sequence (fresh () seq (== a a1)))))
                      args
                      args1)
                 sequence))
             maps))))

(defun permute-ternary-relation (relation)
  (lambda (a b c)
    (conde ((funcall relation a b c))
           ((funcall relation a c b))
           ((funcall relation c b a))
           ((funcall relation b a c))
           ((funcall relation c a b))
           ((funcall relation b c a)))))

(defun make-ternary-relation (mapping)
  (lambda (a b c)
    (map-choice (lambda (a1 b1 c1)
                  (fresh () 
                    (== a a1)
                    (== b b1)
                    (== c c1)))
                (mapcar #'first mapping)
                (mapcar #'second mapping)
                (mapcar #'third mapping))))