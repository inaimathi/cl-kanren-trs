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

(in-package :kanren-trs-test)

;;;chapter 1
;;1.56
(defun teacupo (x)
  (conde ((== 'tea x) +succeed+) ;the succeed is unnecessary
         ((== 'cup x) +succeed+)
         (else +fail+)))         ;this line is superfluous

;;;chapter 2
;;2.9
(defun caro (cons car)
  (fresh (cdr)
    (== (cons car cdr) cons)))

;;2.16
(defun cdro (cons cdr)
  (fresh (car)
    (== (cons car cdr) cons)))

;;2.28
(defun conso (car cdr cons)
  (== (cons car cdr) cons))

;;2.35
(defun nullo (object)
  (== '() object))

;;2.40
(defun eqo (x y)
  (== x y))

;;2.53
(defun pairo (pair?)
  (fresh (car cdr)
    (conso car cdr pair?)))

;;;chapter 3
;;3.5
(defun listo (list)
  (conde ((nullo list) +succeed+)
         ((pairo list)
          (fresh (d)
            (cdro list d)
            (listo d)))
         (else +fail+)))

;;3.17
(defun lolo (list)
  (conde ((nullo list) +succeed+)
         ;;these two fresh clauses could be consolidated into one
         ((fresh (a) 
            (caro list a)
            (listo a))
          (fresh (d)
            (cdro list d)
            (lolo d)))
         (else +fail+)))

;;3.31
(defun twinso-0 (s)
  (fresh (x y)
    (conso x y s)
    (conso x () y)))

;;3.36
(defun twinso-1 (s)
  (fresh (x)
    (== `(,x ,x) s)))

(setf (symbol-function 'twinso) #'twinso-1)

;;3.37
(defun loto (list)
  (conde ((nullo list)
          +succeed+)
         ((fresh (a)
            (caro list a)
            (twinso a))
          (fresh (d)
            (cdro list d)
            (loto d)))
         (else +fail+)))

;;3.48
(defun listofo (predo list)
  (conde ((nullo list)
          +succeed+)
         ((fresh (a)
            (caro list a)
            (funcall predo a))
          (fresh (d)
            (cdro list d)
            (listofo predo d)))
         (else +fail+)))

;;3.50
(defun loto-1 (list)
  (listofo #'twinso list))

;;3.54
(defun eq-caro (list x)
  (caro list x))

;;3.54
(defun membero (x list)
  (conde ((nullo list) +fail+) 
         ((eq-caro list x) +succeed+) 
         (else (fresh (d)
                 (cdro list d)
                 (membero x d)))))

;;3.65
(defun list-identity (list)
  (run nil (y)
    (membero y list)))

;;3.80
(defun pmembero-0 (x list)
  (conde ((nullo list) +fail+)
         ((eq-caro list x) (cdro list '()))
         (else (fresh (d)
                 (cdro list d)
                 (pmembero-0 x d)))))

;;3.83 
(defun pmembero-1 (x list)
  (conde ((nullo list) +fail+)
         ((eq-caro list x) (cdro list '()))
         ((eq-caro list x) +succeed+)
         (else (fresh (d)
                 (cdro list d)
                 (pmembero-1 x d)))))

;;3.86 
(defun pmembero-2 (x list)
  (conde ((nullo list) +fail+)
         ((eq-caro list x) (cdro list '()))
         ((eq-caro list x) 
          (fresh (a d)
            (cdro list `(,a . ,d))))
         (else (fresh (d)
                 (cdro list d)
                 (pmembero-2 x d)))))

;;3.93
(defun pmembero-3 (x list)
  (conde ((eq-caro list x) 
          (fresh (a d)
            (cdro list `(,a . ,d))))
         ((eq-caro list x) (cdro list '()))
         (else (fresh (d)
                 (cdro list d)
                 (pmembero-3 x d)))))

;;3.95
(defun first-value (list)
  (run 1 (y)
    (membero y list)))

;;3.98
(defun memberrevo (x list)
  (conde ((nullo list) +fail+)
         (+succeed+
          (fresh (d)
            (cdro list d)
            (memberrevo x d)))
         (else (eq-caro list x))))

;;3.101
(defun reverse-list (list)
  (run nil (y)
    (memberrevo y list)))

;;;chapter 4
(defun memo-0 (x list out)
  (conde ((nullo list) +fail+)
         ((eq-caro list x) (== list out))
         (else (fresh (d)
                 (cdro list d)
                 (memo-0 x d out)))))

(defun memo-1 (x list out)
  (conde ((eq-caro list x) (== list out))
         (else (fresh (d)
                 (cdro list d)
                 (memo-1 x d out)))))

(defun remembero (x list out)
  (conde ((nullo list) (== '() out))
         ((eq-caro list x)
          (cdro list out))
         (else (fresh (a d result)
                 (conso a d list)
                 (remembero x d result)
                 (conso a result out)))))

(defun surpriseo (s)
  (remembero s '(a b c) '(a b c)))

;;;chapter 5
(defun appendo-0 (list rest out)
  (conde ((nullo list) (== rest out))
         (else (fresh (a d result)
                 (caro list a)
                 (cdro list d)
                 (appendo-0 d rest result)
                 (conso a result out)))))

(defun appendo-1 (list rest out)
  (conde ((nullo list) (== rest out))
         (else (fresh (a d result)
                 (conso a d list)
                 (appendo-1 d rest result)
                 (conso a result out)))))

(defun appendo-2 (list rest out)
  (conde ((nullo list) (== rest out))
         (else (fresh (a d result)
                 (conso a d list)
                 (conso a result out)
                 (appendo-2 d rest result)))))

(setf (symbol-function 'appendo) #'appendo-2)
                 
(defun swappendo (list rest out)
  (conde (+succeed+ (fresh (a d result)
                      (conso a d list)
                      (conso a result out)
                      (swappendo d rest result)))
         (else (nullo list) (== rest out))))

(defun unwrapo-0 (x out)
  (conde ((pairo x)
          (fresh (a)
            (caro x a)
            (unwrapo-0 a out)))
         (else (== x out))))

(defun unwrapo-1 (x out)
  (conde (+succeed+ (== x out))
         (else (fresh (a) ;note abscence of pairo
                 (caro x a)
                 (unwrapo-1 a out)))))

(defun flatteno (list? out)
  (conde ((nullo list?) (== '() out))
         ((pairo list?)
          (fresh (a d result-car result-cdr)
            (conso a d list?)
            (flatteno a result-car)
            (flatteno d result-cdr)
            (appendo result-car result-cdr out)))
         (else (conso list? '() out))))

(defun flattenrevo (list? out)
  (conde (+succeed+ (conso list? '() out))
         ((nullo list?) (== '() out))
         (else
          (fresh (a d result-car result-cdr)
            (conso a d list?)
            (flattenrevo a result-car)
            (flattenrevo d result-cdr)
            (appendo result-car result-cdr out)))))
;;;chapter 6
;;6.1
(eval-when  (:execute :load-toplevel :compile-toplevel)
  (defun anyo (goal)
    (conde (goal +succeed+)
           (else (anyo goal)))))

;;6.4
(defconst +never+ (anyo +fail+))

;;6.7
(defconst +always+ (anyo +succeed+))

;;6.12
(defconst +sal+ #'(lambda (goal)
                    (conde (+succeed+ +succeed+)
                           (else goal))))

;;;chapter 7
(defun bit-xoro (x y r)
  (conde ((== 0 x) (== 0 y) (== 0 r))
         ((== 1 x) (== 0 y) (== 1 r))
         ((== 0 x) (== 1 y) (== 1 r))
         ((== 1 x) (== 1 y) (== 0 r))
         (else +fail+)))

(defun bit-nando (x y r)
  (conde ((== 0 x) (== 0 y) (== 1 r))
         ((== 1 x) (== 0 y) (== 1 r))
         ((== 0 x) (== 1 y) (== 1 r))
         ((== 1 x) (== 1 y) (== 0 r))
         (else +fail+)))

(defun bit-ando (x y r)
  (conde ((== 0 x) (== 0 y) (== 0 r))
         ((== 1 x) (== 0 y) (== 0 r))
         ((== 0 x) (== 1 y) (== 0 r))
         ((== 1 x) (== 1 y) (== 1 r))
         (else +fail+)))

(defun half-addero (x y r c)
  (all (bit-xoro x y r)
       (bit-ando x y c)))

(defun full-addero (b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(defun build-num (n)
  (cond ((zerop n) '())
        ((oddp n) `(1 . ,(build-num (/ (- n 1) 2))))
        ((and (evenp n) (not (zerop n)))
         `(0 . ,(build-num (/ n 2))))))

(defun poso (n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(defun >1o (n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(defun addero (d n m r)
  (condi ((== 0 d) (== '() m) (== n r))
         ((== 0 d) (== '() n) (== m r)
          (poso m))
         ((== 1 d) (== '() m)
          (addero 0 n '(1) r))
         ((== 1 d) (== '() n)
          (addero 0 '(1) m r))
         ((== '(1) n) (== '(1) m)
          (fresh (a c)
            (== `(,a ,c) r)
            (full-addero d 1 1 a c)))
         ((== '(1) n) (gen-addero d n m r))
         ((== '(1) m) (>1o n) (>1o r)
          (addero d '(1) n r))
         ((>1o n) (gen-addero d n m r))
         (else +fail+)))

(defun gen-addero (d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (alli (full-addero d a b c e)
          (addero e x y z))))

(defun +o (n m k)
  (addero 0 n m k))

(defun -o (n m k)
  (+o m k n))

;;;chapter 8


;;;chapter 9 (just forms not already in the reference implementation)
(defun ext-s-check (rhs lhs subst)
  (cond ((occurs-check rhs lhs subst) +fail+)
        (t (extend-subst rhs lhs subst))))

(defun unify-check (v w subst) 
  (let ((v (walk v subst))
        (w (walk w subst)))
    (cond ((eq v w) subst)
          ((id-p v)
           (ext-s-check v w subst))
          ((id-p w)
           (ext-s-check w v subst))
          ((and (consp v) (consp w))
           (let ((subst (unify-check (car v) (car w) subst)))
             (if (not (eq subst +fail+))
                 (unify-check (cdr v) (cdr w) subst)
               +fail+)))
          ((equal v w) subst)
          (t +fail+))))

(defun ==-check (v w)
  #'(lambda (subst)
      (let ((subst-1 (unify-check v w subst)))
        (if (not (eq subst-1 +fail+))
            (funcall +succeed+ subst-1)
          (funcall +fail+ subst)))))

;;;chapter 10
(defun not-pastao (x)
  (conda ((== 'pasta x) +fail+)
         (else +succeed+)))

(defun onceo (goal)
  (condu (goal +succeed+)
         (else +fail+)))

(defun bumpo (n x)
  (conde ((== n x) +succeed+)
         (else (fresh (m)
                 (-o n `(1) m)
                 (bumpo m x)))))

(defun gen&testo (op i j k)
  (onceo (fresh (x y z)
           (funcall op x y z)
           (== i x)
           (== j y)
           (== k z))))

(defun enumerateo (op r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (funcall op i j k)
    (gen&testo op i j k)
    (== `(,i ,j ,k) r)))

(defun gen-addero-1 (d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (all (full-addero d a b c e)
         (addero e x y z))))


        




                 
