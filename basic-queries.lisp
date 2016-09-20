(in-package :cl-kanren)

;; The absolute basics
(defun nullo (object)
  (== '() object))

(defun conso (car cdr cons)
  (== (cons car cdr) cons))

;; Conso intermediates
;;; You can do any of these with raw conso, but naming them makes things clearer.
(defun caro (cons car)
  (fresh (cdr) (== (cons car cdr) cons)))

(defun cdro (cons cdr)
  (fresh (car) (== (cons car cdr) cons)))

(defun pairo (pair?)
  (fresh (car cdr) (conso car cdr pair?)))

(defun eq-caro (list x)
  (caro list x))

;; Some list primitives
(defun listo (list)
  (conde ((nullo list) +succeed+)
         ((pairo list)
          (fresh (d)
            (cdro list d)
            (listo d)))
         (else +fail+)))

(defun membero (x list)
  (conde ((nullo list) +fail+)
         ((eq-caro list x) +succeed+)
         (else (fresh (d)
                 (cdro list d)
                 (membero x d)))))

(defun appendo (list rest out)
  (conde ((nullo list) (== rest out))
         (else (fresh (a d result)
                 (conso a d list)
                 (conso a result out)
                 (appendo d rest result)))))

;; Some tree primitives
(defun brancho (x tree)
  (fresh (car cdr)
    (conso car cdr tree)
    (conde ((nullo tree) +fail+)
	   ((== car x) +succeed+)
	   ((brancho x car))
	   (else (brancho x cdr)))))

(defun flatteno (list? out)
  (conde ((nullo list?) (== '() out))
         ((pairo list?)
          (fresh (a d result-car result-cdr)
            (conso a d list?)
            (flatteno a result-car)
            (flatteno d result-cdr)
            (appendo result-car result-cdr out)))
         (else (conso list? '() out))))
