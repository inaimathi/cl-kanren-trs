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

(defmacro tests (&rest tests)
  `(lambda () 
     (list
      ,@(mapcar #'(lambda (test-pair)
                    (let ((test-result (car test-pair))
                          (test-expected (cadr test-pair)))
                      `(if (equal ,test-result ',test-expected)
                           t
                           (format nil "failed:~s -expected ~s -actual ~s" 
                                   ',(car test-pair) 
                                   ',test-expected 
                                   ,test-result))))
                tests))))

(defun failed-tests ()
  (reduce #'(lambda (current rest)
              (if (eq current t)
                  rest
                  (cons current rest)))
          (run-tests *tests*)
          :initial-value nil
          :from-end t))

(defun run-tests (tests)
  (funcall tests))

(defparameter *tests* 
  (tests 

;;;chapter 1
   ;;
   ;;1.10
   ((run nil (q) 
      +fail+) 
    ())
   
   ;;1.11
   ((run nil (q) 
      (== 't q))
    (t))
   
   ;;1.12
   ((run nil (q)
      +fail+
      (== 't q))
    ())
   
   ;;1.13-14
   ((run nil (q)
      +succeed+
      (== 't q))
    (t))
   
   ;;1.15-16
   ((run nil (r)
      +succeed+
      (== 'corn r))
    (corn))
   
   ;;1.17
   ((run nil (r)
      +fail+
      (== 'corn r))
    ())

   ;;1.18
   ((run nil (q)
      +succeed+
      (== 'nil q))
    (nil))

   ;;1.20
   ((run nil (q) 
      (let ((x 't))
        (== nil x))) 
    ())

   ;;1.21
   ((run nil (q) 
      (let ((x nil))
        (== nil x))) 
    (:_.0))

   ;;1.22
   ((run nil (x)
      (let ((x 'nil))
        (== 't x)))
    ())
   
   ;;1.23
   ((run nil (q)
      (fresh (x)
        (== 't x)
        (== 't q)))
    (t))

   ;;1.26
   ((run nil (q)
      (fresh (x)
        (== x 't)
        (== 't q)))
    (t))

   ;;1.27
   ((run nil (q)
      (fresh (x)
        (== x 't)
        (== q 't)))
    (t))

   ;;1.28
   ((run nil (x)
      +succeed+)
    (:_.0))         
   
   ;;1.29
   ((run nil (x)
      (let ((x 'nil))
        (declare (ignorable x))
        (fresh (x)
          (== 't x))))
    (:_.0))
   
   ;;1.30
   ((run nil (r)
      (fresh (x y)
        (== (cons x (cons y '())) r)))
    ((:_.0 :_.1)))
   
   ;;1.31
   ((run nil (s)
      (fresh (tee u)
        (== (cons tee (cons u '())) s)))
    ((:_.0 :_.1)))
   
   ;;1.32
   ((run nil (r)
      (fresh (x)
        (let ((y x))
          (fresh (x)
            (== (cons y (cons x (cons y '()))) r)))))
    ((:_.0 :_.1 :_.0)))

   ;;1.33
   ((run nil (r)
      (fresh (x)
        (let ((y x))
          (fresh (x)
            (== (cons x (cons y (cons x '()))) r)))))
    ((:_.0 :_.1 :_.0)))
   
   ;;1.34
   ((run nil (q)
      (== 'nil q)
      (== 't q))
    ())

   ;;1.35
   ((run nil (q)
      (== 'nil q)
      (== 'nil q))
    (nil))

   ;;1.36
   ((run nil (q)
      (let ((x q))
        (== 't x)))
    (t))

   ;;1.37
   ((run nil (r)
      (fresh (x)
        (== x r)))
    (:_.0))
   
   ;;1.38
   ((run nil (q)
      (fresh (x)
        (== 't x)
        (== x q)))
    (t))

   ;;1.39
   ((run nil (q)
      (fresh (x)
        (== x q)
        (== 't x)))
    (t))

   ;;1.40
   ((run nil (q)
      (fresh (x)
        (== 't x)
        (== x q)))
    (t))
   ((run nil (q)
      (fresh (x)
        (== (eq x q) q)))
    (nil))
   ((run nil (q)
      (let ((x q))
        (fresh (q)
          (== (eq x q) x))))
    (nil))

   ;;1.43
   ((run nil (q)
      (cond (nil +succeed+)
            (t +fail+))
      (== 't q))
    ())

   ;;1.44
   ((run nil (q)
      (conde (+fail+ +succeed+)
             (else +fail+))
      (== 't q))
    ())

   ;;1.45
   ((run nil (q)
      (conde (+fail+ +fail+)
             (else +succeed+))
      (== 't q))
    (t))

   ;;1.46
   ((run nil (q)
      (conde (+succeed+ +succeed+)
             (else +fail+))
      (== 't q))
    (t))

   ;;1.47
   ((run nil (x)
      (conde ((== 'olive x) +succeed+)
             ((== 'oil x) +succeed+)
             (else +fail+)))
    (olive oil))

   ;;1.49
   ((run 1 (x)
      (conde ((== 'olive x) +succeed+)
             ((== 'oil x) +succeed+)
             (else +fail+)))
    (olive))

   ;;1.50
   ((run nil (x)
      (conde ((== 'virgin x) +fail+)
             ((== 'olive x) +succeed+)
             (+succeed+ +succeed+)
             ((== 'oil x) +succeed+)
             (else +fail+)))
    (olive :_.0 oil))

   ;;1.52
   ((run 2 (x)
      (conde ((== 'extra x) +succeed+)
             ((== 'virgin x) +fail+)
             ((== 'olive x) +succeed+)
             ((== 'oil x) +succeed+)
             (else +fail+)))
    (extra olive))
    
   ;;1.53
   ((run nil (r)
      (fresh (x y)
        (== 'split x)
        (== 'pea y)
        (== (cons x (cons y '())) r)))
    ((split pea)))

   ;;1.54
   ((run nil (r)
      (fresh (x y)
        (conde ((== 'split x) (== 'pea y))
               ((== 'navy x) (== 'bean y))
               (else +fail+))
        (== (cons x (cons y '())) r)))
    ((split pea)(navy bean)))

   ;;1.55
   ((run nil (r)
      (fresh (x y)
        (conde ((== 'split x) (== 'pea y))
               ((== 'navy x) (== 'bean y))
               (else +fail+))
        (== (cons x (cons y (cons 'soup '()))) r)))
    ((split pea soup) (navy bean soup)))

   ;;1.56
   ((run nil (x)
      (teacupo x))
    (tea cup))

   ;;1.57
   ((run nil (r)
      (fresh (x y)
        (conde ((teacupo x) (== 't y) +succeed+)
               ((== 'nil x) (== 't y))
               (else +fail+))
        (== (cons x (cons y '())) r)))
    ((tea t)(cup t)(nil t)))

   ;;1.58
   ((run nil (r)
      (fresh (x y z)
        (conde ((== y x) (fresh (x)(== z x)))
               ((fresh (x) (== y x)) (== z x))
               (else +fail+))
        (== (cons y (cons z '())) r)))
    ((:_.0 :_.1)(:_.0 :_.1)))

   ;;1.59
   ((run nil (r)
      (fresh (x y z)
        (conde ((== y x) (fresh (x)(== z x)))
               ((fresh (x) (== y x)) (== z x))
               (else +fail+))
        (== 'nil x)
        (== (cons y (cons z '())) r)))
    ((nil :_.0)(:_.0 nil)))
    
   ;;1.60
   ((run nil (q)
      (let ((a (== 't q))
            (b (== 'nil q)))
        (declare (ignorable a))
        b))
    (nil))

   ;;1.61
   ((run nil (q)
      (let ((a (== 't q))
            (b (fresh (x)
                 (== x q)
                 (== 'nil x)))
            (c (conde ((== 't q) +succeed+)
                      (else (== 'nil q)))))
        (declare (ignorable a c))
        b))
    (nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;chapter 2
   ;;
   ;;2.2
   ((run nil (r)
      (fresh (y x)
        (== `(,x ,y) r)))
    ((:_.0 :_.1)))
    
   ;;2.3
   ((run nil (r)
      (fresh (v w)
        (== (let ((x v)
                  (y w))
              `(,x ,y))
            r)))
    ((:_.0 :_.1)))
    
   ;;2.6
   ((run nil (r)
      (caro '(a c o r n) r))
    (a))
    
   ;;2.7
   ((run nil (q)
      (caro '(a c o r n) 'a)
      (== 't q))
    (t))
    
   ;;2.8
   ((run nil (r)
      (fresh (x y)
        (caro `(,r ,y) x)
        (== 'pear x)))
    (pear))
    
   ;;2.11
   ((run nil (r)
      (fresh (x y)
        (caro '(grape raisin pear) x)
        (caro '((a)(b)(c)) y)
        (== (cons x y) r)))
    ((grape a)))
    
   ;;2.15
   ((run nil (r)
      (fresh (v)
        (cdro '(a c o r n) v)
        (caro v r)))
    (c))
    
   ;;2.18
   ((run nil (r)
      (fresh (x y)
        (cdro '(grape raisin pear) x)
        (caro '((a)(b)(c)) y)
        (== (cons x y) r)))
    (((raisin pear) a)))

   ;;2.19
   ((run nil (q)
      (cdro '(a c o r n) '(c o r n))
      (== 't q))
    (t))
    
   ;;2.20
   ((run nil (x)
      (cdro '(c o r n) `(,x r n)))
    (o))

   ;;2.21
   ((run nil (l)
      (fresh (x)
        (cdro l '(c o r n))
        (caro l x)
        (== 'a x)))
    ((a c o r n)))

   ;;2.22
   ((run nil (l)
      (conso '(a b c) '(d e) l))
    (((a b c) d e)))

   ;;2.23
   ((run nil (x)
      (conso x '(a b c) '(d a b c)))
    (d))

   ;;2.24
   ((run nil (r)
      (fresh (x y z)
        (== `(e a d ,x) r)
        (conso y `(a ,z c) r)))
    ((e a d c)))

   ;;2.25
   ((run nil (x)
      (conso x `(a ,x c) `(d a ,x c)))
    (d))

   ;;2.26
   ((run nil (l)
      (fresh (x)
        (== `(d a ,x c) l)
        (conso x `(a ,x c) l)))
    ((d a d c)))

   ;;2.27
   ((run nil (l)
      (fresh (x)
        (conso x `(a ,x c) l)
        (== `(d a ,x c) l)))
    ((d a d c)))

   ;;2.29
   ((run nil (l)
      (fresh (d x y w s)
        (conso w '(a n s) s)
        (cdro l s)
        (caro l x)
        (== 'b x)
        (cdro l d)
        (caro d y)
        (== 'e y)))
    ((b e a n s)))

   ;;2.32
   ((run nil (q)
      (nullo '(grape raisin pear))
      (== 't q))
    ())

   ;;2.33
   ((run nil (q)
      (nullo '())
      (== 't q))
    (t))

   ;;2.34
   ((run nil (x)
      (nullo x))
    (()))

   ;;2.38
   ((run nil (q)
     (eqo 'pear 'plum)
     (== 't q))
    ())

   ;;2.39
   ((run nil (q)
      (eqo 'plum 'plum)
      (== 't q))
    (t))

   ;;2.52
   ((run nil (r)
      (fresh (x y)
        (== (cons x (cons y 'salad)) r)))
    ((:_.0 :_.1 . salad)))

   ;;2.54
   ((run nil (q)
      (pairo (cons q q))
      (== 't q))
    (t))

   ;;2.55
   ((run nil (q)
      (pairo '())
      (== 't q))
    ())

   ;;2.56
   ((run nil (q)
      (pairo 'pair)
      (== 't q))
    ())

   ;;2.57
   ((run nil (x)
      (pairo x))
    ((:_.0 . :_.1)))

   ;;2.58
   ((run nil (r)
      (pairo (cons r 'pear)))
    (:_.0))
;;;;;;;;;;;;;;;;;;;;;;;
;;;chapter 3
   ;;
   ;;3.7
   ((run nil (x)
      (listo `(a b ,x d)))
    (:_.0))

   ;;3.10
   ((run 1 (x)
      (listo `(a b c . ,x)))
    (()))

   ;;3.14
   ((run 5 (x)
      (listo `(a b c . ,x)))
    (()
     (:_.0)
     (:_.0 :_.1)
     (:_.0 :_.1 :_.2)
     (:_.0 :_.1 :_.2 :_.3)))
    
   ;;3.20
   ((run 1 (l)
      (lolo l))
    (()))
    
   ;;3.21
   ((run nil (q)
      (fresh (x y)
        (lolo `((a b)(,x c)(d ,y)))
        (== 't q)))
    (t))

   ;;3.22
   ((run 1 (q)
      (fresh (x)
        (lolo `((a b) . ,x))
        (== 't q)))
    (t))

   ;;3.23
   ((run 1 (x)
      (lolo `((a b)(c d) . ,x)))
    (()))

   ;;3.24
   ((run 5 (x)
      (lolo `((a b)(c d) . ,x)))
    (()
     (())
     (() ())
     (() () ())
     (() () () ())))
    
   ;;3.32
   ((run nil (q)
      (twinso-0 '(tofu tofu))
      (== 't q))
    (t))
   ((run nil (q)
      (twinso-1 '(tofu tofu))
      (== 't q))
    (t))
    
   ;;3.33
   ((run nil (z)
      (twinso-0 `(,z tofu)))
    (tofu))
   ((run nil (z)
      (twinso-1 `(,z tofu)))
    (tofu))

   ;;3.38
   ((run 1 (z)
      (loto `((g g) . ,z)))
    (()))

   ;;3.42
   ((run 5 (z)
      (loto `((g g) . ,z)))
    (()
     ((:_.0 :_.0))
     ((:_.0 :_.0) (:_.1 :_.1))
     ((:_.0 :_.0) (:_.1 :_.1) (:_.2 :_.2))
     ((:_.0 :_.0) (:_.1 :_.1) (:_.2 :_.2) (:_.3 :_.3))))
   
   ;;3:45
   ((run 5 (r)
      (fresh (w x y z)
        (loto `((g g) (e ,w) (,x ,y) . ,z))
        (== `(,w (,x ,y) ,z) r)))
    ((e (:_.0 :_.0) ())
     (e (:_.0 :_.0) ((:_.1 :_.1)))
     (e (:_.0 :_.0) ((:_.1 :_.1) (:_.2 :_.2)))
     (e (:_.0 :_.0) ((:_.1 :_.1) (:_.2 :_.2) (:_.3 :_.3)))
     (e (:_.0 :_.0) ((:_.1 :_.1) (:_.2 :_.2) (:_.3 :_.3) (:_.4 :_.4)))))

   ;;3.47
   ((run 3 (out)
      (fresh (w x y z)
        (== `((g g) (e ,w) (,x ,y) . ,z) out)
        (loto out)))
    (((g g) (e e) (:_.0 :_.0))
     ((g g) (e e) (:_.0 :_.0) (:_.1 :_.1))
     ((g g) (e e) (:_.0 :_.0) (:_.1 :_.1) (:_.2 :_.2))))
   
   ;;3.49
   ((run 3 (out)
      (fresh (w x y z)
        (== `((g g) (e ,w) (,x ,y) . ,z) out)
        (listofo #'twinso out)))
    (((g g) (e e) (:_.0 :_.0))
     ((g g) (e e) (:_.0 :_.0) (:_.1 :_.1))
     ((g g) (e e) (:_.0 :_.0) (:_.1 :_.1) (:_.2 :_.2))))
   
   ;;3.57
   ((run nil (q)
     (membero 'olive '(virgin olive oil))
     (== 't q))
    (t))

   ;;3.58
   ((run 1 (y)
      (membero y '(hummus with pita)))
    (hummus))

   ;;3.59
   ((run 1 (y)
      (membero y '(with pita)))
    (with))
   
   ;;3.60
   ((run 1 (y)
      (membero y '(pita)))
    (pita))

   ;;3.61
   ((run 1 (y)
      (membero y '()))
    ())

   ;;3.62
   ((run nil (y)
      (membero y '(hummus with pita)))
    (hummus with pita))

   ;;3.66
   ((run nil (x)
     (membero 'e `(pasta ,x fagioli)))
    (e))

   ;;3.69
   ((run nil (x)
     (membero 'e `(pasta e ,x fagioli)))
    (:_.0 e))

   ;;3.70
   ((run nil (x)
     (membero 'e `(pasta ,x e fagioli)))
    (e :_.0))

   ;;3.71
   ((run nil (r)
     (fresh (x y)
       (membero 'e `(pasta ,x fagioli ,y))
       (== `(,x ,y) r)))
    ((e :_.0) (:_.0 e)))

   ;;3.73
   ((run 1 (l)
     (membero 'tofu l))
    ((tofu . :_.0)))

   ;;3.76
   ((run 5 (l)
     (membero 'tofu l))
    ((tofu . :_.0)
     (:_.0 tofu . :_.1)
     (:_.0 :_.1 tofu . :_.2)
     (:_.0 :_.1 :_.2 tofu . :_.3)
     (:_.0 :_.1 :_.2 :_.3 tofu . :_.4)))
   
   ;;3.81

;;;;;;;;;;;;;;;;;;;;;;;;
   ;;6.24
   ((run 5 (r)
      (condi ((teacupo r) +succeed+)
             ((== nil r) +succeed+)
             (else +fail+)))
    (tea nil cup))
   
   ;;6.24
   ((run 5 (q)
      (condi ((== 'nil q) +always+)
             ((== 't q) +always+)
             (else +fail+))
      (== 't q))
    (t t t t t))
;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;10.1
   ((run nil (q)
     (conda (+fail+ +succeed+)
            (else +fail+)))
    ())

   ;;10.2
   ((run nil (q)
     (conda (+fail+ +succeed+)
            (else +succeed+)))
    (:_.0))

   ;;10.3
   ((run nil (q)
     (conda (+succeed+ +fail+)
            (else +succeed+)))
    ())

   ;;10.4
   ((run nil (q)
     (conda (+succeed+ +succeed+)
            (else +fail+)))
    (:_.0))

   ;;10.5
   ((run nil (x)
     (conda ((== 'olive x) +succeed+)
            ((== 'oil x) +succeed+)
            (else +fail+)))
    (olive))

   ;;10.7
   ((run nil (x)
     (conda ((== 'virgin x) +fail+)
            ((== 'olive x) +succeed+)
            ((== 'oil x) +succeed+)
            (else +fail+)))
    ())
   
   ;;10.14
   ((run nil (q)
     (condu (+always+ +succeed+)
            (else +fail+))
     (== 't q))
    (t))
   
   ;;10.18
   ((run 1 (q)
     (condu (+always+ +succeed+)
            (else +fail+))
     +fail+
     (== 't q))
    ())
))

