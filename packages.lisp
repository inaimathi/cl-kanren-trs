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

(defpackage :kanren-trs
  (:use :cl)
  (:import-from #:alexandria #:with-gensyms)
  (:export
;;;developer-interface
   #:unify #:walk* #:reify-subst
   ;;extendable generics
   #:equivp #:unify-impl
   #:walk-impl #:reify-subst-impl

;;;user-interface
   #:else
   #:+succeed+
   #:+fail+ #:jog #:run #:run* #:==
   #:fresh #:project #:conde #:condi #:condu
   #:all #:alli #:conda

;;;basic queries
   #:nullo #:conso #:caro #:cdro #:pairo #:eq-caro
   #:listo #:membero #:appendo
   #:brancho #:flatteno

;;;lib-functions
   #:choice-case #:map-choice #:make-nary-relation
   #:permute-binary-relation #:make-binary-relation
   #:permute-ternary-relation #:make-ternary-relation))
