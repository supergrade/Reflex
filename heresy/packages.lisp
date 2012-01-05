
;;; Copyright (c) 2007, Matthew Lamari (matt.lamari@gmail.com).  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package :cl-user)

(defpackage :heresy
  (:use :cl)
;  (:nicknames :heresy)
  #+:sbcl (:shadow :defconstant)
  (:export

   :composed
   :curried
   :rcurried

   :lazy-list


   :to-list
   :to-array
   :to-string

   :to-lazy-list

   :deferred-lazy-list
   :resolved
   :unresolved

   :slam

#|
   :diff-hash-table
   :diff-hash-table-changed
   :diff-hash-table-with-additions
   :diff-hash-table-with-addition
   :diff-hash-table-with-removals
   :diff-hash-table-with-removal
   :get-diff-hash
|#

   :make-const-hash-table
   :const-hash-table-count
   :const-hash-table-lookup
   :const-hash-table-with-changes
   :const-hash-table-with-additions
   :const-hash-table-with-removals
   :const-hash-table-with-addition
   :const-hash-table-with-removal
   :const-hash-table-key-value-pairs
   :const-hash-table-keys
   :const-hash-table-values


   :map/
   :listp/
   :lazy-listp
   :list/
   :filter/
   :let/
   :let*/
   :nub/
   :nub-by/
   :lazy
   :eager
   :assoc-list/
   :memoized/
   :loop-over/

   :split-on-test/
   :split-down-on-test/
   :split-positional/

   :group/
   :group-by/

#|
   :split-when/
   :split-at/
|#

   :intersperse/
   :length/
   :car/
   :cdr/
   :tail/
   :head/
   :head-tail/
   :first/
   :second/
   :third/
   :nth/
   :nthcdr/
   :drop/
   :take/
   :take-while/
   :drop-while/

   :position/

   :tails/
   :null/
   :non-null/
   :concat/
   :append/
   :assoc/
   :list*/
   :prepend/
   :iterate/
   :iteratex/
   :foldl/
   :foldl1/
   :foldr/
   :foldr1/

   :scanl/
   :scanl1/
   :scanr/
   :scanr1/

   :self-ref-list/

   :grouped-cdrs-by-car/
   :grouped-seconds-by-first/



   :sort/
   :sort-by/
   :and/
   :or/
   :reverse/

   :latch-on/
   :latch-off/

   :string-from-chars/

   :equal/


   :caar/
   :cdar/
   :cadr/
   :cddr/
   :caaar/
   :cdaar/
   :cadar/
   :cddar/
   :caadr/
   :cdadr/
   :caddr/
   :cdddr/
   :caaaar/
   :cdaaar/
   :cadaar/
   :cddaar/
   :caadar/
   :cdadar/
   :caddar/
   :cdddar/
   :caaadr/
   :cdaadr/
   :cadadr/
   :cddadr/
   :caaddr/
   :cdaddr/
   :cadddr/
   :cddddr/
   :caaaaar/
   :cdaaaar/
   :cadaaar/
   :cddaaar/
   :caadaar/
   :cdadaar/
   :caddaar/
   :cdddaar/
   :caaadar/
   :cdaadar/
   :cadadar/
   :cddadar/
   :caaddar/
   :cdaddar/
   :cadddar/
   :cddddar/
   :caaaadr/
   :cdaaadr/
   :cadaadr/
   :cddaadr/
   :caadadr/
   :cdadadr/
   :caddadr/
   :cdddadr/
   :caaaddr/
   :cdaaddr/
   :cadaddr/
   :cddaddr/
   :caadddr/
   :cdadddr/
   :caddddr/
   :cdddddr/
   ))

; (pushnew :heresy *features*)

