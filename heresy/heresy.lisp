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


; (load "c:/prog/lisp/common/stdutils.lisp")

(in-package :heresy)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-user::load-lutils))



(eval-when (:compile-toplevel :load-toplevel :execute)

#|
  (define-compiler-macro funcall2 (&whole form &rest params)
    `(handler-case
         (funcall ,@params)
       (error (err) (error (format nil "~S ~S" err ,params)))))
  (defun funcall2 (&rest rest) (apply #'funcall rest))
|#


  #|
  (defmacro defunm (function-name params &body body)
    `(progn
       (defun ,function-name ,params ,(eval `(let ,(mapcar (lambda (elt) `(,elt ',elt)) params) ,@body)))
       (define-compiler-macro ,function-name ,params ,@body)))
|#
  (defclass function-form () ())

  (defclass function-form-symbol (function-form)
    ((symbol :type symbol :initarg :symbol :reader get-symbol)))

  (defclass function-form-lambda (function-form)
    ((args :initarg :args :type list :reader get-args)
     (body :initarg :body :type list :reader get-body)))

  (defclass function-form-expression (function-form)
    ((expression :initarg :expression :reader get-expression)))

  (defclass function-form-partial (function-form)
    ((sub-function-form :initarg :sub-function-form :type function-form :reader get-sub-function-form)
     (known-args :initarg :known-args :type list :reader get-known-args)
     (new-arg-side :initarg :new-arg-side :type symbol :reader get-new-arg-side))) ; new-arg-side should be :right for curried, :left for rcurried

  (defclass function-form-composed (function-form)
    ((functions-reversed :initarg :functions-reversed :type list :reader get-functions-reversed)))

  (defclass function-form-constantly (function-form)
    ((value :initarg :value :reader get-value)))



  (defun function-literal-to-form (function)
   (cond
     ((when (consp function)
        (case (car function)
          (function
           (destructuring-bind (function-sym function) function
             (declare (ignore function-sym))
             (typecase function
               (cons
                (when (eql (car function) 'lambda)
                  (destructuring-bind (lambda args &body body) function
                    (declare (ignore lambda))
                    (make-instance 'function-form-lambda :args args :body body))))
               (symbol (make-instance 'function-form-symbol :symbol function)))))
          (lambda
           (make-instance 'function-form-lambda :args (second function) :body (cddr function)))
          (curried
           (destructuring-bind (curried-sym curried &rest known-args) function
             (declare (ignore curried-sym))
             (make-instance 'function-form-partial :sub-function-form (function-literal-to-form curried) :new-arg-side :right :known-args known-args)))
          (rcurried
           (destructuring-bind (curried-sym curried &rest known-args) function
             (declare (ignore curried-sym))
             (make-instance 'function-form-partial :sub-function-form (function-literal-to-form curried) :new-arg-side :left :known-args known-args)))
          (constantly
           (destructuring-bind (constantly-sym value) function
             (declare (ignore constantly-sym))
             (make-instance 'function-form-constantly :value value)))
          (composed
           (make-instance 'function-form-composed :functions-reversed (nreverse (mapcar #'function-literal-to-form (cdr function))))))))
     (t (make-instance 'function-form-expression :expression function))))

  (defmethod get-call-form-precond-parameters ((function function-form) (parameter-count integer))
    (list (loop for i from 1 to parameter-count collect (gensym "get-call-form-precond-parameters"))))

  (defmethod get-call-form-precond-parameters ((function function-form-lambda) (parameter-count integer))
    (assert (eql parameter-count (length (get-args function))))
    (list (get-args function)))

  (defmethod get-call-form-precond-parameters ((function function-form-partial) (parameter-count integer))
    (let* ((known-arg-count (length (get-known-args function)))
           (sub-form-preconds (get-call-form-precond-parameters (get-sub-function-form function) (+ parameter-count known-arg-count))))
      (ecase (get-new-arg-side function)
        (:right ; normal curried
         (cons (last (first sub-form-preconds) parameter-count) sub-form-preconds))
        (:left ; rcurried
         (cons (subseq (first sub-form-preconds) 0 parameter-count) sub-form-preconds)))))

  (defmethod get-call-form-precond-parameters ((function function-form-composed) (parameter-count integer))
    (get-call-form-precond-parameters (first (get-functions-reversed function)) parameter-count))



  (defmethod get-call-form ((function function-form-symbol) (call-form-precond-parameters list) &optional (per-precond-parameter-actions nil))
    `(,(get-symbol function) ,@(if per-precond-parameter-actions (mapcar #'funcall per-precond-parameter-actions (first call-form-precond-parameters)) (first call-form-precond-parameters))))

  (defmethod get-call-form ((function function-form-expression) (call-form-precond-parameters list) &optional (per-precond-parameter-actions nil))
    (declare (ignore per-precond-parameter-actions))
    `(funcall ,(get-expression function) ,@(first call-form-precond-parameters)))

  (defmethod get-call-form ((function function-form-lambda) (call-form-precond-parameters list) &optional (per-precond-parameter-actions nil))
    (if per-precond-parameter-actions
        `(progn
           ,@(mapcar (lambda (precond-parameter action) `(setq ,precond-parameter ,(funcall action precond-parameter))) (first call-form-precond-parameters) per-precond-parameter-actions)
           (let nil ,@(get-body function)))
      `(let nil ,@(get-body function))))

  (defmethod get-call-form ((function function-form-partial) (call-form-precond-parameters list) &optional (per-precond-parameter-actions nil))
    (let* ((known-arg-count (length (get-known-args function)))
         ; (sub-form-preconds (get-call-form-precond-parameters (get-sub-function-form function) (+ (length (first call-form-precond-parameters)) known-arg-count)))
           (sub-form-preconds (cdr call-form-precond-parameters))
           )
      (ecase (get-new-arg-side function)
        (:right
         `(let ,(mapcar (lambda (precond known) `(,precond ,known)) (first sub-form-preconds) (get-known-args function))
            ,@(when per-precond-parameter-actions
                (loop for precond in (last (first sub-form-preconds) (length (first call-form-precond-parameters)))
                      for action in per-precond-parameter-actions
                      collect `(setq ,precond ,(funcall action precond))))
            ,(get-call-form (get-sub-function-form function) sub-form-preconds nil)))
        (:left
         `(let ,(mapcar (lambda (precond known) `(,precond ,known)) (last (first sub-form-preconds) known-arg-count) (get-known-args function))
            ,@(when per-precond-parameter-actions
                (loop for precond in (subseq (first sub-form-preconds) 0 (length (first call-form-precond-parameters)))
                      for action in per-precond-parameter-actions
                      collect `(setq ,precond ,(funcall action precond))))
            ,(get-call-form (get-sub-function-form function) sub-form-preconds nil))))))

  (defmethod get-call-form ((function function-form-composed) (call-form-precond-parameters list) &optional (per-precond-parameter-actions nil))
    (destructuring-bind (last . not-last-reversed)
        (get-functions-reversed function)
      (let ((result-sym (gensym "get-call-form result")))
        `(let ((,result-sym
                ,(get-call-form last call-form-precond-parameters per-precond-parameter-actions)))
           ,@(loop for function in not-last-reversed collect
                   `(setq ,result-sym
                          ,(let ((precond-parameters (get-call-form-precond-parameters function 1)))
                             (destructuring-bind (sole-result-sym) ; there must be only one.
                                 (first precond-parameters)
                               `(let ((,sole-result-sym ,result-sym))
                                  (declare (ignorable ,sole-result-sym))
                                  ,(get-call-form function precond-parameters nil))))))
           ,result-sym))))

  (defmethod get-call-form ((function function-form-constantly) (call-form-precond-parameters list) &optional (per-precond-parameter-actions nil))
    (declare (ignore per-precond-parameter-actions))
    (get-value function))

  (defun composed (&rest functions)
    (if functions
        (let (reversed last)
          (loop for remainder on functions do
                (if (cdr remainder)
                    (push (car remainder) reversed)
                  (setq last (car remainder))))
          (lambda (&rest args)
            (let ((result (apply last args)))
              (loop for elt in reversed do (setq result (funcall elt result)))
              result)))
      #'identity))



  (defun curried (function &rest largs)
    (assert (functionp function))
    (lambda (&rest rargs)
      (apply function (append largs rargs))))

  (defun rcurried (function &rest rargs)
    (assert (functionp function))
    (lambda (&rest largs)
      (apply function (append largs rargs))))


  )




#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct traversal-link
    get-link
    get-link-with-tail-override))
|#


(eval-when (:compile-toplevel :load-toplevel :execute)


(defmacro get-traversal-result (traversal-link)
  `(funcall (resolved ,traversal-link) :get-link))


  (defstruct unresolved call)
  (defmacro unresolved (&body body) `(make-unresolved :call (lambda () ,@body)))
  (defmacro resolved (&body body)
    (let ((temp-sym (gensym "resolved temp")))
      (if (cdr body)
          `(let ((,temp-sym ,@body)) (loop while (typep ,temp-sym 'unresolved) do (setq ,temp-sym (funcall (unresolved-call ,temp-sym)))) ,temp-sym)
        (let ((current body))
          (loop while (and (consp (car current)) (not (cdr current)) (or (eql (caar current) 'unresolved) (eql (caar current) 'resolved))) do (setq current (cdr (car current))))
          `(let ((,temp-sym ,@current)) (loop while (typep ,temp-sym 'unresolved) do (setq ,temp-sym (funcall (unresolved-call ,temp-sym)))) ,temp-sym)))))

  (defmacro tut (&body body)
    "Trampoline unless TCO can be assumed"
    `(unresolved ,@body))


  (defclass traversal-result () ((value :initarg :value :accessor get-value) (next :initarg :next :accessor get-next)))
  (defun traversal-result (value next)
    (make-instance 'traversal-result :value value :next next))


  (defmacro def-traversal-link (cases &key (sub-cases-sink nil))
    (let ((control-sym (gensym "def-traversal-link control"))
          (params-sym (gensym "def-traversal-link params"))
          (sub-cases-sink-sym (gensym "def-traversal-link sub-cases-sink")))
      `(let ((,sub-cases-sink-sym ,sub-cases-sink))
         (lambda (,control-sym &rest ,params-sym)
           (case ,control-sym
             ,@(mapcar
                (lambda (case)
                  (destructuring-bind (control args &body body)
                      case
                    `(,control
                      (destructuring-bind ,args
                          ,params-sym
                        ,@body))))
                cases)
             (:get-sub-cases-sink ,sub-cases-sink-sym)
             (t
              (if ,sub-cases-sink-sym
                  (apply ,sub-cases-sink-sym ,control-sym ,params-sym)
                (error (format nil "No sub-cases for ~A" ,control-sym)))))))))

  (defparameter **standard-terminating-end-call**
    (def-traversal-link
     ((:get-link () (traversal-result nil nil))
      (:get-link-with-tail-override (end-call) (unresolved (get-traversal-result end-call)))
      (:slam (&rest rest) (first rest) (second rest) (unresolved (funcall (second rest) (first rest)))))))

  )

(defun call-for-end ()
  (funcall **standard-terminating-end-call**))

#|
(defmacro assured-traversal-result (&body body)
  (let ((result-sym (gensym)))
    `(let ((,result-sym (resolved (progn ,@body))))
       (when (not (typep ,result-sym 'traversal-result)) (error (format nil "Not a Traversal-result:  ~S" ',body)))
       ,result-sym)))
|#

(defmacro! with-slam-sinks ((slam-params) &body body)
  `(let ((,g!slam-params ,slam-params))
     (destructuring-bind (,g!prior-count ,g!sink-count ,g!single-sym ,g!sequence-sym) ,g!slam-params
       (declare (ignore ,g!sink-count))
       (declare (ignorable ,g!prior-count))
       (labels ((slam-single (single) (funcall ,g!single-sym single))
                (slam-sequence (sequence) (funcall ,g!sequence-sym sequence)))
         (macrolet! ((end-call-thing (,g!end-call-sym count)
                       `(let ((,g!count ,count))
                          (unresolved (apply (resolved ,,g!end-call-sym) :slam (+ ,g!count ,',g!prior-count) (cdr ,',g!slam-params)))))
                     (slam-next (,g!next-sym count)
                       `(let ((,g!count ,count))
                          (unresolved (apply ,,g!next-sym :slam (+ ,g!count ,',g!prior-count) (cdr ,',g!slam-params)))))
                     (slam-for-continue (list sink-count)
                       `(apply #'slam-cont ,list ,',g!prior-count ,sink-count (cddr ,',g!slam-params))))
           ,@body)))))




(defmacro! standard-traversal-link-parametric ((build-func-sym (end-call-sym &rest build-params) &body body) (&rest build-actual-params))
  `(labels ((,build-func-sym (,end-call-sym ,@build-params)
              (lambda (,g!control-sym &rest ,g!params-sym)
                (case ,g!control-sym
                  (:slam
                   (do-slam ,g!params-sym ,body ,end-call-sym))
                  (t
                   (let ((,end-call-sym
                          (ecase ,g!control-sym
                            (:get-link ,end-call-sym)
                            (:get-link-with-tail-override
                             (call-with-new-end-call
                              ,end-call-sym
                              (destructuring-bind (,end-call-sym)
                                             ,g!params-sym
                                           ,end-call-sym))))))
                     ,@body))))))
     (,build-func-sym **standard-terminating-end-call** ,@build-actual-params)))


(defmacro standard-traversal-link-sequence-slam ((build-func-sym (end-call-sym &rest build-params) &body obody)
                                                 sequence
                                                 (&rest build-actual-params))
  `(macrolet ((do-slam (params-sym ibody end-call-sym)
                (declare (ignore ibody))
                `(with-slam-sinks (,params-sym)
                                  (let* ((seq ,',sequence)
                                         (length (length seq)))
                                    (slam-sequence seq)
                                    (end-call-thing ,end-call-sym length)))))
     (standard-traversal-link-parametric (,build-func-sym (,end-call-sym ,@build-params) ,@obody) (,@build-actual-params))))

#|
  (let ((control-sym (gensym))
        (params-sym (gensym)))
    `(labels ((,build-func-sym (,end-call-sym ,@build-params)
                (lambda (,control-sym &rest ,params-sym)
                  (case ,control-sym
                    (:slam
                     (with-slam-sinks (,params-sym)
                                      (slam-sequence ,sequence)
                                      (tut (apply (resolved ,end-call-sym) :slam ,params-sym))))
                    (t
                     (let ((,end-call-sym
                            (ecase ,control-sym
                              (:get-link ,end-call-sym)
                              (:get-link-with-tail-override
                               (destructuring-bind (,end-call-sym)
                                   ,params-sym
                                 ,end-call-sym)))))
                       ,@body))))))
       (,build-func-sym **standard-terminating-end-call** ,@build-actual-params))))
|#





(defmacro! standard-traversal-link ((build-func-sym (end-call-sym &rest build-params) &body obody) (&rest build-actual-params))
  `(macrolet! ((do-slam (params-sym ibody end-call-sym)
                 `(with-slam-sinks (,params-sym)
                    (let* ((,g!result-sym (resolved (progn ,@ibody)))
                           (,g!next-sym (get-next ,g!result-sym)))
                      (cond
                       (,g!next-sym
                        (slam-single (resolved (get-value ,g!result-sym)))
                        (slam-next ,g!next-sym 1))
                       (t
                        (end-call-thing ,end-call-sym 0)))))))
     (standard-traversal-link-parametric (,build-func-sym (,end-call-sym ,@build-params) ,@obody) (,@build-actual-params))))



#|
(defmacro standard-traversal-link ((build-func-sym (end-call-sym &rest build-params) &body body) (&rest build-actual-params))
  (let ((control-sym (gensym))
        (params-sym (gensym)))
  `(labels ((,build-func-sym (,end-call-sym ,@build-params)
              (lambda (,control-sym &rest ,params-sym)
                (case ,control-sym
                  (:slam
                   (with-slam-sinks (,params-sym)
                                    (let* ((,result-sym (resolved (progn ,@body)))
                                           (,next-sym (get-next ,result-sym)))
                                      (cond
                                       (,next-sym
                                        (slam-single (get-value ,result-sym))
                                        (tut (apply ,next-sym :slam ,params-sym)))
                                       (t
                                        (tut (apply (resolved ,end-call-sym) :slam ,params-sym)))))))
                  (t
                   (let ((,end-call-sym
                          (ecase ,control-sym
                            (:get-link ,end-call-sym)
                            (:get-link-with-tail-override
                             (destructuring-bind (,end-call-sym)
                                 ,params-sym
                               ,end-call-sym)))))
                     ,@body))))))
     (,build-func-sym **standard-terminating-end-call** ,@build-actual-params))))
|#

(defun slam-cont (lazy-list current-count sink-count sink-single sink-sequence)
  (funcall (get-call-for-first lazy-list) :slam current-count sink-count sink-single sink-sequence))

(defun slam (lazy-list sink-single sink-sequence)
  (let ((result nil))
    (resolved (slam-cont lazy-list 0 (lambda (count) (setq result count)) sink-single sink-sequence))
    result))


#|
(defmacro standard-traversal-link-with-sub ((build-func-sym (end-call-sym &rest build-params) &body body) sub-cases (&rest build-actual-params))
  `(labels ((,build-func-sym (,end-call-sym ,@build-params)
              (def-traversal-link
               ((:get-link () ,@body)
                (:get-link-with-tail-override (,end-call-sym) ,@body))
               :sub-cases-sink (def-traversal-link ,sub-cases))))
     (,build-func-sym **standard-terminating-end-call** ,@build-actual-params)))
|#


#|
Was experimental/to keep sub-cases but the situation for them went away
(defmacro sub-case-preserving-traversal-link ((build-func-sym (end-call-sym call-sym &rest build-params) &body body) (&rest build-actual-params))
  `(labels ((,build-func-sym (,end-call-sym ,call-sym ,@build-params)
              (def-traversal-link
               ((:get-link () ,@body)
                (:get-link-with-tail-override (,end-call-sym) ,@body))
               :sub-cases-sink `(funcall ,call-sym :sub-cases-sink))))
     (,build-func-sym **standard-terminating-end-call** ,@build-actual-params)))

|#



(defmacro fixed-traversal-link-from-result-form (traversal-result-form)
  (let ((end-call-sym (gensym "end call sym"))
        (next-sym (gensym "next sym"))
        (result-sym (gensym "result sym"))
        (v-sym (gensym "v sym"))
        (n-sym (gensym "n sym")))
    `(def-traversal-link
      ((:get-link () ,traversal-result-form)
       (:get-link-with-tail-override (,end-call-sym)
        (let* ((,result-sym (resolved ,traversal-result-form))
               (,next-sym (resolved (get-next ,result-sym))))
          (if ,next-sym
              (traversal-result
               (get-value ,result-sym)
               (unresolved
                (with-traversal-result
                 (,v-sym ,n-sym)
                 (funcall ,next-sym :get-link-with-tail-override ,end-call-sym)
                 (fixed-traversal-link ,v-sym ,n-sym))))
            (unresolved (get-traversal-result ,end-call-sym)))))))))



(defmacro with-traversal-result ((val-sym next-sym) form &body body)
  (let ((sym (gensym "with-traversal-result sym")))
    `(with-slots ((,val-sym value) (,next-sym next))
;         (let ((,sym ,form)) (assert (typep ,sym 'traversal-result)) ,sym)
;         (confirmed-traversal-result ,form)
         (let ((,sym (resolved ,form)))
           (when (not (typep ,sym 'traversal-result))
             (print (list "bad type for traversal type " ,sym ',body))
             )
           ,sym)
       ,@body)))

(defmacro fixed-traversal-link (value next)
  (let ((end-call-sym (gensym "fixed-traversal-link end-call"))
        (next-sym (gensym "fixed-traversal-link next-sym"))
        (slam-params-sym (gensym "fixed-traversal-link slam-params")))
    `(def-traversal-link
      ((:get-link () (traversal-result ,value ,next))
       (:get-link-with-tail-override (,end-call-sym)
        (let ((,next-sym (resolved ,next)))
          (if ,next-sym

              ; (traversal-result ,value (unresolved (funcall ,next-sym :get-link-with-tail-override ,end-call-sym)))
              (tail-override-for-fixed-traversal-link ,value ,next-sym ,end-call-sym)

            (unresolved (get-traversal-result ,end-call-sym)))))
       (:slam (&rest ,slam-params-sym)
        (with-slam-sinks (,slam-params-sym)
                         (let ((,next-sym (resolved ,next)))
                           (when ,next-sym
                             (slam-single (resolved ,value))
                           (unresolved (apply ,next-sym :slam ,slam-params-sym))))))))))

(defun tail-override-for-fixed-traversal-link (value next end-call)
  (traversal-result value (unresolved (fixed-traversal-link-from-result-form (funcall next :get-link-with-tail-override end-call)))))



(defmacro get-traversal-result-new-end-call (traversal-link new-end-call)
  `(funcall (resolved ,traversal-link) :get-link-with-tail-override ,new-end-call))

(defmacro! deferred-traversal-link-from-call-maker (call-maker-form)
  `(def-traversal-link
    ((:get-link () (get-traversal-result ,call-maker-form))
     (:get-link-with-tail-override (,g!end-call) (get-traversal-result-new-end-call ,call-maker-form ,g!end-call))
     (:slam (&rest ,g!slam-params)
      (with-slam-sinks (,g!slam-params)
                       (unresolved (apply ,call-maker-form :slam ,g!slam-params)))))))

#|
                       (with-traversal-result
                        (,g!value ,g!next)
                        (get-traversal-result ,call-maker-form)
                        (let ((,g!next (resolved ,g!next)))
                          (when ,g!next
                            (slam-single (resolved ,g!value))
                            (unresolved (apply ,g!next :slam ,g!slam-params)))))
|#


(defun confirmed-traversal-result (val)
  (assert (typep val 'traversal-result))
  val)


(defun call-with-new-end-call (call new-end-call)
  (lambda (control &rest params)
    (case control
      (:get-link (unresolved (funcall call :get-link-with-tail-override new-end-call)))
      (:get-link-with-tail-override
       (let ((un-overridden-link (funcall call :get-link-with-tail-override new-end-call))
             (override (car params)))
         (with-traversal-result (v nn) un-overridden-link ; v and n from the 
           (let ((nn (resolved nn)))
             (if nn
                 (traversal-result v (call-with-new-end-call nn override))
               ; do end here - as we've already let the override on via the "next"
               (apply **standard-terminating-end-call** control params)))))))))



(defclass lazy-list () ((call-for-first :initarg :call-for-first :accessor get-call-for-first)))

#|
(defmethod get-call-for-first ((list lazy-list) (call-for-end function))
  (resolved (funcall (get-call-for-first-maker list) call-for-end)))
|#

(defclass lazy-list-under-cdrs (lazy-list)
  ((underlying-call-for-first :initarg :underlying-call-for-first :accessor get-underlying-call-for-first)
   (cdr-count :initarg :cdr-count :accessor get-cdr-count)))

(defclass lazy-list-with-some-persistence (lazy-list) ())

(defclass lazy-list-with-persistence (lazy-list-with-some-persistence) ())

(defclass lazy-list-read-point-based (lazy-list-with-some-persistence) ((read-point :initarg :read-point :accessor get-read-point)))

(defclass lazy-list-known-empty (lazy-list-with-persistence) ())

(defclass lazy-list-list-based (lazy-list-with-persistence)
  ((list-head :initarg :list-head :accessor get-list-head)))

(defclass lazy-list-pair-based (lazy-list-with-persistence)
  ((cons :initarg :cons :accessor get-cons)))


(defun make-instance-2 (type &rest params)
  (when (eql type 'lazy-list-read-point-based)
      (when (not (getf params :read-point))
        (error "No read-point")))
  (apply #'make-instance type params))


(defmacro lazy-list-from-call (call)
  (let ((call-sym (gensym "lazy-list-from-call sym")))
    `(let ((,call-sym ,call))
       ; (assert (typep ,call-sym 'traversal-link))
       (make-instance-2 'lazy-list :call-for-first ,call-sym))))


(defmacro lazy-list-from-traversal-link (traversal-link)
  (let ((sym (gensym "lazy-list-from-traversal-link sym")))
    `(let ((,sym ,traversal-link))
       ; (assert (typep ,sym 'traversal-link))
       (make-instance-2 'lazy-list :call-for-first ,sym))))


(defmacro deferred-lazy-list (list-definition)
  `(lazy-list-from-call
    (deferred-traversal-link-from-call-maker
     (get-call-for-first (to-lazy-list ,list-definition)))))



(defparameter **in-lazy-mode** nil)


(defun in-lazy-mode ()
  (declare (special **in-lazy-mode**))
  **in-lazy-mode**)
(define-compiler-macro in-lazy-mode () `**in-lazy-mode**)


(defmacro if-lazy-eager (if-lazy if-strict)
  `(if (in-lazy-mode)
       ,if-lazy
     ,if-strict))

(defmacro lazy (&body body)
  "Enters a \"lazy\" context - calls to functions such as tail/ defer traversal.
This context uses a special variable, and extends into sub-calls until overridden."
  `(let ((**in-lazy-mode** t))
     ,@body))

(defmacro eager (&body body)
  "Enters an \"eager\" context - calls to functions such as tail/ do traversal before returning.
This context uses a special variable, and extends into sub-calls until overridden."
  `(let ((**in-lazy-mode** nil))
     ,@body))



(defparameter **respecting-thread-safety** nil)

(defstruct read-point
  rp-value
  rp-next
  rp-lock)

(defstruct read-point-value-resolver
  run)

(defstruct read-point-next-resolver
  run
  get-call)

(defmacro respecting-lock-if-present ((lock) &body body)
  `(if ,lock
      (bordeaux-threads::with-lock-held (,lock) ,@body)
     (progn ,@body)))

(defmacro respecting-read-point-lock ((read-point) &body body)
  (let ((lock-sym (gensym)))
    `(let ((,lock-sym (read-point-rp-lock ,read-point)))
       (respecting-lock-if-present (,lock-sym) ,@body))))

(defmacro assure-readpoint-value-resolved (read-point)
  (let ((sym (gensym)))
    `(let ((,sym (read-point-rp-value ,read-point)))
       (when (typep ,sym 'read-point-value-resolver)
         (funcall (read-point-value-resolver-run ,sym))))))

(defmacro assure-readpoint-next-resolved (read-point)
  (let ((sym (gensym)))
    `(let ((,sym (read-point-rp-next ,read-point)))
       (when (typep ,sym 'read-point-next-resolver)
         (funcall (read-point-next-resolver-run ,sym))))))

(defun read-point-value (read-point)
  (assert (typep read-point 'read-point))
  (respecting-read-point-lock
   (read-point)
   (assure-readpoint-value-resolved read-point)
   (read-point-rp-value read-point)))

(defun read-point-at-end (read-point)
  (assert (typep read-point 'read-point))
  (respecting-read-point-lock
   (read-point)
   (assure-readpoint-next-resolved read-point)
   (not (read-point-rp-next read-point))))


(defun read-point-advanced (read-point)
  (assert (typep read-point 'read-point))
  (respecting-read-point-lock
   (read-point)
   (assure-readpoint-next-resolved read-point)
   (read-point-rp-next read-point)))


(defun read-point-from-call (call &optional (lock (when **respecting-thread-safety** (bordeaux-threads:make-lock))))
  (let ((read-point nil))
    (setq read-point
          (make-read-point
           :rp-lock lock
           :rp-value (make-read-point-value-resolver
                      :run
                      (lambda ()
                        (respecting-lock-if-present
                         (lock)
                         (with-traversal-result
                          (value next)
                          (resolved (get-traversal-result call))
                          (setf (read-point-rp-value read-point) (resolved value))
                          (setf (read-point-rp-next read-point)
                                (make-read-point-next-resolver
                                 :run
                                 (lambda ()
                                   (respecting-lock-if-present
                                    (lock)
                                    (setf (read-point-rp-next read-point)
                                          (let ((next (resolved next)))
                                            (when next (read-point-from-call next (when lock (bordeaux-threads:make-lock))))))))
                                 :get-call
                                 (lambda ()
                                   (respecting-lock-if-present
                                    (lock)
                                    (let ((next (resolved next)))
                                      (setf (read-point-rp-next read-point)
                                            (when next (read-point-from-call next (when lock (bordeaux-threads:make-lock)))))
                                      next)))))))))
           :rp-next (make-read-point-next-resolver
                     :run
                     (lambda ()

                       (respecting-lock-if-present
                        (lock)
                        (with-traversal-result
                         (value next)
                         (resolved (get-traversal-result call))
                         (let ((next (resolved next)))
                           (setf (read-point-rp-next read-point) (when next (read-point-from-call next (when lock (bordeaux-threads:make-lock)))))
                           (setf (read-point-rp-value read-point)
                                 (when next
                                   (if (typep value 'unresolved)
                                       (make-read-point-value-resolver
                                        :run
                                        (lambda ()
                                          (respecting-lock-if-present
                                           (lock)
                                           (setf (read-point-rp-value read-point) (resolved value)))))
                                     value)))))))
                     :get-call
                     (lambda ()
                       (respecting-lock-if-present
                        (lock)
                        (with-traversal-result
                         (value next)
                         (resolved (get-traversal-result call))
                         (let ((next (resolved next)))
                           (setf (read-point-rp-next read-point) (when next (read-point-from-call next (when lock (bordeaux-threads:make-lock)))))
                           (setf (read-point-rp-value read-point)
                                 (when next
                                   (if (typep value 'unresolved)
                                       (make-read-point-value-resolver
                                        :run
                                        (lambda ()
                                          (respecting-lock-if-present
                                           (lock)
                                           (setf (read-point-rp-value read-point) (resolved value)))))
                                     value)))
                           next)))))))
    read-point))

(defun read-point-built (list)
  (etypecase list
    (lazy-list-read-point-based (get-read-point list))
    (lazy-list
     (read-point-from-call (get-call-for-first list)))))

(defun call-for-read-point-taken-to-end (read-point)
  (assert (typep read-point 'read-point))
  (standard-traversal-link
   (build (end-call read-point)
          (if (read-point-at-end read-point)
              (unresolved (get-traversal-result end-call))
            (let ((advanced (read-point-advanced read-point))) ; calc here to (potentially) advance value
              (let ((value (read-point-rp-value read-point)))
                (traversal-result
                 (if (typep value 'read-point-value-resolver)
                     (unresolved (read-point-value read-point))
                   value)
                 (build end-call advanced))))))
   (read-point)))

(defun call-to-detach-from-read-point (read-point)
  (assert (typep read-point 'read-point))
  (standard-traversal-link
   (build (end-call read-point)
          (let ((result
                 (respecting-read-point-lock
                  (read-point)
                  (let ((rp-next (read-point-rp-next read-point)))
                    (cond
                     ((null rp-next) (get-traversal-result end-call))
                     (t
                      (etypecase rp-next
                        (read-point (traversal-result (unresolved (read-point-value read-point)) (build end-call rp-next)))
                        (read-point-next-resolver
                         (unresolved
                           (let ((nexts-call (funcall (read-point-next-resolver-get-call rp-next))))
                             (if nexts-call
                                 (traversal-result
                                  (unresolved (read-point-value read-point))
                                  (with-traversal-result
                                   (val next)
                                   (get-traversal-result-new-end-call nexts-call end-call)
                                   (let ((next (resolved next)))
                                     (if next
                                         (fixed-traversal-link val next)
                                       end-call))))
                               (get-traversal-result end-call))))))))))))
            result))
   (read-point)))


(defun lazy-list-from-read-point (read-point)
  (make-instance-2 'lazy-list-read-point-based :call-for-first (call-for-read-point-taken-to-end read-point) :read-point read-point))


; Runs in block nil (return will break out)
(defmacro loop-over/ (symbol lazy-list &body body)
  (let ((current-sym (gensym)) (value-sym (gensym)) (next-sym (gensym)) (top-sym (gensym)) (list-sym (gensym)))
    "Most trivial loop construct - loops a symbol across lazy-list running body.  Runs in block NIL."
    `(let ((,list-sym ,lazy-list))
       (typecase ,list-sym
         (lazy-list-known-empty nil)
         (lazy-list-list-based (loop for ,symbol in (get-list-head ,list-sym) do ,@body))
         (t
          (block nil
            (let ((,current-sym (get-call-for-first ,lazy-list)))
              (tagbody
               ,top-sym
               (with-traversal-result (,value-sym ,next-sym)
                   (resolved (get-traversal-result ,current-sym))
                 (let ((,next-sym (resolved ,next-sym))
                       (,value-sym (resolved ,value-sym)))
                   (when ,next-sym
                     ,(if (symbolp symbol)
                          `(let ((,symbol (resolved ,value-sym)))
                            ,@body
                            (setf ,current-sym ,next-sym)
                            (go ,top-sym))
                        `(destructuring-bind ,symbol (resolved ,value-sym)
                           ,@body
                           (setf ,current-sym ,next-sym)
                           (go ,top-sym))))))))))))))

(defmethod print-object ((lazy-list lazy-list) stream)
;  (format stream "(LIST/ #|Known Type: ~S|#" (type-of lazy-list))
  (format stream "(LIST/")
  (loop-over/ elt lazy-list (format stream " ~S" elt))
  (format stream ")"))

(defmacro list-to-lazy-list-call (origin &key (terminator-generator (lambda (rest-sym) rest-sym)) (value-generator (lambda (rest-sym) `(car ,rest-sym))))
  (let* ((build-sym (gensym))
         (rest-sym (gensym))
         (end-call-sym (gensym)))
    `(standard-traversal-link
      (,build-sym (,end-call-sym ,rest-sym)
                  (if ,(funcall (eval terminator-generator) rest-sym)
                      (traversal-result ,(funcall (eval value-generator) rest-sym) (,build-sym ,end-call-sym (cdr ,rest-sym)))
                    (unresolved (get-traversal-result ,end-call-sym))))
      (,origin))))



(defun to-lazy-list (list)
  (etypecase list
    (lazy-list list)
    (list (if list
              (make-instance-2 'lazy-list-list-based :call-for-first (list-to-lazy-list-call list) :list-head list)
            (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)))
    (vector
     (let ((length (length list)))
       (make-instance-2 'lazy-list-with-persistence
                        :call-for-first
                        (standard-traversal-link-sequence-slam
                         (build (end-call current-index)
                                (if (eql current-index length)
                                    (unresolved (get-traversal-result end-call))
                                  (traversal-result
                                   (aref list current-index)
                                   (build end-call (1+ current-index)))))
                         list
                         (0)))))
    (array
     (let* ((dimensions (array-dimensions list)))
       (make-instance-2 'lazy-list-with-persistence
                        :call-for-first
                        (standard-traversal-link
                         (build (end-call current-dimension-head prior-coords current-coord)
                                (if (eq (car current-dimension-head) current-coord) ; test for end of axis.
                                    (unresolved (get-traversal-result end-call))
                                  (traversal-result
                                   (if (cdr current-dimension-head) ; Another axis.
                                       (lazy-list-from-call (build **standard-terminating-end-call** (cdr current-dimension-head) (append prior-coords (list current-coord)) 0))
                                     (apply #'aref list (append prior-coords (list current-coord))))
                                   (build end-call current-dimension-head prior-coords (1+ current-coord)))))
                         (dimensions nil 0)))))))


(defun memoized/ (list)
  "Caches list on first traversal (unless it's determined to already be implemented in terms of persistence)."
  (etypecase list
    (sequence (to-lazy-list list))
    (lazy-list-with-some-persistence list)
    (lazy-list (lazy-list-from-read-point (read-point-built list)))))


(defun lazy-listp (potential)
  (typep potential 'lazy-list))

(defun listp/ (potential)
  (or (listp potential) (typep potential 'lazy-list)))

(defun list/ (&rest rest)
  "Lazy equivalent of CL's list function - returning a lazy-list (although one that has the parameter list at its core)."
  (if rest
      (make-instance-2 'lazy-list-list-based :call-for-first (list-to-lazy-list-call rest) :list-head rest)
    (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)))

(defun equal/ (a b)
  (let ((a-is-list (listp/ a))
        (b-is-list (listp/ b)))
    (cond
     ((and a-is-list b-is-list)
      (let ((a (read-point-built (to-lazy-list a)))
            (b (read-point-built (to-lazy-list b))))
        (unless
            (loop while (not (or (read-point-at-end a) (read-point-at-end b))) do
                  (unless (equal/ (read-point-value a) (read-point-value b))
                    (return-from nil t))
                  (setq a (read-point-advanced a))
                  (setq b (read-point-advanced b)))
          (and (read-point-at-end a) (read-point-at-end b)))))
     ((or a-is-list b-is-list) nil)
     (t (equal a b)))))



(defun iterate/ (from-previous element &optional (end-before-func (constantly nil)))
  "Haskell's iterate function - returns element, then (funcall from-previous element), etc. against result"
  (assert (functionp from-previous))
  (lazy-list-from-call
   (standard-traversal-link
    (get-val (end-call elt)
             (if (funcall end-before-func elt)
                 (unresolved (get-traversal-result end-call))
               (traversal-result
                elt
                ; EXPERIMENTAL - putting in a deferral on the get-val recurse, so that caller can get an always-NON-NULL NEXT (always true for iterate)
                ; but not run the next function iteration until he actually tries to use it
                          ; THIS is dependent on implementation of standard-traversal-link, making it not-black-box
                (lambda (x) (funcall (get-val end-call (funcall from-previous elt)) x)))))
    (element))))

;;; Wish I remembered why I made a compiler-macro for this. . .
(define-compiler-macro iterate/ (from-previous element &optional (end-before-func '(constantly nil)))
  (let ((get-val-sym (gensym))
        (end-call-sym (gensym))
        (end-before-precond-parameters (get-call-form-precond-parameters (function-literal-to-form end-before-func) 1))
        (from-previous-precond-parameters (get-call-form-precond-parameters (function-literal-to-form from-previous) 1))
        (elt-sym (gensym))
        (x-sym (gensym))
        )
    `(lazy-list-from-call
      (standard-traversal-link
       (,get-val-sym (,end-call-sym ,elt-sym)
                     (if (let ((,(caar end-before-precond-parameters) ,elt-sym)) (declare (ignorable ,(caar end-before-precond-parameters))) ,(get-call-form (function-literal-to-form end-before-func) end-before-precond-parameters nil))
                         (unresolved (get-traversal-result ,end-call-sym))
                       (traversal-result
                        ,elt-sym
                ; EXPERIMENTAL - putting in a deferral on the get-val recurse, so that caller can get an always-NON-NULL NEXT (always true for iterate)
                ; but not run the next function iteration until he actually tries to use it
                          ; THIS is dependent on implementation of standard-traversal-link, making it not-black-box
                          (lambda (,x-sym)
                            (funcall (,get-val-sym ,end-call-sym (let ((,(caar from-previous-precond-parameters) ,elt-sym)) (declare (ignorable ,(caar from-previous-precond-parameters))) ,(get-call-form (function-literal-to-form from-previous) from-previous-precond-parameters nil))) ,x-sym)))))
       (,element)))))



; (pprint (funcall (compiler-macro-function 'iterate/) '(iterate/ #'1+ 1 (lambda (x) (> x 20))) nil))
; (defun test () (iterate/ #'1+ 1 (lambda (x) (> x 20))))




(defun iteratex/ (input-to-contribution initial-input)
  (assert (functionp input-to-contribution))
  (labels ((build (input)
             (lambda ()
               (block nil
                 (let ((current-input input))
                   (tagbody
                    top
                    (let ((contribution (funcall input-to-contribution current-input)))
                      (destructuring-bind (primary-result
                                           &key
                                           (emissions #| (list current-input) |# nil emissions-supplied-p)
                                           (emission nil emission-supplied-p)
                                           (exit-before nil)
                                           (exit-after nil)
                                           (next-input primary-result))
                          contribution
                        (assert (not (and emissions-supplied-p emission-supplied-p))) ; can *NOT* supply both
                        (cond
                         (exit-before (return (call-for-end)))
                         ((and emissions-supplied-p (null/ emissions)) (setq current-input next-input) (go top))
                         (t
                          (if emission-supplied-p
                              (return
                               (values emission
                                       (if exit-after
                                           #'call-for-end
                                         (build next-input))))
                            (labels ((build-for-read-point (read-point)
                                       (values (read-point-value read-point)
                                               (let ((advanced (read-point-advanced read-point)))
                                                 (if (read-point-at-end advanced)
                                                     (if exit-after
                                                         #'call-for-end
                                                       (build next-input))
                                                   (lambda () (build-for-read-point advanced)))))))
                              (return (build-for-read-point (read-point-built (to-lazy-list emissions))))))))))))))))
    (lazy-list-from-call (build initial-input))))

(defun to-list (list)
"Returns the proper list corresponding to the passed-in list designator - attempts to minimize work involved if list is a CL sequence
or a lazy-list based upon a fixed container"
  (etypecase list
    (lazy-list-list-based (get-list-head list))
    (lazy-list-pair-based (destructuring-bind (first . second) (get-cons list) (list first second)))
    (lazy-list
     (let ((result nil))
       (loop-over/ elt list (push elt result))
       (nreverse result)))
    (list list)
    (sequence (map 'list #'identity list))))

(defun to-array (list &rest array-params)
"Returns the array corresponding to the passed-in list designator - attempts minimize work involved if list is a CL sequence or lazy-list
based upon a fixed container"
  (etypecase list
    (sequence (apply #'make-array (length list) :initial-contents list array-params))
    (t
     (let ((result nil)
           (count 0))
       (loop-over/ elt (to-lazy-list list) (progn (push elt result) (incf count)))
       (apply #'make-array count :initial-contents (nreverse result) array-params)))))


(defun string-from-chars/ (chars-list)
"Returns a string from the supplied chars list"
  (typecase chars-list
    (string chars-list)
    (array (let ((result (make-string (length chars-list))))
             (loop for i from 0
                   for elt across chars-list do
                   (setf (aref result i) elt))
             result))
    (list
     (with-output-to-string (str)
       (loop for elt in chars-list do
             (write-char elt str))))
    (t
     (with-output-to-string (str)
       (loop-over/ elt (to-lazy-list chars-list)
                   (write-char elt str)))
#|
     (let ((result nil)
           (count 0))
       (loop-over/ elt (to-lazy-list chars-list) (progn (push elt result) (incf count)))
       (let ((string (make-string count)))
         (loop for index from (1- count) downto 0
               for elt in result do
               (setf (aref string index) elt))
         string))
|#
     )))


(defun to-string-irresolute (chars-list)
  (let ((char-accumulator-r nil)
        (char-count-accumulator 0))
    (unresolved
      (let ((current-call (get-call-for-first (to-lazy-list chars-list)))
            (unresolved nil))
        (labels ((run ()
                   (with-traversal-result (value next)
                       (get-traversal-result (resolved current-call))
                     (unresolved
                       (let ((next (resolved next)))
                         (unresolved
                           (if next
                               (unresolved
                                 (let ((value (resolved value)))
                                   (unresolved
                                     (progn
                                       (push value char-accumulator-r)
                                       (incf char-count-accumulator)
                                       (setq current-call next)
                                       unresolved))))
                             (let ((string (make-string char-count-accumulator)))
                               (loop for i from (1- char-count-accumulator) downto 0 do
                                     (setf (aref string i) (pop char-accumulator-r)))
                               string))))))))
          (setq unresolved (make-unresolved :call #'run)))))))



#|
  (let ((char-accumulator-r nil)
        (char-count-accumulator 0)
        (current-call (resolved (get-call-for-first (to-lazy-list chars-list))))
        (unresolved nil))
    (labels ((run ()
               (with-traversal-result (value next)
                   (get-traversal-result current-call)
                 (let ((next (resolved next)))
                   (if next
                       (progn
                         (push (resolved value) char-accumulator-r)
                         (incf char-count-accumulator)
                         (setq current-call next)
                         unresolved)
                     (let ((string (make-string char-count-accumulator)))
                       (loop for i from (1- char-count-accumulator) downto 0 do
                             (setf (aref string i) (pop char-accumulator-r)))
                       string))))))
      (setq unresolved (make-unresolved :call #'run))))
|#



(defmethod to-string ((chars-list sequence))
"Returns a string from the supplied chars list"
  (string-from-chars/ chars-list))


(defmethod to-string ((chars-list lazy-list))
"Returns a string from the supplied chars list"
  (string-from-chars/ chars-list))



(defun length/ (list)
  "Returns the length of the supplied list, evaluating the list to the end if lazy to measure its length."
  (typecase list
    (sequence (length list))
    (lazy-list-list-based (length (get-list-head list)))
    (lazy-list-pair-based 2)
    (lazy-list-known-empty 0)
    (t
     (let ((current (read-point-built (to-lazy-list list)))
           (len 0))
       (loop while (not (read-point-at-end current)) do
             (setq current (read-point-advanced current))
             (incf len))
       len))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun full-cdr-based-form-resolution (form)
    ; Attempts to preserve evaluation order of the full expression.
    (let ((form-under-cdrs form)
          (cdr-items-r nil))
      (loop while (and (consp form-under-cdrs) (let ((control (car form-under-cdrs))) (or (eql control 'cdr/) (eql control 'nthcdr/)))) do
            (push (butlast form-under-cdrs) cdr-items-r)
            (setq form-under-cdrs (car (last form-under-cdrs))))
      (let ((sum-of-known-numerics
             (loop for elt in cdr-items-r sum
                   (if (eql (car elt) 'cdr/)
                       1
                     (destructuring-bind (to-drop)
                         (cdr elt)
                       (if (integerp to-drop)
                           to-drop
                         0)))))
            (unevaluated-to-drops
             (loop for elt in cdr-items-r append
                   (when (eql (car elt) 'nthcdr/)
                     (destructuring-bind (to-drop)
                         (cdr elt)
                       (unless (integerp to-drop)
                         (list to-drop)))))))
        (cond
         ((and (zerop sum-of-known-numerics) (null unevaluated-to-drops)) form-under-cdrs)
         ((and (eql sum-of-known-numerics 1) (null unevaluated-to-drops)) `(cdr/-implementation ,form-under-cdrs))
         ((and (null unevaluated-to-drops) `(nthcdr/-implementation ,sum-of-known-numerics ,form-under-cdrs)))
         (t
          (if (and cdr-items-r (eql (caar cdr-items-r) 'nthcdr/) (not (integerp (cadar cdr-items-r))))
              (let ((inner-most-to-drop-sym (gensym))
                    (sub-list-sym (gensym)))
                `(let ((,inner-most-to-drop-sym ,(car unevaluated-to-drops))
                       (,sub-list-sym ,form-under-cdrs))
                   (nthcdr/-implementation (+ ,@(when (plusp sum-of-known-numerics) (list sum-of-known-numerics)) ,inner-most-to-drop-sym ,@(cdr unevaluated-to-drops)) ,sub-list-sym)))
            (let ((sub-list-sym (gensym)))
              `(let ((,sub-list-sym ,form-under-cdrs))
                 (nthcdr/-implementation (+ ,@(when (plusp sum-of-known-numerics) (list sum-of-known-numerics)) ,@unevaluated-to-drops) ,sub-list-sym)))))))))

)


(defun cdr/-implementation (list)
  "Equivalent of Haskell's tail function or CL's CDR - traverses in eager context, defers in lazy."
  (typecase list
    (lazy-list-known-empty list)
    (list (to-lazy-list (cdr list)))
    (lazy-list-list-based (to-lazy-list (cdr (get-list-head list))))
    (lazy-list
     (if-lazy-eager
      (typecase list
        (lazy-list-under-cdrs
         (let ((new-cdr-count (1+ (get-cdr-count list)))
               (underlying-call-for-first (get-underlying-call-for-first list)))
           (assert (> new-cdr-count 0))
           (make-instance-2 'lazy-list-under-cdrs
                            :call-for-first
                            (standard-traversal-link
                             (build (end-call)
                                    (let ((current (resolved (with-traversal-result (value next) (resolved (get-traversal-result-new-end-call underlying-call-for-first end-call)) (declare (ignore value)) next))))
                                      (loop for i from 2 to new-cdr-count
                                            while current
                                            do
                                            (when current (with-traversal-result (value next) (resolved (get-traversal-result current)) (declare (ignore value)) (setq current (resolved next)))))
                                      (if current
                                          (get-traversal-result current)
                                        (traversal-result nil nil))))
                             ())
                            :cdr-count new-cdr-count
                            :underlying-call-for-first underlying-call-for-first)))
        (t
         (let ((call-for-first (get-call-for-first list)))
           (make-instance-2 'lazy-list-under-cdrs
                            :call-for-first
                            (standard-traversal-link
                             (build (end-call)
                                    (with-traversal-result
                                        (value next)
                                        (get-traversal-result-new-end-call call-for-first end-call)
                                      (declare (ignore value))
                                      (unresolved
                                       (get-traversal-result next))))
                             ())
                            :cdr-count 1
                            :underlying-call-for-first call-for-first))))
      (typecase list
        (lazy-list-read-point-based
         (let ((read-point (get-read-point list)))
           (if (read-point-at-end read-point)
               (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)
             (let ((advanced (read-point-advanced read-point)))
               (make-instance-2 'lazy-list-read-point-based :call-for-first (call-for-read-point-taken-to-end advanced) :read-point advanced)))))
        (t
         (make-instance-2
          (case (type-of list) ; tail keeps persistence
            ((lazy-list-with-persistence lazy-list-with-some-persistence) (type-of list))
            (t 'lazy-list))
          :call-for-first
          (with-traversal-result
              (value next)
              (get-traversal-result (get-call-for-first list))
            (declare (ignore value))
            (let ((next (resolved next)))
              (cond (next)  ; Man not having this check was HARD to track down. . . .
                    (t **standard-terminating-end-call**)))))))))
    (t (cdr/ (to-lazy-list list)))))
(defun cdr/ (list) (cdr/-implementation list))
(define-compiler-macro cdr/ (&whole form list)
  (declare (ignore list))
  (full-cdr-based-form-resolution form))



(defun tail/ (list)
  "Equivalent of Haskell's tail function (or cdr/ ) - traverses in eager context, defers in lazy."
  (cdr/ list))
(define-compiler-macro tail/ (list)
  `(cdr/ ,list))


(defun car/ (list)
  "Equivalent of CL's car/first or Haskell's head - returns first value in list, or nil if list is empty"
  (typecase list
    (list (car list))
    (lazy-list-list-based (car (get-list-head list)))
    (lazy-list
     (with-traversal-result (value next) (get-traversal-result (get-call-for-first list)) (declare (ignore next)) (resolved value)))
    (t (with-traversal-result (value next) (get-traversal-result (get-call-for-first (to-lazy-list list))) (declare (ignore next)) (resolved value)))))

(defun head/ (list)
  "Equivalent of CL's car/first or Haskell's head - returns first value in list, or nil if list is empty"
  (car/ list))
(define-compiler-macro head/ (list) `(car/ ,list))

(defun first/ (list)
  "Equivalent of CL's car/first or Haskell's head - returns first value in list, or nil if list is empty"
  (car/ list))
(define-compiler-macro first/ (list) `(car/ ,list))

(defmacro build-car-things ()
  `(progn
     ,@(loop for len from 2 to 5 collect
             `(progn
                ,@(labels ((build-it (bits-left val)
                             (if (zerop bits-left)
                                 (list nil #'identity)
                               (destructuring-bind (char-list func)
                                   (build-it (1- bits-left) (floor (/ val 2)))
                                 (if (evenp val)
                                     (list (cons #\a char-list) (lambda (elt) `(car/ ,(funcall func elt))))
                                   (list (cons #\d char-list) (lambda (elt) `(cdr/ ,(funcall func elt)))))))))
                    (loop for combo from 0 to (1- (expt 2 len)) collect
                          (destructuring-bind (char-list func)
                              (build-it len combo)
                            ; (format t ":~A" (concatenate 'string "c" (map 'string #'identity char-list) "r/"))
                            ;(terpri)
                            ;(force-output)
                            (labels ((make-core-string (char-list)
                                       (concatenate 'string "c" (map 'string #'identity char-list) "r"))
                                     (th (n) (case n (1 "1st") (2 "2nd") (3 "3rd") (t (format nil "~Ath" n)))))
                              `(progn
                                 (defun ,(read-from-string (concatenate 'string (make-core-string char-list) "/"))
                                        (list-designator)
                                   ,(let ((a-count (length (remove-if-not (lambda (elt) (eql elt #\a)) char-list)))
                                          (d-count (length (remove-if-not (lambda (elt) (eql elt #\d)) char-list))))
                                      (cond
                                       ((zerop a-count)
                                        (format nil "Returns list-designator's list with ~A item skipped, as a lazy-list" d-count))
                                       ((= 1 a-count)
                                        (format nil "Returns ~A element in list-designator" (th (1+ d-count))))
                                       (t
                                        (let ((trailing-d-count (length (loop for char in (reverse char-list) while (eql #\d char) collect char))))
                                          (case trailing-d-count
                                            (0 (format nil "~A/ of first element in list-designator" (make-core-string (butlast char-list))))
                                            (t
                                             (format nil "~A/ of ~A element in list-designator" (make-core-string (butlast char-list (1+ trailing-d-count))) (th trailing-d-count))))))))
                                   ,(funcall func 'list-designator))
                                 ,(let ((list-sym (gensym)) (whole-sym (gensym)))
                                    `(define-compiler-macro ,(read-from-string (concatenate 'string (make-core-string char-list) "/"))
                                         (&whole ,whole-sym ,list-sym)
									   (declare (ignore ,list-sym))
                                       ,whole-sym ; couldn't think of a way to make these compiler-macros work out
                                       ; ,(funcall func list-sym)
                                       )))))))))))
(build-car-things)



; returns head and tail as successive values
(defun head-tail/ (list)
  "Returns head and tail of list as successive values, and whether or not head is a valid value as the third result."
  (etypecase list
    (list (values (car list) (to-lazy-list (cdr list)) (consp list)))
    (lazy-list-list-based (destructuring-bind (head . tail) (get-list-head list) (values head (to-lazy-list tail) (consp (get-list-head list)))))
    (t (with-traversal-result (head tail-call) (resolved (get-traversal-result (get-call-for-first (to-lazy-list list))))
                              (let ((tail-call (resolved tail-call)))
                                (if tail-call
                                    (values (resolved head) (lazy-list-from-call tail-call) t)
                                  (values)))))))



(defun nthcdr/-implementation (to-drop list)
  "Equivalent to CL's nthcdr or Haskell's drop - returns list with to-drop elements skipped - traversing at point of call in eager context,
deferring traversal in lazy context."
  (labels ((nthcdr-known-lazy-list/ (list)
             (if-lazy-eager
              (lazy-list-from-call
               (deferred-traversal-link-from-call-maker
                (standard-traversal-link
                 (build (end-call)
                        (let ((current (get-call-for-first list)))
                          (loop for i from 1 to to-drop while current do
                                (setq current (resolved (with-traversal-result (value next) (get-traversal-result current) (declare (ignore value)) (resolved next)))))
                          (if current
                              (get-traversal-result-new-end-call current end-call)
                            (get-traversal-result end-call))))
                 ())))
              (lazy-list-from-call
               (let ((current (get-call-for-first list)))
                 (loop for i from 1 to to-drop while current do
                       (setq current (resolved (with-traversal-result (value next) (get-traversal-result current) (declare (ignore value)) (resolved next)))))
                 (if current
                     current
                   **standard-terminating-end-call**)))

#|
               (standard-traversal-link
                (build (end-call)
                       (let ((current (get-call-for-first list)))
                         (loop for i from 1 to to-drop while current do
                               (setq current (resolved (with-traversal-result (value next) (get-traversal-result current) (resolved next)))))
                         (if current
                             (get-traversal-result-new-end-call current end-call)
                           (get-traversal-result end-call))))
                ()))
|#

              )))
    (typecase list
      (list (to-lazy-list (nthcdr to-drop list)))
      (lazy-list-list-based (to-lazy-list (nthcdr to-drop (get-list-head list))))
      (lazy-list
       (nthcdr-known-lazy-list/ list))
      (t (nthcdr-known-lazy-list/ (to-lazy-list list))))))
(defun nthcdr/ (to-drop list) (nthcdr/-implementation to-drop list))
(define-compiler-macro nthcdr/ (&whole form to-drop list)
  (declare (ignore to-drop list))
  (full-cdr-based-form-resolution form))




(defun drop/ (to-drop list)
  "Equivalent to CL's nthcdr or Haskell's drop - returns list with to-drop elements skipped - traversing at point of call in eager context,
deferring traversal in lazy context."
  (nthcdr/ to-drop list))
(define-compiler-macro drop/ (to-drop list)
  `(nthcdr/ ,to-drop ,list))



(defun null/ (list)
  "Returns nil if list has contents, a value otherwise.  Will only traverse a single element for lazy-lists"
  (etypecase list
    (null t)
    (list (null list))
    (sequence (zerop (length list)))
    (lazy-list-list-based (null (get-list-head list)))
    (lazy-list-known-empty t)
    (lazy-list (with-traversal-result (value next) (resolved (get-traversal-result (get-call-for-first list))) (declare (ignore value)) (not (resolved next))))
    (t nil)))

(defun non-null/ (list)
  "Not null/"
  (not (null/ list)))
(define-compiler-macro non-null/ (list)
  `(not (null/ ,list)))




(defun take/ (to-take list)
  "Returns a lazy-list of the first to-take elements from list.  Performance note:  The resulting lazy-list will tend to maintain a reference to the original list, convert to a static container (via to-list of to-array) to break this link."
  (assert (integerp to-take))
  (if (zerop to-take)
      (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)
    (lazy-list-from-call
     (standard-traversal-link
      (build (end-call current-call num-left)
             (if current-call
                 (with-traversal-result
                     (value next)
                  (get-traversal-result current-call)
                  (let ((next (resolved next)))
                    (if next
                        (let ((new-num-left (1- num-left)))
                          (traversal-result value (build end-call (if (zerop new-num-left) nil next) new-num-left)))
                      (unresolved (get-traversal-result end-call)))))
               (unresolved (get-traversal-result end-call))))
      ((get-call-for-first (to-lazy-list list)) to-take)))))



#|
(defun split-when---experimental/ (predicate list)
  "Returns lazy-lists for before-split-point, and split-point-and-after, as first and second value results"
  (labels ((get-split-result ()
             (labels ((split-for-proper-list (list)
                        (let ((current list)
                              (before-predicate-r nil))
                          (tagbody
                           top
                           (when current
                             (let ((val (car current)))
                               (when (not (funcall predicate val))
                                 (setq current (cdr current))
                                 (push val before-predicate-r)))))
                          (values (to-lazy-list (nreverse before-predicate-r)) (to-lazy-list current)))))
               (let* ((list (to-lazy-list list))
                      (current (read-point-built list))
                      (before-predicate-r nil))
                 (tagbody
                  top
                  (when (not (read-point-at-end current))
                    (let ((val (read-point-value current)))
                      (when (not (funcall predicate val))
                        (setq current (read-point-advanced current))
                        (push val before-predicate-r)
                        (go top)))))
                 (values
                  (to-lazy-list (nreverse before-predicate-r))
                  (typecase list
                    (lazy-list-read-point-based (lazy-list-from-read-point current))
                    (lazy-list-with-persistence (make-instance-2 'lazy-list-with-persistence :call-for-first (call-to-detach-from-read-point current)))
                    (t (make-instance-2 'lazy-list :call-for-first (call-to-detach-from-read-point current)))))))))
    (if-lazy-eager
     (let ((lock (make-thread-lock))
           (result-known nil)
           (result-first nil)
           (result-second nil))
       (labels ((ensure-result ()
                  (when (not result-known) (respecting-lock-if-present (lock) (when (not result-known) (multiple-value-setq (result-first result-second) (get-split-result)) (setq result-known t))))))
         (values
          (lazy-list-from-call (lambda () (ensure-result) (funcall (get-call-for-first result-first))))
          (lazy-list-from-call (lambda () (ensure-result) (funcall (get-call-for-first result-second)))))))
     (get-split-result))))
|#


(defun intersperse/ (val list)
  "Equivalent of Haskell's intersperse function - returns a lazy-list of val interspersed between elements of list.  If list is of length 0 or 1, val does not appear."
  (let ((traversal-result (resolved (get-traversal-result (get-call-for-first (to-lazy-list list))))))
    (let ((next (resolved (get-next traversal-result))))
      (if next
          (lazy-list-from-call
           (standard-traversal-link
            (build (end-call current-value current-resolved-next)
                   (traversal-result
                    current-value
                    (fixed-traversal-link-from-result-form
                     (if current-resolved-next
                         (with-traversal-result
                          (value next)
                          (get-traversal-result current-resolved-next)
                          (let ((next (resolved next)))
                            (if next
                                (traversal-result
                                 val
                                 (build end-call value next))
                              (get-traversal-result end-call))))
                       (get-traversal-result end-call)))))
            ((get-value traversal-result) next)))
        (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)))))


; returns 3 values - lazy-list to predicate true, lazy-list of remainder, and (lambda () (values value true-if-found)) at predicate true
(defun split-on-test/ (test list)
  (let* ((split-value-known nil)
         (known-split-value nil)
         (known-post-value-remainder nil)
         (rest-of-pre-predicate-read-point nil))
    (macrolet! ((replace-pp (replacement)
                  `(let ((,g!replacement ,replacement))
                     (unless ,g!replacement
                       (print ',replacement))
                     (setq rest-of-pre-predicate-read-point ,g!replacement))))
      (setq rest-of-pre-predicate-read-point
            (read-point-from-call
             (standard-traversal-link
              (build (end-call current-read-point)
                     (if (read-point-at-end current-read-point)
                         (progn
                           (replace-pp (read-point-from-call **standard-terminating-end-call**))
                           (setq known-post-value-remainder (read-point-from-call **standard-terminating-end-call**))
                           (get-traversal-result end-call))
                       (let ((value (read-point-value current-read-point))
                             (advanced (read-point-advanced current-read-point)))
                         (if (funcall test value)
                             (progn
                               (setq known-split-value value)
                               (setq split-value-known t)
                               (replace-pp (read-point-from-call **standard-terminating-end-call**))
                               (setq known-post-value-remainder advanced)
                               (get-traversal-result end-call))
                           (let ((new-tr (traversal-result value (build end-call advanced))))
                             (replace-pp (read-point-from-call (lambda (command) (assert (eq command :get-link)) new-tr))) ; have to make sure replace with this link not the source
                             new-tr)))))
              ((read-point-built (to-lazy-list list))))))
      (values
       (let ((x rest-of-pre-predicate-read-point)) (lazy-list-from-read-point x))
       (lazy-list-from-call
        (standard-traversal-link
         (build (end-call)
                (loop while (and (not (read-point-at-end rest-of-pre-predicate-read-point)) (not known-post-value-remainder)) do (replace-pp (read-point-advanced rest-of-pre-predicate-read-point)))
                (if known-post-value-remainder
                    (get-traversal-result-new-end-call (call-to-detach-from-read-point known-post-value-remainder) end-call)
                  (get-traversal-result end-call)))
         ()))
       (lambda ()
         (loop while (and (not (read-point-at-end rest-of-pre-predicate-read-point)) (not known-post-value-remainder)) do (replace-pp (read-point-advanced rest-of-pre-predicate-read-point)))
         (values known-split-value split-value-known))))))



(defun split-on-test-to-first-non-empty-before/ (test list)
  (multiple-value-bind (before after call)
      (split-on-test/ test list)
    (let ((before before)
          (call call)
          (after after))
      (let ((before-null (null/ before))
            (after-null (null/ after)))
        (loop while (and before-null (not after-null)) do
              (multiple-value-setq (before after call)
                  (split-on-test/ test after))
              (setq before-null (null/ before))
              (setq after-null (null/ after)))
        (multiple-value-bind (split-val split-val-present)
            (funcall call)
          (if (and before-null after-null (not split-val-present))
              (call-for-end)
            (values split-val split-val-present before before-null after after-null)))))))


; returns list of list, value, list, value, list, value, list where value = something that triggers test
(defun split-down-on-test/ (test list &key (keep-split-causing-elements nil) (keep-empty-non-split t) (process-split-causing-element #'identity) (process-non-split-causing-elements-list #'identity))
  (lazy-list-from-call
   (if keep-split-causing-elements
       (standard-traversal-link
        (build-keep (end-call list)
                    (multiple-value-bind (before after val-maker)
                        (split-on-test/ test list)
                      (multiple-value-bind (v exists)
                          (funcall val-maker)
                        (cond
                         ((and exists (null/ before) (not keep-empty-non-split)) (traversal-result (funcall process-split-causing-element v) (build-keep end-call after)))
                         (exists (traversal-result
                                  (funcall process-non-split-causing-elements-list before)
                                  (fixed-traversal-link (funcall process-split-causing-element v) (build-keep end-call after))))
                         ((not (null/ before)) (traversal-result
                                                (funcall process-non-split-causing-elements-list before)
                                                end-call))
                         (t (unresolved (get-traversal-result end-call)))))))
        (list))
     (standard-traversal-link
      (build-no-keep (end-call list)
               ; Find first non-null "before"
                     ; (progn (to-list list) (print "list good 1"))
                     (multiple-value-bind (before after call)
                         (split-on-test/ test list)
                       ; (print (list "*" before "__" after))
                       (let ((before before)
                             (call call)
                             (after after))
                         (declare (ignorable call))
                         (let ((before-null (null/ before))
                               (after-null (null/ after)))
                           (unless keep-empty-non-split
                             (loop while (and before-null (not after-null)) do
                                   (multiple-value-setq (before after call)
                                       (split-on-test/ test after))
                                   (setq before-null (null/ before))
                                   (setq after-null (null/ after))))
                           (if (and before-null after-null)
                               (unresolved (get-traversal-result end-call))
                             (progn
                               (traversal-result
                                before
                                (build-no-keep end-call after))))))))
      (list)))))

(defun group-by/ (test list)
  (labels ((grouped-commons (rp)
             (lazy-list-from-call
              (standard-traversal-link
               (build (end-call a b)
                      (if a
                          (let ((a-value (read-point-value a)))
                            (if (or (read-point-at-end b) (not (funcall test a-value (read-point-value b))))
                                (traversal-result
                                 a-value
                                 (build end-call nil nil))
                              (traversal-result
                               a-value
                               (build end-call b (read-point-advanced b)))))                              
                        (unresolved (get-traversal-result end-call))))
               (rp (read-point-advanced rp))))))
    (lazy-list-from-call
     (standard-traversal-link
      (build (end-call rp)
             (cond
              ((read-point-at-end rp) (unresolved (get-traversal-result end-call)))
              (t
               (traversal-result
                (grouped-commons rp)
                (unresolved
                 (build
                  end-call
                  (let ((a rp)
                        (b (read-point-advanced rp)))
                    (loop while (and (not (read-point-at-end b)) (funcall test (read-point-value a) (read-point-value b))) do
                          (setq a b)
                          (setq b (read-point-advanced b)))
                    b)))))))
      ((read-point-built (to-lazy-list list)))))))

(defun group/ (list) (group-by/ #'eql list))





(defun map/ (function first &rest other-lazy-lists)
  (assert (functionp function))
  (lazy-list-from-call
   (standard-traversal-link (build (end-call callers-list)
                                   (let ((result-stash
                                          (loop for caller in callers-list collect
                                                (with-traversal-result (value next)
                                                                       (get-traversal-result caller)
                                                                       (let ((next (resolved next)))
                                                                         (if next
                                                                             (cons next value)
                                                                           (return nil)))))))
                                     (if result-stash
                                         (traversal-result
                                          (apply function (mapcar (lambda (elt) (resolved (cdr elt))) result-stash))
                                          (build end-call (mapcar #'car result-stash)))
                                       (unresolved (get-traversal-result end-call)))))
                            ((mapcar
                              (lambda (elt) (get-call-for-first (to-lazy-list elt)))
                              (cons first other-lazy-lists))))))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter **sequence-sources**
    `(
      (:standard-lazy-list
       ,(lambda (input)
          `(:get-first-link (get-call-for-first (to-lazy-list ,input))
            :get-value-next-extractor ,(lambda (link-sym exit-form)
                                         `(with-traversal-result (value next)
                                              (get-traversal-result ,link-sym)
                                            (let ((next (resolved next)))
                                              (unless next ,exit-form)
                                              (values value next))))
            :get-value-resolution ,(lambda (value-sym) `(resolved ,value-sym)))))

      )))


;? Temporarily shelved.

(define-compiler-macro map/ (func first &rest rest)
  (let* ((call-form-precond-parameters (get-call-form-precond-parameters (function-literal-to-form func) (1+ (length rest))))
         (result-sym-lists (mapcar #'list* (first call-form-precond-parameters) (loop for i from 0 to (length rest) collect (list (gensym) (gensym) (gensym)))))
         (per-source-generators (loop for input in (cons first rest) collect (funcall (second (assoc :standard-lazy-list **sequence-sources**)) input)))
         (end-call-sym (gensym))
         (exit-sym (gensym))
         (next-sym (gensym))
         (value-sym (gensym))
         (build-func-sym (gensym)))
    `(lazy-list-from-call
      (standard-traversal-link
       (,build-func-sym (,end-call-sym ,@(mapcar #'fourth result-sym-lists))
                        (block ,exit-sym
                          (let ,(loop for result-sym-list in result-sym-lists append
                                      (list (second result-sym-list) (third result-sym-list)))
                            ,@(loop for result-sym-list in result-sym-lists
                                    for per-source-generator in per-source-generators
                                    collect
                                    `(multiple-value-bind (,value-sym ,next-sym)
                                         ,(funcall (getf per-source-generator :get-value-next-extractor)
                                                   (fourth result-sym-list)
                                                   `(return-from ,exit-sym (unresolved (get-traversal-result ,end-call-sym))))
                                       (setq ,(second result-sym-list) ,next-sym)
                                       (setq ,(third result-sym-list) ,value-sym)))
                            (traversal-result
                             (let ,(loop for result-sym-list in result-sym-lists collect `(,(first result-sym-list) ,(third result-sym-list)))
                               ,(get-call-form (function-literal-to-form func) call-form-precond-parameters (mapcar (rcurried #'getf :get-value-resolution) per-source-generators)))
                             (,build-func-sym ,end-call-sym ,@(mapcar #'second result-sym-lists))))))
       ; ,(mapcar (lambda (input) `(get-call-for-first (to-lazy-list ,input))) (cons first rest))
       ,(mapcar (rcurried #'getf :get-first-link) per-source-generators)
       ))))



; (funcall (compiler-macro-function 'map/) '(map/ #'1+ '(1 2 3 4)) nil)
; (pprint (funcall (compiler-macro-function 'map/) '(map/ (lambda (x) (* x 2)) '(1 2 3 4)) nil))
; (pprint (funcall (compiler-macro-function 'map/) '(map/ (lambda (x y) (* x y 2)) '(1 2 3 4) '(5 6 7 8)) nil))
; (defun test () (map/ (lambda (x y) (* x y 2)) '(1 2 3 4) '(5 6 7 8)))
; (defun test () (map/ #'+ '(1 2 3 4) '(5 6 7 8)))
; (pprint (funcall (compiler-macro-function 'map/) '(map/ (curried (curried #'+ 100) 200) '(1 2 3 4)) nil))
; (pprint (funcall (compiler-macro-function 'map/) '(map/ (curried (curried (lambda (a b c) (+ a b c)) 100) 200) '(1 2 3 4)) nil))
; (defun test () (map/ (curried (curried (lambda (a b c) (+ a b c)) 100) 200) '(1 2 3 4)))
; (pprint (funcall (compiler-macro-function 'map/) '(map/ (composed #'1+ #'1+) '(1 2 3 4)) nil))
; (defun test () (map/ (composed #'1+ #'1+) '(1 2 3 4)))
; (pprint (funcall (compiler-macro-function 'map/) '(map/ (constantly 69) '(1 2 3 4)) nil))

#|

|#

(defun split-positional/ (positional list)
  "Splits on positional - positional can be an integer zero-index, a function
  (that validates that an index is a split-point), or a list of indices that is assumed to already be sorted."
  (typecase positional
    (integer (multiple-value-bind (before after call)
                 (split-on-test/ (lambda (elt) (= (cdr elt) positional)) (map/ (lambda (elt pos) (cons elt pos)) list (iterate/ #'1+ 0)))
               (multiple-value-bind (split-val split-val-valid)
                   (funcall call)
                 (values (map/ #'car before) (map/ #'car (append/ (when split-val-valid split-val) after))))))
    (t
     (let ((split-masks
            (typecase positional
              (function (map/ positional (iterate/ #'1+ 0)))
              (t
               (map/
                (lambda (elt) (eql (car elt) (first/ (cdr elt))))
                (iterate/
                 (lambda (elt)
                   (eager
                     (destructuring-bind (index . remainder) elt (cons (1+ index) (if (eql index (car/ remainder)) (cdr/ remainder) remainder)))))
                 (cons 0 positional)))))))
       (labels ((build (split-remainder)
                  (lambda ()
                    (multiple-value-bind (head tail valid)
                        (head-tail/ split-remainder)
                        (if valid
                            (if (consp head)
                                (if (null/ tail)
                                    (values (list/ head) (lambda () nil nil))
                                  (multiple-value-bind (th tt tv)
                                      (head-tail/ tail)
                                    (declare (ignore tv))
                                    (if (consp th)
                                        (values (list/ head) (build tail))
                                      (values (list*/ head th) (build tt)))))
                              (values head (build tail)))
                          (call-for-end))))))
         (map/ (curried #'map/ #'car) (lazy-list-from-call (build (split-down-on-test/ #'cdr (map/ #'cons list split-masks) :keep-split-causing-elements t :keep-empty-non-split nil)))))))))



(defun take-while/ (test list)
  "Returns a lazy-list representing elements of list while test (run against values) returns true.  Performance note:  Will tend to maintain reference to the original list,
  create a new static list (via to-list or to-array) if this is a concern."
  (assert (functionp test))
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call current-call)
           (if current-call
               (with-traversal-result
                (value next)
                (get-traversal-result current-call)
                (let ((next (resolved next)))
                  (if next
                      (let ((value (resolved value)))
                        (if (funcall test value)
                            (traversal-result value (build end-call next))
                          (unresolved (get-traversal-result end-call))))
                    (unresolved (get-traversal-result end-call)))))
             (unresolved (get-traversal-result end-call))))
    ((resolved (get-call-for-first (to-lazy-list list)))))))



(defun drop-while/ (test list)
  "Returns the subset of list after test returns false - in eager context, traverses immediately - in lazy, upon first traversal of resultant lazy-list.  Performance note:  If list uses
  some form of memoization/caching, and another instance has \"cached ahead\", the result lazy-list will be bound to the cache until it can overtake it."
  (assert (functionp test))
  (labels ((get-read-point-after-test ()
             (let ((current (read-point-built (to-lazy-list list))))
               (tagbody
                top
                (when (and (not (read-point-at-end current)) (funcall test (read-point-value current)))
                  (setq current (read-point-advanced current))
                  (go top)))
               current)))
    (if-lazy-eager
     (lazy-list-from-call (fixed-traversal-link-from-result-form (get-traversal-result (call-to-detach-from-read-point (get-read-point-after-test)))))
     (lazy-list-from-call (call-to-detach-from-read-point (get-read-point-after-test))))))


(defun position/ (item list &key (test #'eql))
"Returns the first 0-index of item in list that satisfies test, nil if not found"
  (let ((current (read-point-built (to-lazy-list list)))
        (pos 0))
    (tagbody
     top
     (when (and (not (read-point-at-end current)) (not (funcall test (read-point-value current) item)))
       (setq current (read-point-advanced current))
       (incf pos)
       (go top)))
    (if (read-point-at-end current)
        nil
      pos)))



(defun nth/ (index list)
  "Equivalent of CL's NTH; but traverses CL sequences or lazy-lists"
  (assert (integerp index))
  (typecase list
    (array (aref list index))
    (list (nth index list))
    (t
     (let ((read-point (read-point-built (to-lazy-list list)))
           (known-at-end nil))
       (setq list nil) ; to help make "list" eligible for gc.
       (loop for i from 1 to index
             while (not (setq known-at-end (read-point-at-end read-point)))
             do
             (setq read-point (read-point-advanced read-point)))
       (unless known-at-end
         (read-point-value read-point))))))


(defun second/ (list)
  "Like CL's second; but can acommodate lazy-lists or CL sequences"
  (car/ (cdr/ list)))

(define-compiler-macro second/ (list) `(cadr/ ,list))

(defun third/ (list)
  "Like CL's third; but can acommodate lazy-lists or CL sequences"
  (car/ (cdr/ (cdr/ list))))
(define-compiler-macro third/ (list) `(caddr/ ,list))



(defun tails/ (list)
  "Returns list of lists, with each list being the (cdr/) of the previous one.  Final list in sequence is empty list."
  (labels ((tails-list-for-proper-list (list)
             (make-instance-2 'lazy-list-with-persistence
                              :call-for-first
                              (standard-traversal-link
                               (build (end-call remainder)
                                      (if remainder
                                          (traversal-result (to-lazy-list remainder) (build end-call (cdr remainder)))
                                        (traversal-result (list/) end-call)))
                               (list)))))
    (typecase list
      (list (tails-list-for-proper-list list))
      (lazy-list-list-based (tails-list-for-proper-list (get-list-head list)))
      (t
       (let ((new-class-type
              (cond
               ((typep list 'lazy-list-with-persistence) 'lazy-list-with-persistence) ; list based or with full persist, this persist (minor calc)

               ((typep list 'lazy-list-with-some-persistence) 'lazy-list-with-some-persistence) ; will not grant the memoize - may want memoized to
                                                                                                    ; surrender the readpoint
               (t 'lazy-list))))
         (make-instance-2 new-class-type
                          :call-for-first
                          (standard-traversal-link
                           (build (end-call call)
                                  (with-traversal-result (value next)
                                      (get-traversal-result call)
                                    (let ((next (resolved next)))
                                      (if next
                                          (let ((value (resolved value)))
                                            (traversal-result
                                             (make-instance-2 new-class-type :call-for-first (fixed-traversal-link value next))
                                             (build end-call next)))
                                        (traversal-result (list/) (fixed-traversal-link-from-result-form (get-traversal-result end-call)))))))
                           ((get-call-for-first (to-lazy-list list))))))))))



                                                            




(defun concat/ (list)
  (lazy-list-from-call
   (macrolet! ((do-slam (params-sym body end-call-sym)
                 `(unresolved
                   (with-slam-sinks (,params-sym)
                     (labels ((,g!slam-func (,g!tlc ,g!accum)
                                (with-traversal-result (,g!sub-list ,g!call-for-next)
                                                       (resolved (get-traversal-result ,g!tlc))
                                                       (let ((,g!call-for-next (resolved ,g!call-for-next)))
                                                         (if ,g!call-for-next
                                                             (slam-for-continue (to-lazy-list (resolved ,g!sub-list))
                                                                                (lambda (,g!count)
                                                                                  (,g!slam-func ,g!call-for-next (+ ,g!count ,g!accum))))
                                                           (end-call-thing ,end-call-sym ,g!accum))))))
                       (,g!slam-func top-level-call 0))))))
     (standard-traversal-link-parametric
      (build (end-call top-level-call)
             (with-traversal-result (sub-list call-for-next-sublist)
                                    (resolved (get-traversal-result top-level-call))
                                    (let ((call-for-next-sublist (resolved call-for-next-sublist)))
                                      (if call-for-next-sublist
                                          (unresolved
                                           (get-traversal-result-new-end-call
                                            (get-call-for-first (to-lazy-list (resolved sub-list)))
                                            (unresolved (build end-call call-for-next-sublist))))
                                        (unresolved (get-traversal-result end-call))))))
      ((get-call-for-first (to-lazy-list list)))))))


(defun append/ (&rest list-of-lists)
  #|
  (when (not (eql (length/ (concat/ list-of-lists)) (length/ (concat/ (map/ #'to-list list-of-lists)))))
    (setf heresy::a list-of-lists)
    (assert nil))
    |#
      
  (if (cdr list-of-lists) ; i.e. more than one
      (concat/ list-of-lists)
    (to-lazy-list (first list-of-lists))))

; (A B C) == (cons A (cons B C))
(defun list*/ (&rest list-list-terminated)
  "Basically CL's list* - \"conses\" all elements but last onto list in last parameter, returning a lazy-list."
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call remainder)
           (destructuring-bind (car-remainder &rest cdr-remainder) ; so as to only capture required pieces.
               remainder
           (if cdr-remainder
               (traversal-result car-remainder (build end-call cdr-remainder))
             (unresolved (get-traversal-result-new-end-call (get-call-for-first (to-lazy-list car-remainder)) end-call)))))
    (list-list-terminated))))




(defun assoc/ (item alist &rest rest)
  "CL's assoc; but works with lazy-lists of cons pairs"
  (typecase alist
    (list (apply #'assoc item alist rest))
    (t
     (destructuring-bind (&key (test #'eql) (key #'identity))
         rest
       (let ((current (read-point-built alist)))
         (tagbody
          top
          (when (not (read-point-at-end current))
            (when (not (funcall test item (funcall key (first (read-point-value current)))))
              (setq current (read-point-advanced current))
              (go top))))
         (when (not (read-point-at-end current))
           (read-point-value current)))))))


(defun prepend/ (&rest list-list-terminated)
  "CL's list*, returning a lazy-list"
  (apply #'list*/ list-list-terminated))



(defun filter/ (predicate list)
  (lazy-list-from-call
   (standard-traversal-link
    (filter-call (end-call current-call)
                 (with-traversal-result
                  (value next)
                  (get-traversal-result current-call)
                  (let ((next (resolved next))
                        (value value))
                    (block nil
                      (tagbody
                       top
                       (if next
                           (let ((val (resolved value)))
                             (if (funcall predicate val)
                                 (return (traversal-result val (filter-call end-call next)))
                               (with-traversal-result
                                   (v2 n2)
                                   (get-traversal-result next)
                                 (setq next n2)
                                 (setq value v2)
                                 (go top))))
                         (return (get-traversal-result end-call))))))))
    ((get-call-for-first (to-lazy-list list))))))







; Needs optimization, to put 
(defun nub-by/ (equality list)
  (let ((hash (make-hash-table :test equality))
        (readpoint-seeking-end nil))
    (values
     (let ((return-list
            (memoized/
             (lazy-list-from-call
              (standard-traversal-link
                (read-point-to-call (end-call start)
                             (let ((current start)
                                   (value nil))
                               (tagbody
                                top
                                (when (not (read-point-at-end current))
                                  (setq value (read-point-value current))
                                  (multiple-value-bind (current-val current-val-valid) (gethash value hash)
                                    (declare (ignore current-val))
                                    (if current-val-valid
                                        (progn
                                          (setq current (read-point-advanced current))
                                          (go top))
                                      (setf (gethash value hash) t)))))
                               (if (read-point-at-end current)
                                   (unresolved (get-traversal-result end-call))
                                 (traversal-result
                                  value
                                  (read-point-to-call end-call (read-point-advanced current))))))
                ((read-point-built (to-lazy-list list))))))))
       (setq readpoint-seeking-end (read-point-built return-list))
       return-list)
     (lambda (key)
       (when readpoint-seeking-end
         (loop while (not (read-point-at-end readpoint-seeking-end)) do (setq readpoint-seeking-end (read-point-advanced readpoint-seeking-end))))
       (multiple-value-bind (dummy exists) (gethash key hash) (declare (ignore dummy)) exists)))))

(defun nub/ (list) (nub-by/ #'eql list))

(defun and/ (list)
  "Returns last element or nil"
  (let ((last nil))
    (block :exit
      (loop-over/ elt (to-lazy-list list)
        (if elt
            (setq last elt)
          (return-from :exit nil)))
      last)))

(defun or/ (list)
;  "Returns first non-nil element or nil"
  (block :exit
    (loop-over/ elt (to-lazy-list list)
      (if elt
          (return-from :exit elt)))
    nil))

(defun latch-on/ (func list &key (initial-value nil))
  (labels ((build (read-point latched latched-val)
             (lambda ()
               (if (read-point-at-end read-point)
                   (call-for-end)
                 (if latched
                     (values latched-val (build (read-point-advanced read-point) t latched-val))
                   (let ((val (read-point-value read-point)))
                     (if (funcall func val)
                         (values val (build (read-point-advanced read-point) t val))
                       (values
                        initial-value
                        (build (read-point-advanced read-point) nil nil)))))))))
    (lazy-list-from-call (build (read-point-built (to-lazy-list list)) nil nil))))

(defun foldl/ (function first list)
  (assert (functionp function))
  (etypecase list
    (sequence (reduce function list :from-end nil :initial-value first))
    (lazy-list-list-based
     (reduce function (get-list-head list) :from-end nil :initial-value first))
    (lazy-list
     (let ((accum first)
           (current (get-call-for-first list)))
       (tagbody
        top
        (with-traversal-result (val next)
            (get-traversal-result current)
            (let ((next (resolved next)))
              (when next
                (setf accum (funcall function accum (resolved val)))
                (setf current next)
                (go top)))))
       accum))))




(defun foldl1/ (function list)
  (typecase list
    (sequence (reduce function list :from-end nil))
    (lazy-list-list-based (reduce function (get-list-head list) :from-end nil))
    (t
     (with-traversal-result (value next)
         (get-traversal-result (get-call-for-first (to-lazy-list list)))
         (let ((current (resolved next))
               (accum (resolved value)))
           (loop while current do
                 (with-traversal-result (value next)
                                        (get-traversal-result current)
                                        (let ((next (resolved next)))
                                          (setq current next)
                                          (when next
                                            (setq accum (funcall function accum (resolved value)))))))
           accum)))))



(defun foldr/ (function first list)
  (assert (functionp function))
  (etypecase list
    (sequence (reduce function list :from-end t :initial-value first))
    (lazy-list-list-based
     (reduce function (get-list-head list) :from-end t :initial-value first))
    (lazy-list
     (reduce function (to-list list) :from-end t :initial-value first))))

(defun foldr1/ (function list)
  (assert (functionp function))
  (etypecase list
    (sequence (reduce function list :from-end t ))
    (lazy-list-list-based
     (reduce function (get-list-head list) :from-end t))
    (lazy-list
     (reduce function (to-list list) :from-end t ))))


(defun scanl/ (function first list)
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call first call)
           (with-traversal-result
            (val next)
            (get-traversal-result call)
            (let ((next (resolved next)))
              (if next
                  (traversal-result first (build end-call (funcall function first (resolved val)) next))
                (traversal-result first end-call)))))
    (first (get-call-for-first (to-lazy-list list))))))


(defun scanl1/ (function list)
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call first first-valid call)
           (with-traversal-result
            (val next)
            (get-traversal-result call)
            (let ((next (resolved next)))
              (if next
                  (if first-valid
                      (traversal-result first (build end-call (funcall function first (resolved val)) t next))
                    (unresolved (get-traversal-result (build end-call (resolved val) t next))))
                (traversal-result first end-call)))))
    (nil nil (get-call-for-first (to-lazy-list list))))))


(defun scanr/ (function first list)
  (labels ((build-result-list-call ()
             (let ((result (list first)))
               (loop for elt in (reverse (to-list list)) do
                     (push (funcall function elt (car result)) result))
               (list-to-lazy-list-call result))))
    (if-lazy-eager
     (lazy-list-from-call
      (deferred-traversal-link-from-call-maker (build-result-list-call)))
     (lazy-list-from-call (build-result-list-call)))))



(defun scanr1/ (function list)
  (labels ((build-result-list-call ()
             (let* ((reversed (reverse (to-list list)))
                    (result (list (first reversed))))
               (if reversed
                   (progn
                     (loop for elt in (cdr reversed) do
                           (push (funcall function elt (car result)) result))
                     (list-to-lazy-list-call result))
                 (list-to-lazy-list-call nil)))))
    (if-lazy-eager
     (lazy-list-from-call
      (deferred-traversal-link-from-call-maker (build-result-list-call)))
     (lazy-list-from-call (build-result-list-call)))))



(defun grouped-by-firsts/ (test list-of-pair-conses)
  (labels ((build-list ()
             (let ((hash (make-hash-table :test test)))
               (loop-over/
                   elt
                   (to-lazy-list list-of-pair-conses)
                 (destructuring-bind (key . value) (to-list elt)
                   (setf (gethash key hash) (cons value (gethash key hash)))))
               (values
                (get-call-for-first
                 (map/ (lambda (key) (cons key (gethash key hash)))
                       (loop for key being the hash-keys of hash collect (progn (setf (gethash key hash) (nreverse (gethash key hash))) key))))
                (lambda (key)
                  (gethash key hash))))))
    (if-lazy-eager
     (let ((build-call-values-list nil))
       (labels ((verify-data-ready ()
                  (cond (build-call-values-list)
                        (t (setq build-call-values-list (multiple-value-list (build-list)))))))
         (values
          (lazy-list-from-call
           (fixed-traversal-link-from-result-form (get-traversal-result (first (verify-data-ready)))))
          (second (verify-data-ready)))))
     (multiple-value-bind (list query)
         (build-list)
       (values
        (lazy-list-from-call list)
        query)))))



(defun grouped-cdrs-by-car/ (list-of-cons-pairs &key (test 'eql))
  "Takes a list of cons pairs, of the form (first . second) - returns a list of conses of the form (first . (second 0 second1 second2 second3....)) as the first value, where the seconds are matches on first.
Second return value is a function, that returns a list of seconds based on a search key/first as first value, found (T or NIL) as second.
When run in an eager context, grouped-by-first-in-cons-pairs/ calculates the internal hash immediately.
When run in a lazy context, the creation of the internal hash is deferred - and is on the first request of either the resultant list, or execution of the second return value."
  (grouped-by-firsts/ test list-of-cons-pairs))

(defun grouped-seconds-by-first/ (list-of-list-pairs &key (test 'eql))
  "Equivalent to grouped-cdrs-by-car/ , except that the input pairs come as a list of lists, instead of a list of conses."
  (grouped-by-firsts/ test (map/ (curried #'apply #'cons) list-of-list-pairs)))




(defun sort-by/ (ordering list)
  (labels ((get-as-distinct-sortable ()
             (typecase list
               (list (copy-seq list))
               (sequence (map 'list #'identity list))
               (lazy-list-list-based (copy-seq (to-list list)))
               (t (to-list list))))) ; based on assumption that only sequences and lazy-list-list-based will to-list to a sequence

    (assert (functionp ordering))
    (if-lazy-eager
     (lazy-list-from-call
      (fixed-traversal-link-from-result-form
       (let ((sorted (get-as-distinct-sortable)))
         (setq sorted (sort sorted ordering))
         (get-traversal-result
          (get-call-for-first (to-lazy-list sorted))))))
     (let ((sorted (get-as-distinct-sortable)))
       (setq sorted (sort sorted ordering))
       (to-lazy-list sorted)))))

(defun sort/ (list)
  (sort-by/ #'< list))

(defun reverse/ (list) ; could use some optimization
  (labels ((doit ()
             (typecase list
               (sequence (to-lazy-list (reverse list)))
               (lazy-list-list-based (to-lazy-list (reverse (get-list-head list))))
               (t (reverse (to-list list))))))
    (if-lazy-eager
     (deferred-lazy-list (doit))
     (doit))))



(defmacro self-ref-list/ (ref-name &body definition)
  (let ((self-sym (gensym)))
    `(lazy-list-from-call
      (fixed-traversal-link-from-result-form
        (get-traversal-result
         (get-call-for-first
          (let ((,self-sym :error))
            (symbol-macrolet ((,ref-name (lazy-list-from-call (fixed-traversal-link-from-result-form (get-traversal-result ,self-sym)))))
              (let* ((ref (memoized/ (lazy ,@definition)))
                     (call (get-call-for-first ref)))
                (setq ,self-sym call)
                ref)))))))))



(defmacro let/ (definitions &body body)
  `(let
       ,(mapcar
         (lambda (entry)
           (if (consp entry)
               (destructuring-bind (var-name value-clause)
                   (if (consp entry) entry (list entry nil))
                 `(,var-name
                   (self-ref-list/ ,var-name ,value-clause)))
             entry))
         definitions)
     ,@body))



(defstruct hash-table-description
  (hash-function :error :type function :read-only t)
  (test :error :type function :read-only t))

(defstruct (const-hash-table
            (:constructor new-const-hash-table (hash-table-description root-node)))
  (hash-table-description :error :read-only t)
  (root-node :error :read-only t))
; (defconstant old-make-const-hash-table #'make-const-hash-table)




(defstruct equal-hash-key-value-pairs
  (count :error :type fixnum :read-only t)
  (key-value-pairs :error :type cons :read-only t)) ; this is a list, BIGGER than one; but ending not on nil but on the hash value.
; hash value is only needed when strikign the end.  Hash value is a fixnum.

(defstruct hash-leaf-node
  (hash :error :type fixnum :read-only t)
  (key-value :error :type cons :read-only t))

(defstruct (hash-bucket-node
            (:constructor new-hash-bucket-node (count contents-mask lookup)))
  (count :error :type fixnum :read-only t)
  (contents-mask :error :type fixnum :read-only t)
  (lookup :error :type simple-array :read-only t))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter **bucket-node-bucket-size** 16)
  (defparameter **per-level-shift** 4)
  )

(defmacro shift-at-end (shift)
  `(< ,shift -31))


(defun make-const-hash-table (&key (hash-function #'sxhash) (test #'eql) (key-value-pairs nil))
  (let ((result (new-const-hash-table (make-hash-table-description :test test :hash-function hash-function) nil)))
    (if key-value-pairs
        (const-hash-table-with-changes result nil key-value-pairs)
      result)))




(defun const-hash-table-count (const-hash-table)
  (let ((root-node (const-hash-table-root-node const-hash-table)))
    (etypecase root-node
      (hash-bucket-node (hash-bucket-node-count root-node))
      (hash-leaf-node 1)
      (cons 1)
      (equal-hash-key-value-pairs (equal-hash-key-value-pairs-count root-node))
      (null 0))))

(defmethod print-object ((const-hash-table const-hash-table) stream)
  (print-unreadable-object (const-hash-table stream :type t :identity t)
    (let ((description (const-hash-table-hash-table-description const-hash-table)))
      (princ
       (format nil "(:FUNCTION ~A :TEST ~A :COUNT ~A)"
               (hash-table-description-hash-function description)
               (hash-table-description-test description)
               (const-hash-table-count const-hash-table))))))


; second value is nil if not found
(defun const-hash-table-lookup (const-hash-table key)
  (with-accessors ((hash-table-description const-hash-table-hash-table-description))
      const-hash-table
    (with-accessors ((hash-function hash-table-description-hash-function)
                     (test hash-table-description-test))
        hash-table-description
      (let* ((hash (funcall hash-function key))
             (rolling-hash hash)
             (node (const-hash-table-root-node const-hash-table)))
        (declare (type fixnum hash rolling-hash))

        (block :exit
          (loop
           (etypecase node
             (hash-bucket-node
              (let ((bucket-to-find (logand rolling-hash #.(1- **bucket-node-bucket-size**)))
                    (contents-mask (hash-bucket-node-contents-mask node)))
                (declare (type fixnum bucket-to-find contents-mask))
                (if (plusp (logand contents-mask (ash 1 bucket-to-find))) ; see if there's an entry at the bucket we want.
                    (let ((current-bucket-index 0)) ; loop through others to get the index in lookup
                      (declare (type fixnum current-bucket-index))
                      (loop for bucket from 0 to (1- bucket-to-find) do
                            (when (plusp (logand contents-mask (ash 1 bucket)))
                              (incf current-bucket-index)))
                      (setq node (aref (hash-bucket-node-lookup node) current-bucket-index))
                      (setq rolling-hash (ash rolling-hash #.(- **per-level-shift**))))
                  (return-from :exit (values nil nil)))))

             (hash-leaf-node
              (return-from :exit
                (progn
                  (if (funcall test key (car (hash-leaf-node-key-value node)))
                      (values (cdr (hash-leaf-node-key-value node)) t)
                    (values nil nil)))))

             (cons
              (return-from :exit
                (if (funcall test key (car node))
                    (values (cdr node) t)
                  (values nil nil))))
             (equal-hash-key-value-pairs
              (loop for key-value-remainder on (equal-hash-key-value-pairs-key-value-pairs node) do
                    ; (declare (type cons key-value-remainder))
                    (let ((key-value (car key-value-remainder)))
                      (declare (type cons key-value))
                      (when (funcall test key (car key-value))
                        (return-from :exit (values (cdr key-value) t)))))
              (return-from :exit (values nil nil)))
             (null (return-from :exit (values nil nil))))))))))


(defun const-hash-table-with-changes (const-hash-table keys-to-remove key-value-pairs-to-add)
  (with-accessors ((hash-table-description const-hash-table-hash-table-description)
                   (root-node const-hash-table-root-node))
      const-hash-table
    (let ((test (hash-table-description-test hash-table-description))
          (hash-function (hash-table-description-hash-function hash-table-description)))
      (labels ((get-node-element-count (node)
                 (etypecase node
                   (null 0)
                   (hash-bucket-node (hash-bucket-node-count node))
                   (hash-leaf-node 1)
                   (cons 1)
                   (equal-hash-key-value-pairs (equal-hash-key-value-pairs-count node))))
               (from-hash-leaf-nodes (hash-leaf-nodes-to-add shift)
                 (declare (type fixnum shift))
                 (cond
                  ((null hash-leaf-nodes-to-add) nil)
                  ((cdr hash-leaf-nodes-to-add) ; more than one to add - if at shift-end, they're all equal-hash else make lookup.
                   (if (shift-at-end shift)
                       (let ((count 0)
                             (key-value-pairs-r nil) ; list will dot-end with hash value instead of nil
                             ; (known-hash (hash-leaf-node-hash (cdar hash-leaf-nodes-to-add)))
                             )
                         (declare (type fixnum count #| known-hash |#))
                         (loop while hash-leaf-nodes-to-add do
                               (let* ((current hash-leaf-nodes-to-add) ; we'll be recycling the cons "current"
                                      (new-key-value (hash-leaf-node-key-value (cdar current)))
                                      (new-key (car new-key-value)))
                                 (setf hash-leaf-nodes-to-add (cdr current))
                                 (unless ; loop returns true if replacement made
                                     (loop for key-value-pairs-r-remainder on key-value-pairs-r do
                                           (when (funcall test (caar key-value-pairs-r-remainder) new-key)
                                             (setf (car key-value-pairs-r-remainder) new-key-value)
                                             (return t)))
                                   (incf count) ; only count additions, not the replacement inside the loop.
                                   (setf (car current) new-key-value) ; recycle cons "current"
                                   (setf (cdr current) key-value-pairs-r)
                                   (setf key-value-pairs-r current))))
                         (case count
                           (1 (car key-value-pairs-r)) ; safe assumption - if we only have one, all ins must have been identical and we kept the last.
                           (t (make-equal-hash-key-value-pairs :count count :key-value-pairs key-value-pairs-r)))) ; reversal doesn't matter here - list is new and unique
                     (let ((contents-mask 0))
                       (declare (type fixnum))
                       ; Modify all the cars in the hash-leaf-nodes-to-add list to contain the bucket-hash
                       (loop for hash-leaf-node-to-add in hash-leaf-nodes-to-add do
                             (let ((bucket (logand (ash (hash-leaf-node-hash (cdr hash-leaf-node-to-add)) shift) #.(1- **bucket-node-bucket-size**))))
                               (declare (type fixnum bucket))
                               (setf contents-mask (logior contents-mask (ash 1 bucket)))
                               (setf (car hash-leaf-node-to-add)
                                     bucket)))
                       (let* ((current-ordered (stable-sort hash-leaf-nodes-to-add (lambda (a b) (< (the fixnum (car a)) (the fixnum (car b))))))
                              (lookup (make-array (the fixnum (logcount contents-mask)))) ; safe to assume as no additions to nil will return 0 count/nil
                              (count 0)
                              (sub-shift (- shift #.**per-level-shift**))
                              (basis-if-single nil)
                              (current-insertion-index 0))
                         (declare (type fixnum count current-insertion-index sub-shift))
                         (loop while current-ordered do
                               (let* ((bucket-number (caar current-ordered))
                                      (additions-for-bucket-r current-ordered))
                                 (setf current-ordered (cdr current-ordered))
                                 (setf (cdr additions-for-bucket-r) nil)
                                 (loop while (and current-ordered (eql (caar current-ordered) bucket-number)) do
                                       (let ((temp current-ordered))
                                         (setf current-ordered (cdr current-ordered))
                                         (setf (cdr temp) additions-for-bucket-r)
                                         (setf additions-for-bucket-r temp)))
                                 (setq basis-if-single (cdar additions-for-bucket-r))
                                 (let ((result (from-hash-leaf-nodes (nreverse additions-for-bucket-r) sub-shift)))
                                   (incf count (get-node-element-count result))
                                   (setf (aref lookup current-insertion-index) result)
                                   (incf current-insertion-index))))
                         (if (and (= count 1) basis-if-single)
                             (progn
                               basis-if-single)
                           (new-hash-bucket-node count contents-mask lookup))))))
                  ((shift-at-end shift) (hash-leaf-node-key-value (cdar hash-leaf-nodes-to-add)))
                  (t (cdar hash-leaf-nodes-to-add))))
               (with-removals-and-additions (node hash-key-pairs-to-remove hash-leaf-nodes-to-add shift)
                 (declare (type fixnum shift)
                          (type list hash-key-pairs-to-remove hash-leaf-nodes-to-add))
                 (etypecase node
                   (null (from-hash-leaf-nodes hash-leaf-nodes-to-add shift))
                   (cons
                    (if (block :full-removal-test
                          (loop for hash-key-pair-to-remove in hash-key-pairs-to-remove do
                                (let ((actual-pair-to-remove (cdr hash-key-pair-to-remove)))
                                  (when (funcall test (cdr actual-pair-to-remove) (car node))
                                    (return-from :full-removal-test t))))
                          (loop for hash-leaf-node-to-add in hash-leaf-nodes-to-add do
                                (let ((actual-hash-leaf-node (cdr hash-leaf-node-to-add)))
                                  (when (funcall test (car node) (car (hash-leaf-node-key-value actual-hash-leaf-node)))
                                    (return-from :full-removal-test t))))) ; test to see if "node" should be removed.
                        (from-hash-leaf-nodes hash-leaf-nodes-to-add shift) ; forget about this, and build off a new nil with the adds only.
                      (from-hash-leaf-nodes (cons (cons nil (make-hash-leaf-node :hash (funcall hash-function (car node)) :key-value node)) hash-leaf-nodes-to-add) shift))) ; order not important - no match
                   (hash-leaf-node
                    (if (block :full-removal-test
                          (loop for hash-key-pair-to-remove in hash-key-pairs-to-remove do
                                (let ((actual-pair-to-remove (cdr hash-key-pair-to-remove)))
                                  (when (and
                                         (eql (car actual-pair-to-remove) (hash-leaf-node-hash node)) ; hashes equal
                                         (funcall test (cdr actual-pair-to-remove) (car (hash-leaf-node-key-value node))))
                                    (return-from :full-removal-test t))))
                          (loop for hash-leaf-node-to-add in hash-leaf-nodes-to-add do
                                (let ((actual-hash-leaf-node (cdr hash-leaf-node-to-add)))
                                  (when (and
                                         (eql (hash-leaf-node-hash actual-hash-leaf-node) (hash-leaf-node-hash node))
                                         (funcall test (car (hash-leaf-node-key-value node)) (car (hash-leaf-node-key-value actual-hash-leaf-node))))
                                    (return-from :full-removal-test t))))) ; test to see if "node" should be removed.
                        (from-hash-leaf-nodes hash-leaf-nodes-to-add shift) ; forget about this, and build off a new nil with the adds only.
                      (from-hash-leaf-nodes (cons (cons nil node) hash-leaf-nodes-to-add) shift))) ; order not important - no match
                   (equal-hash-key-value-pairs
                    ; note - by the time we get here, all hashes should match already.  Only thing to do is deletions and additions based on equality tests.
                    ; first thing to do is find the unaffected sublist, i.e. the one that will not be modded by removal or addition (overwrite).
                    (let ((intact-retained-sublist (equal-hash-key-value-pairs-key-value-pairs node))
                          (intact-retained-sublist-start-index 0)
                          (indices-to-remove-r nil)
                          (num-indices-to-remove 0)
                          (spare-cons-bank nil))
                      (declare (type fixnum num-indices-to-remove intact-retained-sublist-start-index))
                      (macrolet ((cons-from-spares (car cdr)
                                   (let ((sym (gensym)))
                                     `(let ((,sym spare-cons-bank))
                                        (setq spare-cons-bank (cdr spare-cons-bank))
                                        (setf (car ,sym) ,car)
                                        (setf (cdr ,sym) ,cdr)
                                        ,sym)))
                                 (cons-from-spares-if-available (car cdr)
                                   `(if spare-cons-bank
                                        (cons-from-spares ,car ,cdr)
                                      (cons ,car ,cdr)))
                                 (recycle-cons (cons)
                                   (let ((sym (gensym)))
                                     `(let ((,sym ,cons))
                                        (setf (cdr ,sym) spare-cons-bank)
                                        (setq spare-cons-bank ,sym))))
                                 (advance-cons-recycling-prior (var)
                                   (assert (symbolp var))
                                   (let ((next (gensym)))
                                     (assert (symbolp var))
                                     `(let ((,next (cdr ,var)))
                                        (recycle-cons ,var)
                                        (setq ,var ,next))))                      
                                 (strip-data-slots-for-cons-spares (list)
                                   (let ((current-sym (gensym))
                                         (temp-sym (gensym)))
                                     `(let ((,current-sym ,list))
                                        (loop while ,current-sym do
                                              (let ((,temp-sym (car ,current-sym)))
                                                (setf (car ,current-sym) (cdar ,current-sym))
                                                (setf (cdr ,temp-sym) spare-cons-bank)
                                                (setf spare-cons-bank ,temp-sym)
                                                (setf ,current-sym (cdr ,current-sym))))))))
                        ; note - data slots are being stripped out of the input vars
                        (strip-data-slots-for-cons-spares hash-key-pairs-to-remove)
                        (strip-data-slots-for-cons-spares hash-leaf-nodes-to-add)
                        ; destroy the hash-leaf-nodes-to-add by pulling out the key-value-pairs to list instead
                        (let ((key-value-pairs-to-add hash-leaf-nodes-to-add))
                          (loop for elt on hash-leaf-nodes-to-add do (setf (car elt) (hash-leaf-node-key-value (car elt))))
                          ; Following code works on the following assumption:  Everything CURRENTLY in the list
                          ; is unique.  So there should be enough conses in "spare-cons-bank", as 
                          ; there can't be more values destroyed or overwritten than there were overwriters.
                          (loop for key-value-remainder on (equal-hash-key-value-pairs-key-value-pairs node)
                                for current-index from 0 do
                                (let* ((key-value (the cons (car key-value-remainder)))
                                       (key (car key-value)))
                                  (when (cond
                                         ((loop for hash-key-pair-to-remove in hash-key-pairs-to-remove do
                                                (when (funcall test (cdr hash-key-pair-to-remove) key)
                                                  (return t))))
                                         (t
                                          (loop for key-value-pair-to-add in key-value-pairs-to-add do
                                                (when (funcall test (car key-value-pair-to-add) key)
                                                  (return t)))))
                                    (setq intact-retained-sublist (cdr key-value-remainder))
                                    (setq intact-retained-sublist-start-index (1+ current-index))
                                    (incf num-indices-to-remove)
                                    (setf indices-to-remove-r (cons-from-spares current-index indices-to-remove-r)))))
                          ; hash-key-pairs-to-remove no longer required.  Recycle the conses.
                          (let ((current hash-key-pairs-to-remove))
                            (loop while current do
                                  (let ((next (cdr current)))
                                    (setf (cdar current) spare-cons-bank)
                                    (setf spare-cons-bank (car current))
                                    (setf (cdr current) spare-cons-bank)
                                    (setf spare-cons-bank current)
                                    (setq current next))))
                          ; note - cannot damage any of node's key-valuepairs or their conses.
                          (let ((copied-out-sublist-r intact-retained-sublist)
                                (vulnerable-range-of-cos 0)
                                (count-delta 0)
                                (current-index-to-remove (nreverse indices-to-remove-r)))
                            (declare (type fixnum vulnerable-range-of-cos))
                            (loop for key-value-pair-remainder on (equal-hash-key-value-pairs-key-value-pairs node)
                                  for current-index from 0 to (1- intact-retained-sublist-start-index) do
                                  (let ((key-value-pair (the cons (car (the cons key-value-pair-remainder)))))
                                    (if (eql current-index (car current-index-to-remove))
                                        (progn
                                          (incf count-delta -1)
                                          (advance-cons-recycling-prior current-index-to-remove))
                                      (progn
                                        (incf vulnerable-range-of-cos)
                                        (setq copied-out-sublist-r (cons-from-spares-if-available key-value-pair copied-out-sublist-r))))))
                            (let ((remaining-key-value-pairs-to-add key-value-pairs-to-add))
                              (loop while remaining-key-value-pairs-to-add do
                                    (let ((key-value-pair-to-add (car remaining-key-value-pairs-to-add)))
                                      (cond ((loop for copied-out-sub-head on copied-out-sublist-r
                                                   for dummy from 1 to vulnerable-range-of-cos
                                                   do
                                                   (when (funcall test (caar copied-out-sub-head) (car key-value-pair-to-add))
                                                     (setf (car copied-out-sub-head) key-value-pair-to-add)
                                                     (advance-cons-recycling-prior remaining-key-value-pairs-to-add)
                                                     (incf count-delta) ; added a new.  Will be a wash if a replacement caused by a prior removal building copied-out-sublist-r originally.
                                                     (return t))))
                                            (t
                                             (let ((next (cdr remaining-key-value-pairs-to-add)))
                                               (setf (cdr remaining-key-value-pairs-to-add) copied-out-sublist-r)
                                               (setf copied-out-sublist-r remaining-key-value-pairs-to-add)
                                               (setf remaining-key-value-pairs-to-add next)
                                               (incf vulnerable-range-of-cos)
                                               (incf count-delta)))))))
                            (let ((count (+ count-delta (equal-hash-key-value-pairs-count node))))
                              (declare (type fixnum count))
                              (case count
                                (0 nil)
                                (1 (first copied-out-sublist-r))
                                ;(1 (make-hash-leaf-node :hash (cdar copied-out-sublist-r) :key-value (first copied-out-sublist-r)))
                                (t (make-equal-hash-key-value-pairs
                                    :count (+ count-delta (equal-hash-key-value-pairs-count node))
                                    :key-value-pairs copied-out-sublist-r)))))))))
                   (hash-bucket-node
                    (macrolet ((hash-to-mask (hash)
                                 `(ash 1 (logand (ash ,hash shift) #.(1- **bucket-node-bucket-size**)))))
                      (let ((original-contents-mask (hash-bucket-node-contents-mask node))
                            (all-removals-mask 0)
                            (additions-to-existing-buckets-mask 0)
                            (additions-to-new-buckets-mask 0))
                        (declare (type fixnum original-contents-mask additions-to-existing-buckets-mask additions-to-new-buckets-mask))
                        ; first, go through hash-key-pairs-to-remove and update the car to masks.
                        ; any whose masks don't "and" with original-contents-mask can be outright discarded.
                        (let ((hash-key-pairs-to-remove-r nil))
                          (let ((current hash-key-pairs-to-remove))
                            (loop while current do
                                  (let ((next (cdr current))
                                        (mask (hash-to-mask (cadar current))))
                                    (declare (type fixnum mask))
                                    (unless (zerop (logand mask original-contents-mask)) ; make sure it's removing something that may be there.
                                      (setf all-removals-mask (logior mask all-removals-mask))
                                      (setf (caar current) mask)
                                      (setf (cdr current) hash-key-pairs-to-remove-r)
                                      (setf hash-key-pairs-to-remove-r current))
                                    (setq current next))))
                          (setq hash-key-pairs-to-remove-r (sort hash-key-pairs-to-remove-r (lambda (a b) (< (the fixnum (car a)) (the fixnum (car b))))))
                          ; hash-key-pairs-to-remove-r is what should be used, hash-key-pairs-to-remove is now destroyed.
                          ; Now, go through additions and give them masks too.  Sort into 2 lists - new-bucket and clashing.
                          (let ((additions-existing-bucket-r nil)
                                (additions-new-bucket-r nil))
                            (let ((current hash-leaf-nodes-to-add))
                              (loop while current do
                                    (let ((next (cdr current))
                                          (mask (hash-to-mask (hash-leaf-node-hash (cdar current)))))
                                      (declare (type fixnum mask))
                                      (setf (caar current) mask)
                                      (if (zerop (logand mask original-contents-mask))
                                          (progn
                                            (setf additions-to-new-buckets-mask (logior mask additions-to-new-buckets-mask))
                                            (setf (cdr current) additions-new-bucket-r)
                                            (setf additions-new-bucket-r current))
                                        (progn
                                          (setf additions-to-existing-buckets-mask (logior mask additions-to-existing-buckets-mask))
                                          (setf (cdr current) additions-existing-bucket-r)
                                          (setf additions-existing-bucket-r current)))
                                      (setq current next))))
                            (let ((additions-existing-bucket (stable-sort (nreverse additions-existing-bucket-r) (lambda (a b) (< (the fixnum (car a)) (the fixnum (car b))))))
                                  (additions-new-bucket (stable-sort (nreverse additions-new-bucket-r) (lambda (a b) (< (the fixnum (car a)) (the fixnum (car b)))))))
                              ; additions-existing-bucket and additions-new-bucket set up, hash-leaf-nodes-to-add should be considered destroyed.
                              ; I'll go to hell for this unhygienic macro action :(
                              (macrolet ((get-changed-bucket (&body body)
                                           `(let ((old-lookup (hash-bucket-node-lookup node))
                                                  (current-read-index 0)
                                                  (current-read-mask 1)
                                                  (sub-shift (- shift #.**per-level-shift**))
                                                  (positive-contents-mask (logior additions-to-existing-buckets-mask additions-to-new-buckets-mask original-contents-mask))
                                                  (changes-to-existing-mask (logior additions-to-existing-buckets-mask all-removals-mask)))
                                              (declare (type fixnum current-read-index current-read-mask positive-contents-mask changes-to-existing-mask sub-shift))
                                              (loop while (<= current-read-mask positive-contents-mask) do
                                                    (cond ((plusp (logand changes-to-existing-mask current-read-mask))
                                                           (let ((additions-r nil)
                                                                 (removals-r nil))
                                                             (loop while (and additions-existing-bucket (eql (caar additions-existing-bucket) current-read-mask)) do
                                                                   (let ((next (cdr additions-existing-bucket)))
                                                                     (setf (cdr additions-existing-bucket) additions-r)
                                                                     (setq additions-r additions-existing-bucket)
                                                                     (setq additions-existing-bucket next)))
                                                             (loop while (and hash-key-pairs-to-remove-r (eql (caar hash-key-pairs-to-remove-r) current-read-mask)) do
                                                                   (let ((next (cdr hash-key-pairs-to-remove-r)))
                                                                     (setf (cdr hash-key-pairs-to-remove-r) removals-r)
                                                                     (setq removals-r hash-key-pairs-to-remove-r)
                                                                     (setq hash-key-pairs-to-remove-r next)))
                                                             (let (; (node-if-single (when (not removals-r) (if additions-r (cdar additions-r) (aref old-lookup current-read-index))))
                                                                   (result (with-removals-and-additions (aref old-lookup current-read-index) removals-r (nreverse additions-r) sub-shift)))
                                                               (incf current-read-index)
                                                               (sink-result current-read-mask result node-if-single))))
                                                          ((plusp (logand current-read-mask additions-to-new-buckets-mask))
                                                           (let ((additions-r nil))
                                                             (loop while (and additions-new-bucket (eql (caar additions-new-bucket) current-read-mask)) do
                                                                   (let ((next (cdr additions-new-bucket)))
                                                                     (setf (cdr additions-new-bucket) additions-r)
                                                                     (setq additions-r additions-new-bucket)
                                                                     (setq additions-new-bucket next)))
                                                             (let (; (node-if-single (when additions-r (cdar additions-r)))
                                                                   (result (from-hash-leaf-nodes (nreverse additions-r) sub-shift)))
                                                               (sink-result current-read-mask result node-if-single))))
                                                          ((plusp (logand current-read-mask original-contents-mask))
                                                           (sink-result current-read-mask (aref old-lookup current-read-index))
                                                           (incf current-read-index))) ; no change to count.
                                                    (setf current-read-mask (ash current-read-mask 1)))
                                              (let nil ,@body))))
                                (if (zerop (logand all-removals-mask (lognot additions-to-existing-buckets-mask))) ; no removals without subsequent additions - deterministic array size.
                                    (let ((lookup-array-length (logcount (logior original-contents-mask additions-to-new-buckets-mask))))
                                      (declare (type fixnum lookup-array-length))
                                      (let ((lookup (make-array lookup-array-length))
                                            (current-insertion-index 0)
                                            ; (outer-node-if-single nil)
                                            (count 0))
                                        (declare (type fixnum current-insertion-index count))
                                        (macrolet ((sink-result (insertion-mask result &optional (node-if-single-form nil))
                                                     (declare (ignore node-if-single-form insertion-mask))
                                                     `(progn
                                                        ; (when ,node-if-single (setq outer-node-if-single ,node-if-single))
                                                        (incf count (get-node-element-count (setf (aref lookup current-insertion-index) ,result)))
                                                        (incf current-insertion-index))))
                                          (get-changed-bucket
                                           (if (and (= count 1) (typep (aref lookup 0) 'hash-leaf-node))
                                               (aref lookup 0) ; only one under this bucket, and it's a hash-leaf-node - just return it.
                                             (new-hash-bucket-node count positive-contents-mask lookup))))))
                                  (let ((lookup-elements-r nil)
                                        (lookup-element-count 0)
                                        (final-contents-mask 0)
                                        ; (outer-node-if-single nil)
                                        (count 0))
                                    (declare (type fixnum count lookup-element-count final-contents-mask))
                                    (macrolet ((sink-result (insertion-mask result &optional (node-if-single-form nil))
                                                 (declare (ignore node-if-single-form))
                                                 `(progn
                                                    (let ((result ,result))
                                                      (when result ; nil = don't add to the array
                                                        ; (when ,node-if-single (setq outer-node-if-single ,node-if-single))
                                                        (incf count (get-node-element-count result))
                                                        (incf lookup-element-count)
                                                        (incf final-contents-mask ,insertion-mask)
                                                        (push result lookup-elements-r))))))
                                      (get-changed-bucket
                                       (if lookup-elements-r ; return nil when empty - element above will accept it
                                           (if (and (= count 1) (typep (first lookup-elements-r) 'hash-leaf-node))
                                               (first lookup-elements-r)
                                             (new-hash-bucket-node count final-contents-mask (make-array lookup-element-count :initial-contents (nreverse lookup-elements-r))))
                                         (progn
                                           (assert (zerop count))
                                           nil))))))))))))))))
        (new-const-hash-table
         hash-table-description
         (with-removals-and-additions
          root-node
          (let ((keys-to-remove-r nil))
            (loop-over/ key-to-remove (to-lazy-list keys-to-remove)
                        (push (list* nil (funcall hash-function key-to-remove) key-to-remove) keys-to-remove-r))
            (nreverse keys-to-remove-r))
          (let ((key-value-pairs-to-add-r nil))
            (loop-over/ key-value-pair-to-add (to-lazy-list key-value-pairs-to-add)
                        (push (list* nil (make-hash-leaf-node :hash (funcall hash-function (car key-value-pair-to-add)) :key-value key-value-pair-to-add)) key-value-pairs-to-add-r))
            (nreverse key-value-pairs-to-add-r))
          0))))))


(defun const-hash-table-with-additions (const-hash-table key-value-pairs-to-add)
  (const-hash-table-with-changes const-hash-table nil key-value-pairs-to-add))
(define-compiler-macro const-hash-table-with-additions (const-hash-table key-value-pairs-to-add)
  `(const-hash-table-with-changes ,const-hash-table nil ,key-value-pairs-to-add))
(defun const-hash-table-with-addition (const-hash-table key value)
  (const-hash-table-with-changes const-hash-table nil (list (cons key value))))
(define-compiler-macro const-hash-table-with-addition (const-hash-table key value)
  `(const-hash-table-with-changes ,const-hash-table nil (list (cons ,key ,value))))


(defun const-hash-table-with-removals (const-hash-table keys-to-remove)
  (const-hash-table-with-changes const-hash-table keys-to-remove nil))
(define-compiler-macro const-hash-table-with-removals (const-hash-table keys-to-remove)
  `(const-hash-table-with-changes ,const-hash-table ,keys-to-remove nil))
(defun const-hash-table-with-removal (const-hash-table key)
  (const-hash-table-with-changes const-hash-table (list key) nil))
(define-compiler-macro const-hash-table-with-removal (const-hash-table key)
  `(const-hash-table-with-changes ,const-hash-table (list ,key) nil))


(defun const-hash-table-key-value-pairs (const-hash-table)
  (labels ((get-key-value-pairs (node)
             (etypecase node
               (hash-bucket-node (concat/ (map/ #'get-key-value-pairs (hash-bucket-node-lookup node))))
               (hash-leaf-node (list/ (hash-leaf-node-key-value node)))
               (cons (list/ node))
               (null nil)
               (equal-hash-key-value-pairs
                (lazy-list-from-call
                 (standard-traversal-link
                  (build (end-call remaining-key-value-pairs)
                         (if (consp remaining-key-value-pairs)
                             (traversal-result
                              (car remaining-key-value-pairs)
                              (build end-call (cdr remaining-key-value-pairs)))
                           (get-traversal-result end-call)))
                  ((equal-hash-key-value-pairs-key-value-pairs node))))))))
    (let ((root-node (const-hash-table-root-node const-hash-table)))
      (if root-node
          (get-key-value-pairs root-node)
        (list/)))))

(defmethod make-load-form ((chs const-hash-table) &optional environment)
  (declare (ignore environment))
  (let ((description (const-hash-table-hash-table-description chs)))
    `(make-const-hash-table :hash-function ',(hash-table-description-hash-function description) :test ',(hash-table-description-test description)
                            :key-value-pairs ',(to-list (const-hash-table-key-value-pairs chs)))))

(defun const-hash-table-keys (const-hash-table)
  (map/ #'car (const-hash-table-key-value-pairs const-hash-table)))
(define-compiler-macro const-hash-table-keys (const-hash-table)
  `(map/ #'car (const-hash-table-key-value-pairs ,const-hash-table)))

(defun const-hash-table-values (const-hash-table)
  (map/ #'cdr (const-hash-table-key-value-pairs const-hash-table)))
(define-compiler-macro const-hash-table-values (const-hash-table)
  `(map/ #'cdr (const-hash-table-key-value-pairs ,const-hash-table)))





(defparameter **unit-tests**
  `(
    ("To-from-list"
     (
      ("Proper List" (let ((lista (loop for elt from 1 to 10 collect elt))
                           (listb (loop for elt from 1 to 10 collect elt)))
                       (values (and (equal (to-list (to-lazy-list lista)) listb) (equal lista listb)))))

      ("Array" (let ((arraya (make-array 10 :initial-contents (loop for elt from 1 to 10 collect elt)))
                     (arrayb (make-array 10 :initial-contents (loop for elt from 1 to 10 collect elt))))
                 (values (and (equal (to-list (to-lazy-list arraya)) (map 'list #'identity arrayb)) (equalp arraya arrayb)))))))

    ("Standard operations"
     ,(apply #'nconc
             (loop for in-type in `(("list" ,#'identity)
                                    ("list-based lazy list" ,#'to-lazy-list)
                                    ("lazy-list eager-value" ,(curried #'map/ #'identity))
                                    ("lazy-list lazy-value" ,(curried #'map/ (lambda (elt) (unresolved elt))))) collect
                   (mapcar
                    (lambda (test)
                      `(,(concatenate 'string (first test) " " (first in-type))
                        ,(second test)))
                    (labels ((transformed (list) `(funcall ,(second in-type) ',list)))
                      (let ((equality-check-sets
                             `(("foldl/" (= (foldl/ #'/ 64 ,(transformed '(4 2 4))) 2))
                               ("foldl1/" (= (foldl1/ #'/ ,(transformed '(64 4 2 8))) 1))
                               ("foldr/" (= (foldr/ #'/ 2 ,(transformed '(8 12 24 4))) 8))
                               ("foldr1/" (= (foldr1/ #'/ ,(transformed '(8 12 24 4))) 4))

                               ("head-tail/ multiple"
                                (equal
                                 (multiple-value-bind (head tail) (head-tail/ ,(transformed '(1 2 3 4))) (list head (to-list tail)))
                                 '(1 (2 3 4))))
                               ("head-tail/ short"
                                (equal
                                 (multiple-value-bind (head tail) (head-tail/ ,(transformed '(1))) (list head (to-list tail)))
                                 '(1 nil)))
                               ("nth/ empty"
                                (equal (nth/ 3 ,(transformed nil)) nil))
                               ("nth/ stocked"
                                (equal (nth/ 3 ,(transformed '(10 20 30 40))) 40))

                               ("assoc/"
                                (equal (assoc/ 3 ,(transformed '((2 . "A") (3 . "B") (4 . "C")))) '(3 . "B")))

                               ("and/ true" (and/ ,(transformed '(1 2 3 4))))
                               ("and/ false" (equal (and/ ,(transformed '(1 2 nil 3 4))) nil))

                               ; ("scanl/" (equal (to-list (scanl/ #'/ 64 ,(transformed '(4 2 4)))) '(64 16 8 2)))
                               ; ("scanl1/" (equal (to-list (scanl1/ #'/ ,(transformed '(64 4 2 8)))) '(64 16 8 1)))
                               ; ("scanr/" (equal (to-list (scanr/ #'/ 2 ,(transformed '(8 12 24 4)))) '(8 1 12 2 2)))
                               ; ("scanr1/" (equal (to-list (scanr1/ #'/ ,(transformed '(8 12 24 2)))) '(8 1 12 2)))
                               ; ("nub/" (equal (to-list (nub/ ,(transformed '(9 8 4 4 1 4 9)))) '(9 8 4 1)))
                               ; ("append/" (equal (to-list (append/ ,(transformed '(1 2 3 4)) ,(transformed '(5 6 7 8)))) '(1 2 3 4 5 6 7 8)))
                               ; ("concat/" (equal (to-list (concat/ ,(transformed '((1 2 3) (4 5 6))))) '(1 2 3 4 5 6)))
                               ("car/" (equal (car/ ,(transformed '(5 6 7 8))) 5))
                               ; ("cdr/" (equal (to-list (cdr/ ,(transformed '(5 6 7 8)))) '(6 7 8)))
                               ; ,@(loop for i from 0 to 10 collect `(,(format nil "nthcdr/ ~S" i) (equal (to-list (nthcdr/ ,i ,(transformed (loop for i from 1 to 8 collect i)))) (nthcdr i (loop for i from 1 to 8 collect i)))))
                               ))
                            (list-checks
                             `(("map/" (map/ #'+ ,(transformed '(1 2 3)) ,(transformed '(4 5 6 7))) '(5 7 9))
                               ("scanl/" (scanl/ #'/ 64 ,(transformed '(4 2 4))) '(64 16 8 2))
                               ("scanl1/" (scanl1/ #'/ ,(transformed '(64 4 2 8))) '(64 16 8 1))
                               ("scanr/" (scanr/ #'/ 2 ,(transformed '(8 12 24 4))) '(8 1 12 2 2))
                               ("scanr1/" (scanr1/ #'/ ,(transformed '(8 12 24 2))) '(8 1 12 2))
                               ("nub/" (nub/ ,(transformed '(9 8 4 4 1 4 9))) '(9 8 4 1))
                               ("append/" (append/ ,(transformed '(1 2 3 4)) ,(transformed '(5 6 7 8))) '(1 2 3 4 5 6 7 8))
                               ("concat/" (concat/ ,(transformed '((1 2 3) (4 5 6)))) '(1 2 3 4 5 6))
                               ("intersperse/ empty-case" (intersperse/ "Intersp" ,(transformed nil)) nil)
                               ("intersperse/ single-case" (intersperse/ "Intersp" ,(transformed '(1))) '(1))
                               ("intersperse/ multiple-case" (intersperse/ "Intersp" ,(transformed '(1 2 3 4))) '(1 "Intersp" 2 "Intersp" 3 "Intersp" 4))
                               ("take-while/ empty-case" (take-while/ #'identity ,(transformed nil)) nil)
                               ("take-while/ single-case inc" (take-while/ #'identity ,(transformed '(1))) '(1))
                               ("take-while/ single-case exc" (take-while/ #'null ,(transformed '(1))) nil)
                               ("take-while/ multiple partial" (take-while/ #'identity ,(transformed '(1 2 nil 3 4))) '(1 2))
                               ("take-while/ multiple end" (take-while/ #'identity ,(transformed '(1 2 3 4 nil))) '(1 2 3 4))
                               ("take-while/ multiple start" (take-while/ #'identity ,(transformed '(nil 1 2 3 4))) nil)
                               ("take-while/ multiple full" (take-while/ #'identity ,(transformed '(1 2 3 4))) '(1 2 3 4))

                               ("drop-while/ empty-case" (drop-while/ #'identity ,(transformed nil)) nil)
                               ("drop-while/ single-case inc" (drop-while/ #'identity ,(transformed '(1))) nil)
                               ("drop-while/ single-case exc" (drop-while/ #'null ,(transformed '(1))) '(1))
                               ("drop-while/ multiple partial" (drop-while/ #'identity ,(transformed '(1 2 nil 3 4))) '(nil 3 4))
                               ("drop-while/ multiple end" (drop-while/ #'identity ,(transformed '(1 2 3 4 nil))) '(nil))
                               ("drop-while/ multiple start" (drop-while/ #'identity ,(transformed '(nil 1 2 3 4))) '(nil 1 2 3 4))
                               ("drop-while/ multiple full" (drop-while/ #'identity ,(transformed '(1 2 3 4))) nil)

                               ("list*/ empty" (list*/ ,(transformed nil)) nil)
                               ("list*/ frontloaded only" (list*/ 1 2 3 4 ,(transformed nil)) '(1 2 3 4))
                               ("list*/ backloaded only" (list*/ ,(transformed '(1 2 3 4))) '(1 2 3 4))
                               ("list*/ mixed" (list*/ 1 2 ,(transformed '(3 4))) '(1 2 3 4))

                               ("tails/" (map/ #'to-list (tails/ ,(transformed '(1 2 3 4)))) '((1 2 3 4) (2 3 4) (3 4) (4) nil))
                               ("tails/ lazy-list path" (to-list (concat/ (filter/ #'non-null/ (tails/ (map/ #'identity ,(transformed '(1 2 3 4))))))) '(1 2 3 4 2 3 4 3 4 4))

                               ("filter/" (filter/ #'identity ,(transformed '(1 2 nil 3 4))) '(1 2 3 4))

                               ; Test makes assumption of final order - not portable/valid ("grouped-by-firsts/" (grouped-by-firsts/ #'eql '((1 . 2) (1 . 3) (2 . 4) (1 . 2))) '((1 2 3 2) (2 4)))
                               ("sort/" (sort/ ,(transformed '(5 3 2 1 9))) '(1 2 3 5 9))
                               ("reverse/" (reverse/ ,(transformed '(8 7 6 4 1))) '(1 4 6 7 8))
                               ("iterate/" (take/ 100 (iterate/ #'1+ 1)) (loop for i from 1 to 100 collect i))
                               ("nthcdr/" (nthcdr/ 3 ,(transformed '(1 2 3 4 5 6))) '(4 5 6))

                               ("cdr/" (cdr/ ,(transformed '(5 6 7 8))) '(6 7 8))

                               ,@(loop for i from 0 to 10 collect
                                       `(,(format nil "nthcdr/ ~S" i) (nthcdr/ ,i ,(transformed (loop for i from 1 to 8 collect i))) (nthcdr ,i (loop for i from 1 to 8 collect i))))
                               ,@(loop for i from 0 to 10 collect
                                       `(,(format nil "take/ ~S" i) (take/ ,i ,(transformed (loop for i from 1 to 8 collect i))) (subseq (loop for i from 1 to 8 collect i) 0 (min ,i 8))))

                               )))
                        (append
                         equality-check-sets
                         (mapcan (lambda (elt)
                                   (destructuring-bind (name lazy-form result-form)
                                       elt
                                     `((,name (equal (to-list ,lazy-form) ,result-form))
                                       (,(concatenate 'string name " concat test")
                                        (equal
                                         (to-list (append/ (append/ ,lazy-form ,lazy-form ,lazy-form) (append/ ,lazy-form ,lazy-form ,lazy-form)))
                                         (append (append ,result-form ,result-form ,result-form) (append ,result-form ,result-form ,result-form))))
                                       (,(concatenate 'string name " hang stuff on end and cdr/ test")
                                        (let ((sub (append (append ,result-form ,result-form ,result-form) (append ,result-form ,result-form ,result-form))))
                                          (if (zerop (length sub))
                                              t
                                            (equal
                                             (to-list (append/ (cdr/ (append/ (append/ ,lazy-form ,lazy-form ,lazy-form) (append/ ,lazy-form ,lazy-form ,lazy-form))) '(1 2 3)))
                                             (append (cdr (append (append ,result-form ,result-form ,result-form) (append ,result-form ,result-form ,result-form))) '(1 2 3)))))))))
                                 list-checks))))))))
    
    ("Multifunctionality Sanity tests"
     (

      ("Cliched Fibonacci to 100"
       (funcall (compile nil (lambda () (= (nth/ 100 (self-ref-list/ fib (list*/ 1 1 (map/ #'+ fib (tail/ fib))))) 573147844013817084101)))))

      #|
      ("Fibonacci capped but taken to 1,000,000 - leak test"
       (nth/ 1000000
             (self-ref-list/ fib (list*/ 1 1 (map/ (curried #'max 10) fib (tail/ fib))))))
|#
      ("CSV Parsing Test (split-down-on-test and others)"
       (let ((csv-file

              "1,2, 3 , I contain \" Quoted, commas, \" you see, 99
g, \"hijk\"lmn
third_line,stuff here"))
         (equalp
          '(("1" "2" " 3 " " I contain \" Quoted, commas, \" you see" " 99") ("g" " \"hijk\"lmn") ("third_line" "stuff here"))
          (to-list
           (map/ (composed #'to-list (curried #'map/ (composed #'to-string (curried #'filter/ (lambda (x) (not (eq x #\return)))) (curried #'map/ #'car))))
                 (map/ (composed
                        (lambda (line) (split-down-on-test/ (curried #'equal '(#\, . nil)) line))
                        (curried #'scanl1/ (lambda (a b) (cons (car b) (if (cdr a) (not (cdr b)) (cdr b)))))
                        (curried #'map/ (lambda (elt) (cons elt (eql elt #\")))))
                       (split-down-on-test/ (rcurried #'member '(#\newline)) csv-file)))))))

      ))



    #|
    ("grouped-by-firsts*"
     ,(let ((pairs-as-lists '(("Brown" "Bill") ("Smith" "Ian") ("Stein" "Fred") ("Brown" "Sarah") ("Brown" "Lance"))))
        (eager
          (multiple-value-bind (result-pairs query)
              (grouped-seconds-by-first/ pairs-as-lists :test 'equal)
            ))
        )
     )
|#
    ))


#|

(defun ttt ()
  (let ((csv-file

         "1,2, 3 , I contain \" Quoted, commas, \" you see, 99
g, \"hijk\"lmn
third_line,stuff here"))
    (equalp
     '(("1" "2" " 3 " " I contain \" Quoted, commas, \" you see" " 99") ("g" " \"hijk\"lmn") ("third_line" "stuff here"))
     (to-list
      (map/ (composed #'to-list (curried #'map/ (composed #'to-string (curried #'filter/ (lambda (x) (not (eq x #\return)))) (curried #'map/ #'car))))
            (map/ (composed
                   (lambda (line) (split-down-on-test/ (curried #'equal '(#\, . nil)) line))
                   (curried #'scanl1/ (lambda (a b) (cons (car b) (if (cdr a) (not (cdr b)) (cdr b)))))
                   (curried #'map/ (lambda (elt) (cons elt (eql elt #\")))))
                  (split-down-on-test/ (rcurried #'member '(#\newline)) csv-file)))))))

(defparameter CCC
  (let ((csv-file
         "1,2, 3 , I contain \" Quoted, commas, \" you see, 99
g, \"hijk\"lmn
third_line,stuff here"))
    (split-down-on-test/ (rcurried #'member '(#\newline #\return)) csv-file)))

(defparameter AAA
  (let ((csv-file

         "1,2, 3 , I contain \" Quoted, commas, \" you see, 99
g, \"hijk\"lmn
third_line,stuff here"))
    (to-list
     (map/ (composed #'to-list (curried #'map/ (composed #'to-string (curried #'map/ #'car))))
           (map/ (composed
                  (lambda (line) (split-down-on-test/ (curried #'equal '(#\, . nil)) line))
                  (curried #'scanl1/ (lambda (a b) (cons (car b) (if (cdr a) (not (cdr b)) (cdr b)))))
                  (curried #'map/ (lambda (elt) (cons elt (eql elt #\")))))
                 (split-down-on-test/ (curried #'eql #\newline) csv-file))))))

(defparameter BBB '(("1" "2" " 3 " " I contain \" Quoted, commas, \" you see" " 99") ("g" " \"hijk\"lmn") ("third_line" "stuff here")))
|#


(defstruct diff-hash-table
  contents
  lock
  )

(defstruct hash-table-delta
  base ; diff-hash-table we're based on
  removals ; keys removed from base
  additions ; keys added or replaced - search this first - alist
  test
  )
  

(defun diff-hash-table (key-value-pairs &key (test #'eql))
  (make-diff-hash-table :lock nil
                   :contents
                   (let ((hash (make-hash-table :test test)))
                     (loop-over/ pair (to-lazy-list key-value-pairs)
                       (setf (gethash (car pair) hash) (cdr pair)))
                     hash)))

(defun diff-hash-table-changed (diff-hash-table key-value-pairs-to-add keys-to-remove)
  (let ((contents (diff-hash-table-contents diff-hash-table)))
    (etypecase contents
      (hash-table
       (let* ((result (make-diff-hash-table :lock nil :contents contents))
              (test (hash-table-test contents))
              (additions nil)
              (removals nil))
         (loop-over/ key (to-lazy-list keys-to-remove)
           (multiple-value-bind (value is-valid)
               (gethash key contents)
             (when is-valid
               (push (cons key value) additions))
             (remhash key contents)))
         (loop-over/ key-value-pair (to-lazy-list key-value-pairs-to-add)
           (destructuring-bind (key . value)
               key-value-pair
             (multiple-value-bind (value is-valid)
                 (gethash key contents)
               (if is-valid
                   (push (cons key value) additions)
                 (push key removals)))
             (setf (gethash key contents) value)))
         (setf (diff-hash-table-contents diff-hash-table) (make-hash-table-delta :base result :removals removals :additions additions :test test))
         result))
      (hash-table-delta
       (make-diff-hash-table :lock nil :contents (make-hash-table-delta :base diff-hash-table :removals (to-list keys-to-remove) :additions (to-list key-value-pairs-to-add) :test (hash-table-delta-test contents)))))))

(defun diff-hash-table-with-additions (diff-hash-table key-value-pair-additions)
  (diff-hash-table-changed diff-hash-table key-value-pair-additions nil))

(defun diff-hash-table-with-addition (diff-hash-table key value)
  (diff-hash-table-changed diff-hash-table (list (cons key value)) nil))


(defun diff-hash-table-with-removals (diff-hash-table keys-to-remove)
  (diff-hash-table-changed diff-hash-table nil keys-to-remove))

(defun diff-hash-table-with-removal (diff-hash-table key-to-remove)
  (diff-hash-table-changed diff-hash-table nil (list key-to-remove)))


(defun get-diff-hash (key diff-hash-table)
  (let ((contents (diff-hash-table-contents diff-hash-table)))
    (typecase contents
      (hash-table
       (gethash key contents))
      (hash-table-delta
       (let* ((test (hash-table-delta-test contents))
              (from-additions (assoc key (hash-table-delta-additions contents) :test test)))
         (if from-additions
             (values (cdr from-additions) t)
           (unless (position key (hash-table-delta-removals contents))
             (let ((base (hash-table-delta-base contents)))
               (unresolved (get-diff-hash key base))))))))))

(defun unit-test ()
  (loop for named-test in **unit-tests** do
        (destructuring-bind (identity sub-test-list)
            named-test
          (format t "Testing ~A" (string identity))
          (terpri)
          (force-output)
          (loop for laziness in '(eager lazy) do
                (loop for sub-test in sub-test-list for sub-test-number from 1 do
                      (loop for compiled in '(nil t) do
                            (destructuring-bind (sub-test-identity test)
                                (if (> (length sub-test) 1)
                                    sub-test
                                  (list (format nil "~A: " sub-test-number) (car sub-test)))
;                  (format t "    ~A: " sub-test-identity)
;                  (force-output)
                              (let ((identity-string (format nil "    ~A in ~A mode, ~A: " sub-test-identity laziness (if compiled "compiled" "not compiled"))))
                                (handler-case
                                    (multiple-value-bind (passed info)
                                        (if compiled
                                            (eval `(funcall (compile nil (lambda () (,laziness ,test)))))
                                          (eval `(,laziness ,test)))
                                      (if passed
                                          nil ;(format t (concatenate 'string identity-string " passed"))
                                        (progn (format t (concatenate 'string identity-string (format nil "!!!failed!!!") (when info (format nil " ~A" info)))) (terpri)))
                                      (force-output))
                                  (error (err)
                                    (format t (concatenate 'string identity-string "failed hard ~A ~S") err err)
                                    (terpri)
                                    (force-output)))))
                            (force-output))))
          (terpri)
          (force-output))))

; Don't unit-test by default (unit-test)



