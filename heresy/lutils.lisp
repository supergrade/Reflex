
(in-package :heresy)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro with-g-symbols (&body body)
  (let ((syms (remove-duplicates (remove-if-not #'g!-symbol-p (flatten body)))))
    `(let ,(mapcar (lambda (s) `(,s (gensym ,(subseq (symbol-name s) 2)))) syms)
       ,@body)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq (symbol-name s) 2))))
              syms)
         ,@body))))


(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))



(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (if (and os gs)
        `(defmacro/g! ,name ,args
                      `(let ,(mapcar #'list (list ,@gs) (list ,@os))
                         ,(progn ,@body)))
      `(defmacro/g! ,name ,args ,@body))))
#+lispworks (editor:setup-indent "defmacro!" 2 2 7)


(defmacro! macrolet! (bindings &rest body)
  `(macrolet
       ,(mapcar
         (lambda (binding)
           (destructuring-bind (name args &rest body)
               binding
             (let* ((os (remove-if-not #'o!-symbol-p args))
                    (gs (mapcar #'o!-symbol-to-g!-symbol os)))
               (let ((syms (remove-duplicates
                            (remove-if-not #'g!-symbol-p (flatten body)))))
                 `(,name ,args
                         (let ,(mapcar
                                (lambda (s)
                                  `(,s (gensym ,(subseq (symbol-name s) 2))))
                                syms)
                           `(let ,(mapcar #'list (list ,@gs) (list ,@os))
                              ,(progn ,@body))))))))
         bindings)
     ,@body))
#+lispworks (editor:setup-indent "macrolet!" 1 nil nil 'flet)



(defmacro! symbol-macrolet! (bindings &rest body)
  `(symbol-macrolet
       ,(mapcar
         (lambda (binding)
           (destructuring-bind (name body)
               binding
               (let ((syms (remove-duplicates
                            (remove-if-not #'g!-symbol-p (flatten body)))))
                 `(,name
                         (let ,(mapcar
                                (lambda (s)
                                  `(,s (gensym ,(subseq (symbol-name s) 2))))
                                syms)
                           ,body)))))
         bindings)
     ,@body))
#+lispworks (editor:setup-indent "symbol-macrolet!" 1)

)