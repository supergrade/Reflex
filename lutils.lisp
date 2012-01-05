

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(compile 'flatten)

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))
(compile 'g!-symbol-p)

(defmacro make-a-gensym (name) `(gensym ,name))
(defmacro with-g-symbols (&body body)
  (let ((syms (remove-duplicates (remove-if-not #'g!-symbol-p (flatten body)))))
    `(let ,(mapcar (lambda (s) `(,s (make-a-gensym ,(subseq (symbol-name s) 2)))) syms)
       ,@body)))
(compile 'with-g-symbols)

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq (symbol-name s) 2))))
              syms)
         ,@(when syms `((declare (ignorable ,@syms))))
         ,@body))))
(compile 'defmacro/g!)

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
(compile 'mkstr)

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
                         ,(locally ,@body)))
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
                           ,@(when syms `((declare (ignorable ,@syms))))
                           `(let ,(mapcar #'list (list ,@gs) (list ,@os))
                              ,@(when ,gs `((declare (ignorable ,(list ,@gs)))))
                              ,(locally ,@body))))))))
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



(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  (let ((syms (loop for i from 1 to numarg
                  collect (symb 'a i))))
   `(lambda ,syms
      (declare (ignorable ,@syms))
      ,(funcall
        (get-macro-character #\`) stream nil))))

(set-dispatch-macro-character
  #\# #\` #'|#`-reader|)
