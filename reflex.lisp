


(defvar **prog-lisp-dir** "c:/prog/reflex/")

(defun prog-lisp-dir (dir)
  (concatenate 'string **prog-lisp-dir** dir))

(load (concatenate 'string (prog-lisp-dir "asdf/") "asdf.lisp"))

(defun pl (path &optional sym)
  (push (prog-lisp-dir path) asdf:*central-registry*)
  ; (asdf:operate 'asdf:load-op sym)
  )
(defun ap (package) (asdf:operate 'asdf:load-op package))

(defmacro load-lutils ()
  `(load (prog-lisp-dir "lutils.lisp")))


(pl "hunchentoot/" :hunchentoot)
(pl "bordeaux-threads/" :bordeaux-threads)
(pl "Alexandria/" :alexandria)
(pl "usocket/" :usocket)
(pl "trivial-backtrace/" :trivial-backtrace)
(pl "rfc2388/" :rfc2388)
(pl "md5-1.8.5/" :md5)
(pl "cl-plus-ssl-cl-plus-ssl/" :cl+ssl)
(pl "trivial-garbage_0.19/" :trivial-garbage)
(pl "flexi-streams-1.0.7/" :flexi-streams)
(pl "trivial-gray-streams-2006-09-16/" :trivial-gray-streams)
(pl "cffi_0.10.5/" :cffi)
(pl "babel_0.3.0/" :babel)
(pl "trivial-features_0.6/" :trivial-features)
(pl "cl-ppcre-2.0.3/" :cl-ppcre)
(pl "cl-fad-0.6.4/" :cl-fad)
(pl "cl-base64-3.3.3/" :cl-base64)
(pl "chunga-1.1.1/" :chunga)
(pl "heresy/" :heresy)
(pl "drakma-1.2.3/" '#:drakma)
(pl "puri-1.5.1/" 'puri)

(ap :hunchentoot)
(ap :heresy)
(ap :drakma)


(use-package '(:hunchentoot :heresy))

(defvar server-already-running nil)
(defparameter **port** 82)


(defparameter **output-stream** *standard-output*)

(defparameter **reflex-hash-lock** (bordeaux-threads:make-lock))
(defparameter **name-to-reflex-entry** (make-hash-table :test #'equal)) 

(defun handle-reflex-request (request)
  (let* ((request-uri (request-uri*))
         (url-split (to-list (map/ #'to-string (filter/ #'non-null/ (split-down-on-test/ (curried #'eql #\/) request-uri))))))
    (when (equal (first url-split) "reflex")
      (destructuring-bind (name command &rest rest) (cdr url-split)
        (cond
          ((equal command "read")
           (lambda (&rest rest)
             (let ((stream (send-headers)))
               (let ((entry (bordeaux-threads:with-lock-held (**reflex-hash-lock**) (gethash name **name-to-reflex-entry**))))
                 (when entry
                   (let ((entries-rlist  (bordeaux-threads:with-lock-held ((getf entry :lock)) (getf entry :entries-rlist)))) ; lock only to take snapshot
                     (loop for entry in entries-rlist do
                       (progn
                         (write-sequence (map 'list #'char-code (concatenate 'string (getf entry :body))) stream)
                         (write-sequence (map 'list #'char-code "<div/>") stream)
                         ))))))))
          ((equal command "write")
           (let ((found (bordeaux-threads:with-lock-held (**reflex-hash-lock**) (or (gethash name **name-to-reflex-entry**) (setf (gethash name **name-to-reflex-entry**) `(:entries-rlist ,nil :lock ,(bordeaux-threads:make-lock)))))))
             (symbol-macrolet ((entries-rlist (getf found :entries-rlist)))
               (let* ((body
                        (map
                         'string
                         (lambda (c) (if (characterp c) c (code-char c)))
                         (hunchentoot:raw-post-data :request hunchentoot:*request* :external-format nil :force-text nil :force-binary nil :want-stream nil))))
                 (bordeaux-threads:with-lock-held ((getf found :lock)) (push `(:body ,body) entries-rlist))
                 (lambda (&rest rest)
                   (let ((stream (send-headers)))
                     (loop for c across body do (write-byte (char-code c) stream))
                     (finish-output stream))))))))))))

;; Start server
(defun start-server ()
  (when (not server-already-running)
    (setf server-already-running (make-instance 'easy-acceptor :port **port**))

    (setf (slot-value server-already-running 'hunchentoot::access-log-destination) nil)
    (setf (slot-value server-already-running 'hunchentoot::message-log-destination) nil)
    
    (setf *dispatch-table*
          (append
           (list
            (lambda (request) (handle-reflex-request request)))

           (list
                                        ; seems it was removed #'default-dispatcher
            )))

    (start server-already-running)))



(defparameter **die** nil)

(defparameter **errors** 0)
; (defparameter **error-list** nil) populating this indefiniteley is basically a leak.
(defparameter **successes** 0)

;; Start client
(defun start-stress-test ()
  (loop for thread from 1 to 100 do
    (let ((thread thread))
      (sleep 0.01)
      (bordeaux-threads:make-thread
       (lambda ()
         (loop for i from 1
               until **die** do
                 (progn
                   (handler-case
                       (unless **die**
                         (close (drakma:http-request (format nil "http://127.0.0.1:~A" **port**) :method :get :want-stream t))
                         (incf **successes**))
                     (error (err)
                       ; effectively a leak populating this (push (format nil "Thread ~A Send ~A failed with ~S ~A" thread i err err) **error-list**)
                       (incf **errors**)
                       )))))
       :name (format nil "Stress Test ~A" thread)))))


#|
               (defun run ()
(start-server)
(start-stress-test))
               |#

(defun get-progress ()
  (format nil "Successes: ~A, Errors: ~A" **successes** **errors**))

(defun show-progress ()
  (print (get-progress)))


; (loop for i from 0 do (progn (print i) (sleep 1)))




