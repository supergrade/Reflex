;;; -*- Lisp -*-
(asdf:defsystem test2b2
    :version "1.0"
    :components ((:file "file2" :in-order-to ((compile-op (load-op "file1"))))
                 (:file "file1"))
    :in-order-to ((load-op (load-op (:version test2a "1.2")))))


