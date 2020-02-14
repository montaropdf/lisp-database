(defsystem "lisp-database"
  :version "0.1.0"
  :author "Roland Everaert"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "query")
                 (:file "reader")
                 (:file "recorder")
                 (:file "manager"))))
  :description "Lisp Database Format Manager."
  :in-order-to ((test-op (test-op "lisp-database/tests"))))

(defsystem "lisp-database/tests"
  :author "Roland Everaert"
  :license ""
  :depends-on ("lisp-database"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lisp-database"
  :perform (test-op (op c) (symbol-call :rove :run c)))
