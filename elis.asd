(defsystem "elis"
  :serial t
  :components ((:file "conditions")
               (:file "parser")
               (:file "executor")
               (:file "elis"))
  :in-order-to ((test-op (test-op "elis/tests"))))

(defsystem "elis/tests"
  :depends-on ("rove"
               "elis")
  :pathname "tests"
  :components ((:file "parser"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
