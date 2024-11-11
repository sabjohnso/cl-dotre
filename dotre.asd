(in-package :cl-user)

(defpackage :dotre-system
  (:use :cl :asdf :uiop))

(in-package :dotre-system)

(defsystem :dotre
  :version "0.1.0"
  :description "A lexical analyzer combinator library"
  :author "Samuel B. Johnson"
  :license "MIT"
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "character-range")
     (:file "character-class")
     (:file "primitives")
     (:file "dotre"))))
  :in-order-to ((test-op
                 (load-op :dotre)
                 (test-op :dotre/test))))


(defsystem :dotre/test
  :description "Tests for the `DOTRE' system"
  :depends-on (:fiveam :dotre)
  :components
  ((:module "test"
    :components
    ((:file "character-range-test")
     (:file "character-class-test")
     (:file "primitives-test")
     (:file "dotre-test"))))
  :perform (test-op (o s)
             (symbol-call :dotre/test.character-range :run-all-tests!)
             (symbol-call :dotre/test.character-class :run-all-tests!)
             (symbol-call :dotre/test.primitives      :run-all-tests!)
             (symbol-call :dotre.test                 :run-all-tests!)))
