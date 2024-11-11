(in-package :cl-user)

(defpackage :dotre.test
  (:use :cl :5am :dotre))

(in-package :dotre.test)

(def-suite dotre)

(in-suite dotre)

(defun run-all-tests! ()
  "Run the tests in the `DOTRE` test suite"
  (run! 'dotre))
