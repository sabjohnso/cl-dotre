(in-package :cl-user)


(defpackage :dotre.utility
  (:use :cl)
  (:export #:optional-unsigned-byte #:read-chars #:unread-chars))

(in-package :dotre.utility)

(deftype optional-unsigned-byte ()
  `(or null unsigned-byte))

(defun read-chars (n inp)
  (loop for i from 1 to n
        collecting (read-char inp)))

(defun unread-chars (chars inp)
  (loop for char in chars
        do (unread-char char inp)))
