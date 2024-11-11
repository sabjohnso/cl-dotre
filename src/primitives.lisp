(in-package :cl-user)

(defpackage :dotre.primitives
  (:nicknames :prim)
  (:use :cl :dotre.character-class)
  (:export #:item #:make-item
           #:repeat #:make-repeat
           #:alt #:make-alt
           #:cut #:make-cut
           #:seq #:make-seq
           #:guard #:make-guard
           #:run-lexeme))

(in-package :dotre.primitives)

(defstruct lexeme)

(defstruct (guard (:include lexeme))
  (lexeme (make-lexeme) :type lexeme)
  (class (make-character-class) :type character-class))

(defstruct (item (:include lexeme)))

(defstruct (repeat (:include lexeme))
  (lexeme (make-lexeme) :type lexeme)
  (lower nil :type (or null unsigned-byte))
  (upper nil :type (or null unsigned-byte)))

(defstruct (alt (:include lexeme))
  (first (make-lexeme) :type lexeme)
  (second (make-lexeme) :type lexeme))

(defstruct (cut (:include lexeme))
  (first (make-lexeme) :type lexeme)
  (second (make-lexeme) :type lexeme))


(declaim (ftype (function (lexeme stream) unsigned-byte) primitive-lexeme-run))

(defun unread-chars (chars inp)
  (loop for char in chars
        do (unread-char char inp)))

(defun has-chars (n inp)
  (labels ((recur (m accum)
             (if (>= m n)
                 (progn
                   (unread-chars accum inp)
                   t)
                 (let ((char (peek-char nil inp nil)))
                   (if char
                       (progn
                         (read-char inp)
                         (recur (1+ m) (cons char accum)))
                       (progn
                         (unread-chars accum inp)
                         nil))))))
    (recur 0 nil)))

(defun read-chars (n inp)
  (loop for i from 1 to n
        collecting (read-char inp)))

(defmacro with-chars-held ((n inp) &body body)
  (let ((result (gensym "RESULT"))
        (var (gensym "VAR")))
    `(let ((,var (read-chars ,n ,inp)))
       (let ((,result
               (progn ,@body)))
         (unread-chars ,var ,inp)
         ,result))))

(defun run-lexeme (lexeme inp)
  (typecase lexeme
    (guard
     (with-slots (lexeme class) lexeme
       (let ((char (peek-char nil inp nil)))
         (if (and char (class-member (char-code char) class)) (run-lexeme lexeme inp) 0))))
    (item
     (if (peek-char nil inp nil) 1  0))
    (alt
     (with-slots (first second) lexeme
       (max (run-lexeme first inp)
            (run-lexeme second inp))))
    (repeat
     (with-slots (lexeme lower upper) lexeme
       (labels ((recur (repetition char-count)
                  (if (and upper (>= repetition upper)) char-count
                      (let ((n (run-lexeme lexeme inp)))
                        (if (zerop n)
                            (if (or (null lower) (and lower (<= lower repetition))) char-count 0)
                            (with-chars-held (n inp)
                              (recur (1+ repetition) (+ n char-count))))))))
         (recur 0 0))))
    (cut
     (with-slots (first second) lexeme
       (let ((n (run-lexeme first inp)))
         (if (zerop n) (run-lexeme second inp) n))))

    (seq
     (with-slots (first second) lexeme
       (let ((m (run-lexeme first inp)))
         (if (zerop m) 0
             (with-chars-held (m inp)
               (let ((n (run-lexeme second inp)))
                 (if (zerop n) 0 (+ m n))))))))))
