(in-package :cl-user)

(defpackage :dotre.primitives
  (:nicknames :prim)
  (:shadow #:copy-seq)
  (:use :cl :dotre.character-range :dotre.character-class )
  (:export #:item
           #:repeat
           #:alt
           #:cut
           #:seq
           #:guard
           #:run-pattern))

(in-package :dotre.primitives)

(defstruct lexeme)

(defstruct (item (:include lexeme)))

(defstruct (guard (:include lexeme))
  (lexeme (make-lexeme) :type lexeme)
  (class (make-character-class) :type character-class))

(defstruct (repeat (:include lexeme))
  (lexeme (make-item) :type lexeme)
  (lower nil :type (or null unsigned-byte))
  (upper nil :type (or null unsigned-byte)))

(defstruct (alt (:include lexeme))
  (first (make-item) :type lexeme)
  (second (make-item) :type lexeme))

(defstruct (cut (:include lexeme))
  (first (make-item) :type lexeme)
  (second (make-item) :type lexeme))

(defstruct (seq (:include lexeme))
  (first (make-item) :type lexeme)
  (second (make-item) :type lexeme))


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

(defun run-pattern (lexeme inp)
  (typecase lexeme
    (guard
     (with-slots (lexeme class) lexeme
       (let ((char (peek-char nil inp nil)))
         (if (and char (class-member (char-code char) class)) (run-pattern lexeme inp) 0))))
    (item
     (if (peek-char nil inp nil) 1  0))
    (alt
     (with-slots (first second) lexeme
       (max (run-pattern first inp)
            (run-pattern second inp))))
    (repeat
     (with-slots (lexeme lower upper) lexeme
       (labels ((recur (repetition char-count)
                  (if (and upper (>= repetition upper)) char-count
                      (let ((n (run-pattern lexeme inp)))
                        (if (zerop n)
                            (if (or (null lower) (and lower (<= lower repetition))) char-count 0)
                            (with-chars-held (n inp)
                              (recur (1+ repetition) (+ n char-count))))))))
         (recur 0 0))))
    (cut
     (with-slots (first second) lexeme
       (let ((n (run-pattern first inp)))
         (if (zerop n) (run-pattern second inp) n))))

    (seq
     (with-slots (first second) lexeme
       (let ((m (run-pattern first inp)))
         (if (zerop m) 0
             (with-chars-held (m inp)
               (let ((n (run-pattern second inp)))
                 (if (zerop n) 0 (+ m n))))))))))


(declaim (ftype (function () guard) item))
(defun item ()
  "Return an item that matches any character"
  (make-guard
   :lexeme (make-item)
   :class (make-character-class (range 0 char-code-limit))))

(deftype optional-unsigned-byte ()
  `(or null unsigned-byte))

(declaim
 (ftype
  (function (guard &key (:lower optional-unsigned-byte) (:upper optional-unsigned-byte)) guard)
  repeat))

(defun repeat (lexeme &key lower upper)
  "Return a pattern matching a number of repetitions of `LEXEME'.

The number is determined by `LOWER' and `UPPER'. If `LOWER'
specifies the inclusive minimum number of matches, and if `LOWER' is
`NIL' the minimum number of matches is 0. `UPPER' specifies the
inclusive maximum number of matches, and if `UPPER' is `NIL', the
number is unbounded."
  (make-guard
   :lexeme (make-repeat :lexeme lexeme :lower lower :upper upper)
   ;; Note: The guard is used even if `LOWER' is nil or 0, because
   ;; matching 0 is equivalent to failing.
   :class (guard-class lexeme)))


(declaim (ftype (function (guard character-class) guard) guard))

(defun guard (lexeme class)
  "Return a pattern with an additional constraint in character class."
  (make-guard
   :lexeme (guard-lexeme lexeme)
   :class (class-intersection (guard-class lexeme) class)))

(declaim (ftype (function (guard &rest guard) guard) alt cut seq))

(defun alt (lexeme &rest lexemes)
  "Return a pattern that matches any of the input patterns."
  (if (null lexemes) lexeme
      (let* ((second (apply #'alt lexemes))
             (class (class-union (guard-class lexeme) (guard-class second))))
        (make-guard
         :lexeme (make-alt
                  :first lexeme
                  :second second)
         :class class))))

(defun cut (lexeme &rest lexemes)
  "Return a pattern that matches when `LEXEMES' matches, or if it fails, when
the pattern formed from `LEXEMES' matches."
  (if (null lexemes) lexeme
      (let ((lexemes (apply #'cut lexemes)))
        (make-guard
         :lexeme (make-cut
                  :first lexeme
                  :second lexemes)
         :class (class-union (guard-class lexeme) (guard-class lexemes))))))

(defun seq (lexeme &rest lexemes)
  "Return a pattern that matches when each of the input patterns match
sequentially."
  (if (null lexemes) lexeme
      (make-guard
       :lexeme (make-seq
                :first (guard-lexeme lexeme)
                :second (apply #'seq lexemes))
       :class (guard-class lexeme))))
