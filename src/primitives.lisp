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

(defun rappend (xs ys)
  (labels ((recur (xs ys)
             (if (null xs) ys
                 (recur (cdr xs) (cons (car xs) ys)))))
    (recur xs ys)))

(defstruct lexeme)

(defstruct (pattern (:include lexeme)))

(deftype pattern-list ()
  '(cons pattern list))

(defstruct (item (:include pattern)))

(defstruct (guard (:include pattern))
  (pattern (make-item) :type pattern)
  (class (make-character-class (range 0 char-code-limit)) :type character-class))

(defstruct (repeat (:include pattern))
  (lexeme (make-item) :type pattern)
  (lower nil :type (or null unsigned-byte))
  (upper nil :type (or null unsigned-byte)))

(defstruct (alt (:include pattern))
  (patterns (list (make-item)) :type pattern-list))

(defstruct (cut (:include pattern))
  (patterns (list (make-item)) :type pattern-list))

(defstruct (seq (:include pattern))
  (patterns (list (make-item)) :type pattern-list))




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

(declaim (ftype (function (pattern stream) (or null unsigned-byte))
                run-pattern
                run-seq
                run-guard
                run-item
                run-alt
                run-cut))


(defun run-pattern (lexeme inp)
  (etypecase lexeme
    (guard  (run-guard  lexeme inp))
    (item   (run-item   lexeme inp))
    (alt    (run-alt    lexeme inp))
    (cut    (run-cut    lexeme inp))
    (repeat (run-repeat lexeme inp))
    (seq    (run-seq    lexeme inp))))

(defun run-guard (pattern inp)
  "Run a `GUARD' pattern over an input stream, returning
the number of characters matched or `NIL' for failure."
  (with-slots (pattern class) pattern
    (let ((char (peek-char nil inp nil)))
      (if char  (and (class-member (char-code char) class)
                     (run-pattern pattern inp))
          (run-pattern pattern inp)))))

(defun run-item (pattern inp)
  "Run an `ITEM' pattern over an input stream, return 1 if
the stream has remaining characters and `NIL' for the
end of the stream."
  (declare (ignore pattern))
  (if (peek-char nil inp nil) 1  nil))


(defun run-repeat (pattern inp)
  "Run a `REPEAT' pattern over an input stream, returning the
number of characters matched or `NIL' for failure."
  (with-slots (lexeme lower upper) pattern
    (loop for n = (run-pattern lexeme inp)
          for m = (if n 1 0) then (if n (1+ m) m)
          when n
                sum n into accum
                and append (read-chars n inp) into accum-chars
                and when (and upper (>= m upper))
                      do (progn
                           (unread-chars (reverse accum-chars) inp)
                           (return accum))
          when (not n)
            do (progn
                 (unread-chars (reverse accum-chars) inp)
                 (return (and (or (null lower) (>= m lower)) accum))))))

(defun run-alt (pattern inp)
  "Run an `ALT' pattern over an input stream, returning the
number of characters matched or `NIL' for failure."

  (with-slots (patterns) pattern
    (loop for pattern in patterns
          for n = (run-pattern pattern inp)
          when n maximize n)))

(defun run-cut (pattern inp)
  "Run a `CUT' pattern over an input stream, returning the
number of characters matched or `NIL' for failure."

  (with-slots (patterns) pattern
    (loop for pattern in patterns
          for n = (run-pattern pattern inp)
          when n do (return n))))

(defun run-seq (pattern inp)
  "Run a `SEQ' pattern over an input stream, returning the
number of characters matched or `NIL' for failure."

  (with-slots (patterns) pattern
    (loop for pattern in patterns
          for m = (run-pattern pattern inp)
          for chars = (and m (read-chars m inp))
          when m
            sum m into accum
            and append chars into accum-chars
          else
            do (progn
                 (unread-chars (reverse accum-chars) inp)
                 (return nil))
          finally
             (progn
               (unread-chars (reverse accum-chars) inp)
               (return accum)))))




(declaim (ftype (function () guard) item))
(defun item ()
  "Return an item that matches any character"
  (make-guard
   :pattern (make-item)
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

  ;; Note: When `LOWER' is `NIL' or zero, the guard needs to pass through
  ;; any character because matching zero characters is a success.  Otherwise,
  ;; the guard from the repeated pattern is used.
  (make-guard
   :pattern (make-repeat :lexeme lexeme :lower lower :upper upper)

   :class (if (or (null lower) (zerop lower))
              (make-character-class (range 0 char-code-limit))
              (guard-class lexeme))))


(declaim (ftype (function (guard character-class) guard) guard))

(defun guard (pattern class)
  "Return a pattern with an additional constraint in character class."
  (make-guard
   :pattern (guard-pattern pattern)
   :class (class-intersection (guard-class pattern) class)))

(declaim (ftype (function (guard &rest guard) guard) alt cut seq))

(defun alt (lexeme &rest lexemes)
  "Return a pattern that matches any of the input patterns."
  (if (null lexemes) lexeme
      (let ((class (apply #'class-union (guard-class lexeme) (mapcar #'guard-class lexemes))))
        (make-guard
         :pattern (make-alt :patterns (cons lexeme lexemes))
         :class class))))

(defun cut (lexeme &rest lexemes)
  "Return a pattern that matches when `LEXEMES' matches, or if it fails, when
the pattern formed from `LEXEMES' matches."

  (if (null lexemes) lexeme
      (let  ((class (apply #'class-union (guard-class lexeme) (mapcar #'guard-class lexemes))))
        ;; ((lexemes (apply #'cut lexemes)))
        (make-guard
         :pattern (make-cut :patterns (cons lexeme lexemes))
         :class class))))

(defun seq (lexeme &rest lexemes)
  "Return a pattern that matches when each of the input patterns match
sequentially."
  (if (null lexemes) lexeme
      (make-guard
       :pattern (make-seq
                :patterns (cons lexeme lexemes))
       :class (guard-class lexeme))))
