(in-package :cl-user)

(defpackage :dotre.primitives
  (:nicknames :prim)
  (:shadow #:copy-seq)
  (:use :cl :dotre.utility :dotre.character-range :dotre.character-class)
  (:export #:item
           #:repeat
           #:peek
           #:alt
           #:cut
           #:seq
           #:guard
           #:pattern-p
           #:run-pattern))

(in-package :dotre.primitives)

;;
;; ... Structures for primitive patterns
;;

(defstruct pattern)

(deftype pattern-list ()
  '(cons pattern list))

(defstruct (item (:include pattern)))

(defstruct (peek (:include pattern))
  (pattern (make-item) :type pattern))

(defstruct (guard (:include pattern))
  (pattern (make-item) :type pattern)
  (class (make-character-class (range 0 char-code-limit)) :type character-class))

(defstruct (repeat (:include pattern))
  (pattern (make-item) :type pattern)
  (lower nil :type (or null unsigned-byte))
  (upper nil :type (or null unsigned-byte)))

(defstruct (alt (:include pattern))
  (patterns (list (make-item)) :type pattern-list))

(defstruct (cut (:include pattern))
  (patterns (list (make-item)) :type pattern-list))

(defstruct (seq (:include pattern))
  (patterns (list (make-item)) :type pattern-list))

;;
;; ... Constructors
;;
(declaim (ftype (function (guard character-class) guard) guard))
(declaim (ftype (function (guard &rest guard) guard) alt cut seq))
(declaim (ftype (function () guard) item))
(declaim (ftype (function (guard) guard) peek))
(declaim
 (ftype
  (function (guard &key
              (:lower optional-unsigned-byte)
              (:upper optional-unsigned-byte))
            guard)
  repeat))

(defun item ()
  "Return an item that matches any character"
  (make-guard
   :pattern (make-item)
   :class (make-character-class (range 0 char-code-limit))))

(defun peek (pattern)
  "Return a pattern that succeeds when the input pattern succeeds
and fails when the input pattern fails, but reporting the length of
the match as 0 on success."
  (make-guard
   :pattern (make-peek :pattern (guard-pattern pattern))
   :class (guard-class pattern)))

(defun repeat (pattern &key lower upper)
  "Return a pattern matching a number of repetitions of `PATTERN'.

The number is determined by `LOWER' and `UPPER'. If `LOWER'
specifies the inclusive minimum number of matches, and if `LOWER' is
`NIL' the minimum number of matches is 0. `UPPER' specifies the
inclusive maximum number of matches, and if `UPPER' is `NIL', the
number is unbounded."

  (make-guard
   :pattern (make-repeat :pattern pattern :lower lower :upper upper)
   :class
   ;; Note: When `LOWER' is `NIL' or zero, the guard needs to pass through
   ;; any character because matching zero characters is a success.  Otherwise,
   ;; the guard from the repeated pattern is used.
   (if (or (null lower) (zerop lower))
       (make-character-class (range 0 char-code-limit))
       (guard-class pattern))))

(defun guard (pattern class)
  "Return a pattern with an additional constraint in character class."
  (make-guard
   :pattern (guard-pattern pattern)
   :class (class-intersection (guard-class pattern) class)))

(defun alt (pattern &rest patterns)
  "Return a pattern that matches any of the input patterns."
  (if (null patterns) pattern
      (let ((class (apply #'class-union (guard-class pattern) (mapcar #'guard-class patterns))))
        (make-guard
         :pattern (make-alt :patterns (cons pattern patterns))
         :class class))))

(defun cut (pattern &rest patterns)
  "Return a pattern that matches when one of the input patterns matches.  The length of the match
returned is that of the first pattern to match."

  (if (null patterns) pattern
      (let  ((class (apply #'class-union (guard-class pattern) (mapcar #'guard-class patterns))))
        (make-guard
         :pattern (make-cut :patterns (cons pattern patterns))
         :class class))))

(defun seq (pattern &rest patterns)
  "Return a pattern that matches when each of the input patterns match
sequentially."
  (if (null patterns) pattern
      (make-guard
       :pattern (make-seq
                :patterns (cons pattern patterns))
       :class (guard-class pattern))))

;;
;; ... Pattern execution
;;
(declaim
 (ftype (function (pattern stream) optional-unsigned-byte)
        run-pattern
        run-seq
        run-guard
        run-item
        run-alt
        run-cut))

(defun run-pattern (pattern inp)
  "Run a `PATTERN' over an input stream, returning
the number of characters matched or `NIL' for failure."
  (etypecase pattern
    (guard  (run-guard  pattern inp))
    (item   (run-item   pattern inp))
    (peek   (run-peek   pattern inp))
    (alt    (run-alt    pattern inp))
    (cut    (run-cut    pattern inp))
    (repeat (run-repeat pattern inp))
    (seq    (run-seq    pattern inp))))

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

(defun run-peek (pattern inp)
  "Run a `PEEK' pattern over an input stream, returning `0' if
the membe pattern succeeds and `NIL' if it fails."
  (with-slots (pattern) pattern
    (and (run-pattern pattern inp) 0)))


(defun run-repeat (pattern inp)
  "Run a `REPEAT' pattern over an input stream, returning the
number of characters matched or `NIL' for failure."
  (with-slots (pattern lower upper) pattern
    (loop for n = (run-pattern pattern inp)
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
