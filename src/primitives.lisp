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
  (lexeme (make-item) :type pattern)
  (class (make-character-class (range 0 char-code-limit)) :type character-class))

(defstruct (repeat (:include pattern))
  (lexeme (make-item) :type pattern)
  (lower nil :type (or null unsigned-byte))
  (upper nil :type (or null unsigned-byte)))

(defstruct (alt (:include pattern))
  (first (make-item) :type pattern)
  (second (make-item) :type pattern))

(defstruct (cut (:include pattern))
  (first (make-item) :type pattern)
  (second (make-item) :type pattern))

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


(defun run-pattern (lexeme inp)
  (typecase lexeme
    (guard
     ;; Note: When a `GUARD' is holding a `REPEAT' pattern accepting zero
     ;; matches, the `GUARD' needs to allow the match with an empty input
     ;; stream.


     (with-slots (lexeme class) lexeme
       (let ((char (peek-char nil inp nil)))
         (if char
             (progn
               (and (class-member (char-code char) class)
                    (run-pattern lexeme inp)))
             (progn
               (and (repeat-p lexeme)
                    (or (null (repeat-lower lexeme))
                        (zerop (repeat-lower lexeme)))))))))
    (item
     (if (peek-char nil inp nil) 1  nil))

    (alt
     ;; FIXME: `ALT' should be modified to hold a
     ;; list of patterns to prevent the recursion
     ;; into the tail of the `ALT' patterns.
     (with-slots (first second) lexeme
       (let ((first (run-pattern first inp))
             (second (run-pattern second inp)))
         (cond ((and first second)
                (assert (numberp first))
                (assert (numberp second))
                (max first second))
               (first
                (assert (numberp first))
                first)
               (t second)))))

    (cut
     ;; FIXME: `CUT' should be modified to hold a
     ;; list of patterns to prevent the recursion
     ;; into the tail of the `CUT' patterns.
     (with-slots (first second) lexeme
       (let ((n (run-pattern first inp)))
         (or n (run-pattern second inp)))))

    (repeat
     (with-slots (lexeme lower upper) lexeme
       (labels ((recur (repetition char-count)
                  (if (and upper (>= repetition upper)) char-count
                      (let ((n (run-pattern lexeme inp)))
                        (if n (progn
                                (assert (numberp n))
                                ;; FIXME: The recursion will build out the stack,
                                ;; because of the `WITH-CHARS-HELD' context. The handleing
                                ;; if this case needs to be modified to use `LOOP'.
                                (with-chars-held (n inp)
                                  (recur (1+ repetition) (+ n char-count))))
                            (and (or (null lower) (<= lower repetition)) char-count))))))
         (recur 0 0))))


    (seq
     (with-slots (patterns) lexeme
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
                  (return accum)))

       ;; (let ((m (run-pattern (car patterns) inp)))
       ;;   (if m
       ;;       ;; FIXME: The recursion will build outthe stack,
       ;;       ;; because of the `WITH-CHARS-HELD' context. The
       ;;       ;; handling of this case should be modified to use
       ;;       ;; `LOOP'.  Also `SEQ' should be modified to hold
       ;;       ;; a list of patterns.
       ;;       (with-chars-held (m inp)
       ;;         (let ((n (run-pattern second inp)))
       ;;           (if n (progn
       ;;                   (assert (numberp n))
       ;;                   (assert (numberp m))
       ;;                   (+ m n))
       ;;               nil)))
       ;;       nil))
       ))))


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

  ;; Note: When `LOWER' is `NIL' or zero, the guard needs to pass through
  ;; any character because matching zero characters is a success.  Otherwise,
  ;; the guard from the repeated pattern is used.
  (make-guard
   :lexeme (make-repeat :lexeme lexeme :lower lower :upper upper)

   :class (if (or (null lower) (zerop lower)) (make-character-class (range 0 char-code-limit))
              (guard-class lexeme))))


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
                :patterns (cons lexeme lexemes))
       :class (guard-class lexeme))))
