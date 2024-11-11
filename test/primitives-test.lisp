(in-package :cl-user)

(defpackage :dotre/test.primitives
  (:use :cl :fiveam :dotre.character-range :dotre.character-class :dotre.primitives)
  (:export #:run-all-tests!))

(in-package :dotre/test.primitives)

(defun run-all-tests! ()
  (run! 'primitives))

(def-suite primitives)

(in-suite primitives)

(test item
  (with-input-from-string (inp "abc123")
    (is (= 1 (run-pattern (item) inp)))
    (is (char= #\a (peek-char nil inp nil))))
  (with-input-from-string (inp "")
    (is (= 0 (run-pattern (item) inp)))))

(test guard
  (with-input-from-string (inp "abc123")

    (is (= 1 (run-pattern (guard (item) (make-character-class (char-range #\a #\a))) inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 0 (run-pattern (guard (item) (make-character-class (char-range #\b #\b))) inp)))
    (is (char= #\a (peek-char nil inp nil))))

    (with-input-from-string (inp "")
      (is (= 0 (run-pattern (guard (item) (make-character-class (char-range #\b #\b))) inp)))))

(test repeat
  (with-input-from-string (inp "abc123")
    (is (= 6 (run-pattern (repeat (item)) inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 3 (run-pattern
              (repeat (guard (item) (make-character-class (char-range #\a #\z))))
              inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 0 (run-pattern
              (repeat (guard (item) (make-character-class (char-range #\0 #\9))))
              inp)))
    (is (char= #\a (peek-char nil inp nil)))


    (is (= 2 (run-pattern
              (repeat
               (guard (item)  (make-character-class (char-range #\a #\z)))
               :lower 2
               :upper 2)
              inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 3 (run-pattern
              (repeat
               (guard (item) (make-character-class (char-range #\a #\z)))
               :lower 2
               :upper 4)
              inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 0 (run-pattern
              (repeat (guard (item) (make-character-class (char-range #\a #\z)))
               :lower 4
               :upper 5)
              inp)))
    (is (char= #\a (peek-char nil inp nil)))))


(test alt
  (with-input-from-string (inp "abc123")
    (let* ((letter (guard
                    (item)
                    (make-character-class
                     (char-range #\A #\Z)
                     (char-range #\a #\z))))
           (digit (guard
                   (item)
                   (make-character-class
                    (char-range #\0 #\9))))
           (alnum (guard
                   (item)
                   (make-character-class
                    (char-range #\0 #\9)
                    (char-range #\A #\Z)
                    (char-range #\a #\z))))
           (many-letters (repeat letter))
           (many-digits (repeat digit))
           (many-alnum (repeat alnum)))

      (is (= 3 (run-pattern (alt many-letters many-digits) inp)))
      (is (char= #\a (peek-char nil inp nil)))

      (is (= 3 (run-pattern (alt many-digits many-letters) inp)))
      (is (char= #\a (peek-char nil inp nil)))

      (is (= 6 (run-pattern (alt many-letters many-alnum) inp)))
      (is (char= #\a (peek-char nil inp nil)))

      (is (= 6 (run-pattern (alt many-alnum many-letters) inp)))
      (is (char= #\a (peek-char nil inp nil)))

      (is (= 0 (run-pattern (alt many-digits many-digits) inp)))
      (is (char= #\a (peek-char nil inp nil))))))



(test cut
  (with-input-from-string (inp "abc123")
    (let* ((letter (guard
                    (item)
                    (make-character-class
                     (char-range #\A #\Z)
                     (char-range #\a #\z))))
           (digit (guard
                   (item)
                   (make-character-class
                    (char-range #\0 #\9))))
           (alnum (guard
                   (item)
                   (make-character-class
                    (char-range #\0 #\9)
                    (char-range #\A #\Z)
                    (char-range #\a #\z))))
           (many-letters (repeat letter))
           (many-digits (repeat digit))
           (many-alnum (repeat alnum)))

      (is (= 3 (run-pattern (cut many-letters many-digits) inp)))
      (is (char= #\a (peek-char nil inp nil)))

      (is (= 3 (run-pattern (cut many-digits many-letters) inp)))
      (is (char= #\a (peek-char nil inp nil)))

      (is (= 3 (run-pattern (cut many-letters many-alnum) inp)))
      (is (char= #\a (peek-char nil inp nil)))

      (is (= 6 (run-pattern (cut many-alnum many-letters) inp)))
      (is (char= #\a (peek-char nil inp nil)))

      (is (= 0 (run-pattern (cut many-digits many-digits) inp)))
      (is (char= #\a (peek-char nil inp nil))))))

(test seq
  (with-input-from-string (inp "abc123")
    (let* ((letter (guard
                    (item)
                    (make-character-class
                     (char-range #\A #\Z)
                     (char-range #\a #\z))))
           (digit (guard
                   (item)
                   (make-character-class
                    (char-range #\0 #\9))))
           (alnum (guard
                   (item)
                   (make-character-class
                    (char-range #\0 #\9)
                    (char-range #\A #\Z)
                    (char-range #\a #\z)))))

      (is (= 4 (run-pattern (seq letter letter letter digit) inp)))
      (is (= 4 (run-pattern (seq (repeat letter) digit) inp)))

      ;; FIXME: The following should pass, but fails because, as currently implemented,
      ;; there is no way to distinguish between failures and matches matching 0 items.
      ;; Here, `(REPEAT DIGIT)' should match zero digits, and the sequence should carry
      ;; on to match `(REPEAT LETTER)'. The solution is to modify the code to return
      ;; `NIL' on failure instead of zero, that they may be distinguished.
      (is (= 3 (run-pattern (seq (repeat digit) (repeat letter)) inp)))

      #||#)))
