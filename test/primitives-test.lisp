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
    (is (= 1 (run-lexeme (make-item) inp)))
    (is (char= #\a (peek-char nil inp nil))))
  (with-input-from-string (inp "")
    (is (= 0 (run-lexeme (make-item) inp)))))

(test guard
  (with-input-from-string (inp "abc123")
    (is (= 1 (run-lexeme
              (make-guard
               :lexeme (make-item)
               :class (make-character-class
                       (char-range #\a #\a)))
              inp)))
    (is (char= #\a (peek-char nil inp nil)))
    (is (= 0 (run-lexeme
              (make-guard
               :lexeme (make-item)
               :class (make-character-class
                       (char-range #\b #\b)))
              inp)))
    (is (char= #\a (peek-char nil inp nil)))))

(test repeat
  (with-input-from-string (inp "abc123")
    (is (= 6 (run-lexeme (make-repeat :lexeme (make-item)) inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 3 (run-lexeme
              (make-repeat
               :lexeme (make-guard
                        :lexeme (make-item)
                        :class (make-character-class
                                (char-range #\a #\z))))
              inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 0 (run-lexeme
              (make-repeat
               :lexeme (make-guard
                        :lexeme (make-item)
                        :class (make-character-class
                                (char-range #\0 #\9))))
              inp)))
    (is (char= #\a (peek-char nil inp nil)))


    (is (= 2 (run-lexeme
              (make-repeat
               :lexeme (make-guard
                        :lexeme (make-item)
                        :class (make-character-class
                                (char-range #\a #\z)))
               :lower 2
               :upper 2)
              inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 3 (run-lexeme
              (make-repeat
               :lexeme (make-guard
                        :lexeme (make-item)
                        :class (make-character-class
                                (char-range #\a #\z)))
               :lower 2
               :upper 4)
              inp)))
    (is (char= #\a (peek-char nil inp nil)))

    (is (= 0 (run-lexeme
              (make-repeat
               :lexeme (make-guard
                        :lexeme (make-item)
                        :class (make-character-class
                                (char-range #\a #\z)))
               :lower 4
               :upper 5)
              inp)))
    (is (char= #\a (peek-char nil inp nil)))))


(test alt
  (with-input-from-string (inp "abc123")
      (let* ((letter (make-guard
                      :lexeme (make-item)
                      :class (make-character-class
                              (char-range #\A #\Z)
                              (char-range #\a #\z))))
             (digit (make-guard
                     :lexeme (make-item)
                     :class (make-character-class (char-range #\0 #\9))))
             (alnum (make-guard
                     :lexeme (make-item)
                     :class (make-character-class
                             (char-range #\0 #\9)
                             (char-range #\A #\Z)
                             (char-range #\a #\z))))
             (many-letters (make-repeat :lexeme letter))
             (many-digits (make-repeat :lexeme digit))
             (many-alnum (make-repeat :lexeme alnum)))

        (is (= 3 (run-lexeme (make-alt :first many-letters :second  many-digits) inp)))
        (is (char= #\a (peek-char nil inp nil)))

        (is (= 3 (run-lexeme (make-alt :first many-digits :second  many-letters) inp)))
        (is (char= #\a (peek-char nil inp nil)))

        (is (= 6 (run-lexeme (make-alt :first many-letters :second  many-alnum) inp)))
        (is (char= #\a (peek-char nil inp nil)))

        (is (= 6 (run-lexeme (make-alt :first many-alnum :second  many-letters) inp)))
        (is (char= #\a (peek-char nil inp nil)))

        (is (= 0 (run-lexeme (make-alt :first many-digits :second  many-digits) inp)))
        (is (char= #\a (peek-char nil inp nil))))))

(test cut
  (with-input-from-string (inp "abc123")
      (let* ((letter (make-guard
                      :lexeme (make-item)
                      :class (make-character-class
                              (char-range #\A #\Z)
                              (char-range #\a #\z))))
             (digit (make-guard
                     :lexeme (make-item)
                     :class (make-character-class (char-range #\0 #\9))))
             (alnum (make-guard
                     :lexeme (make-item)
                     :class (make-character-class
                             (char-range #\0 #\9)
                             (char-range #\A #\Z)
                             (char-range #\a #\z))))
             (many-letters (make-repeat :lexeme letter))
             (many-digits (make-repeat :lexeme digit))
             (many-alnum (make-repeat :lexeme alnum)))

        (is (= 3 (run-lexeme (make-cut :first many-letters :second  many-digits) inp)))
        (is (char= #\a (peek-char nil inp nil)))

        (is (= 3 (run-lexeme (make-cut :first many-digits :second  many-letters) inp)))
        (is (char= #\a (peek-char nil inp nil)))

        (is (= 3 (run-lexeme (make-cut :first many-letters :second  many-alnum) inp)))
        (is (char= #\a (peek-char nil inp nil)))

        (is (= 6 (run-lexeme (make-cut :first many-alnum :second  many-letters) inp)))
        (is (char= #\a (peek-char nil inp nil)))

        (is (= 0 (run-lexeme (make-cut :first many-digits :second  many-digits) inp)))
        (is (char= #\a (peek-char nil inp nil))))))
