(in-package :cl-user)


(defpackage :dotre/test.character-class
  (:use :cl :5am :dotre.character-range :dotre.character-class)
  (:export #:run-all-tests!))


(in-package :dotre/test.character-class)


(defun run-all-tests! ()
  (run! 'character-class))


(def-suite character-class)


(in-suite character-class)


(defun random-range ()
  (let ((a1 (random 128))
        (a2 (random 128)))
    (range (min a1 a2)
         (max a1 a2))))

(defun random-character-class (m)
  (let ((n (random m)))
    (apply #'make-character-class
           (loop for i from 1 to n collecting (random-range)))))

(test construction-and-types
  (let ((cls (range-to-character-class (range 4 6))))
    (is-true (typep cls 'character-class))
    (is-true (character-class-p cls))
    (is-true (typep cls 'valid-character-class))
    (is-true (valid-character-class-p cls)))

  (let ((cls (make-character-class (range 3 4) (range 6 7))))
    (is-true (typep cls 'character-class))
    (is-true (character-class-p cls))
    (is-true (typep cls 'valid-character-class))
    (is-true (valid-character-class-p cls)))

  (let ((cls (make-character-class (range 6 7) (range 3 4))))
    (is-true (typep cls 'character-class))
    (is-true (character-class-p cls))
    (is-true (typep cls 'valid-character-class))
    (is-true (valid-character-class-p cls)))

  (let ((cls (make-character-class (range 4 5) (range 6 7))))
    (is-true (typep cls 'character-class))
    (is-true (character-class-p cls))
    (is-true (typep cls 'valid-character-class))
    (is-true (valid-character-class-p cls)))

  (let ((cls (make-character-class (range 6 6) (range 4 7))))
    (is-true (typep cls 'character-class))
    (is-true (character-class-p cls))
    (is-true (typep cls 'valid-character-class))
    (is-true (valid-character-class-p cls)))

  (dotimes (i 10)
    (let ((cls (random-character-class 10)))
      (is-true (typep cls 'character-class))
      (is-true (character-class-p cls))
      (is-true (typep cls 'valid-character-class))
      (is-true (valid-character-class-p cls)))))

(test class-member
  (let ((cls (make-character-class (range 3 4) (range 7 8))))
    (is-false (class-member 1 cls))
    (is-false (class-member 2 cls))

    (is-true (class-member 3 cls))
    (is-true (class-member 4 cls))

    (is-false (class-member 5 cls))
    (is-false (class-member 6 cls))

    (is-true (class-member 7 cls))
    (is-true (class-member 8 cls))

    (is-false (class-member 9 cls))
    (is-false (class-member 10 cls))))


(test class-union
  (let* ((class1 (make-character-class (range 1 2) (range 4 10)))
         (class2 (make-character-class (range 0 0) (range 5 11) (range 13 14)))
         (class3 (class-union class1 class2)))
    (loop for i from 0 to 20
          do (is-true (or (and (or (class-member i class1) (class-member i class2))
                               (class-member i class3))
                          (not (or (class-member i class1)
                                   (class-member i class2)
                                   (class-member i class3))))))))


(test class-intersection
  (dotimes (i 5)
    (let* ((class1 (random-character-class 5))
           (class2 (random-character-class 5))
           (class3 (class-intersection class1 class2)))
      (loop for i from 0 to 10
            for j = (random 128)
            do (is-true (or (and (class-member j class1) (class-member j class2) (class-member j class3))
                            (and (not (class-member j class3))
                                 (or (not (class-member j class1)) (not (class-member j class2))))))))))

(test class-difference
  (dotimes (i 5)
    (let* ((class1 (random-character-class 5))
           (class2 (random-character-class 5))
           (class3 (class-difference class1 class2)))
      (loop for i from 0 to 10
            for j = (random 128)
            do (is-true
                (or (and (class-member j class1) (not (class-member j class2)) (class-member j class3))
                    (and (class-member j class1) (class-member j class2) (not (class-member j class3)))
                    (and (not (class-member j class1)) (not (class-member j class3)))))))))

(test class-symmetric-difference
  (dotimes (i 5)
    (let* ((class1 (random-character-class 5))
           (class2 (random-character-class 5))
           (class3 (class-symmetric-difference class1 class2)))
      (loop for i from 0 to 10
            for j = (random 128)
            do (is-true
                (or (and (class-member i class1) (not (class-member i class2)) (class-member i class3))
                    (and (not (class-member i class1)) (class-member i class2) (class-member i class3))
                    (not (class-member i class3))))))))

(test class-complement
  (dotimes (i 5)
    (let* ((class1 (random-character-class 5))
           (class2 (class-complement class1)))
      (loop for i from 0 to 10
            for j = (random 128)
            do (is-true
                (or (and (class-member j class1) (not (class-member j class2)))
                    (and (not (class-member j class1))  (class-member j class2))))))))
