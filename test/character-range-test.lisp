(in-package :cl-user)


(defpackage :dotre/test.character-range
  (:use :cl :5am :dotre.character-range)
  (:export #:run-all-tests!))


(in-package :dotre/test.character-range)


(defun run-all-tests! ()
  (run! 'character-range))


(def-suite character-range)


(in-suite character-range)


(test construction-and-types
  (let ((r (range 50 50)))
    (is-true (typep r 'range))
    (is-true (typep r 'valid-range)))

  (let ((r (range 50 60)))
    (is-true (typep r 'range))
    (is-true (typep r 'valid-range)))

  (let ((r (cons 60 50)))
    (is-true (typep r 'range))
    (is-false (typep r 'valid-range)))

  (let ((r "Something completely different."))
    (is-false (typep r 'range))
    (is-false (typep r 'valid-range))))


(test range-poset
  (is-true (range< (range 40 50) (range 45 55)))
  (is-true (range< (range 40 50) (range 40 55)))
  (is-true (range< (range 40 50) (range 45 45)))
  (is-false (range< (range 45 55) (range 40 50)))
  (is-false (range< (range 40 55) (range 40 50)))
  (is-false (range< (range 45 45) (range 40 50)))
  (is-false (range< (range 40 50) (range 40 50)))

  (is-true (range<= (range 40 50) (range 45 55)))
  (is-true (range<= (range 40 50) (range 40 55)))
  (is-true (range<= (range 40 50) (range 45 45)))
  (is-false (range<= (range 45 55) (range 40 50)))
  (is-false (range<= (range 40 55) (range 40 50)))
  (is-false (range<= (range 45 45) (range 40 50)))
  (is-true (range<= (range 40 50) (range 40 50)))

  (is-false (range= (range 45 55) (range 40 50)))
  (is-false (range= (range 40 55) (range 40 50)))
  (is-false (range= (range 45 45) (range 40 50)))
  (is-true (range= (range 40 50) (range 40 50)))

  (is-false (range>= (range 40 50) (range 45 55)))
  (is-false (range>= (range 40 50) (range 40 55)))
  (is-false (range>= (range 40 50) (range 45 45)))
  (is-true (range>= (range 45 55) (range 40 50)))
  (is-true (range>= (range 40 55) (range 40 50)))
  (is-true (range>= (range 45 45) (range 40 50)))
  (is-true (range>= (range 40 50) (range 40 50)))

  (is-false (range> (range 40 50) (range 45 55)))
  (is-false (range> (range 40 50) (range 40 55)))
  (is-false (range> (range 40 50) (range 45 45)))
  (is-true (range> (range 45 55) (range 40 50)))
  (is-true (range> (range 40 55) (range 40 50)))
  (is-true (range> (range 45 45) (range 40 50)))
  (is-false (range> (range 40 50) (range 40 50))))


(test ranges-adjacent
  (is-true (ranges-adjacent (range 4 5) (range 6 7)))
  (is-true (ranges-adjacent (range 6 7) (range 4 5)))

  (is-false (ranges-adjacent (range 3 4) (range 6 7)))
  (is-false (ranges-adjacent (range 6 7) (range 3 4)))

  (is-false (ranges-adjacent (range 5 6) (range 6 7)))
  (is-false (ranges-adjacent (range 6 7) (range 5 6))))


(test ranges-intersect
  (is-true (ranges-intersect (range 5 6) (range 6 7)))
  (is-true (ranges-intersect (range 6 7) (range 5 6)))

  (is-false (ranges-intersect (range 4 5) (range 6 7)))
  (is-false (ranges-intersect  (range 6 7) (range 4 5)))

  (is-false (ranges-intersect (range 3 4) (range 6 7)))
  (is-false (ranges-intersect (range 6 7) (range 3 4))))


(test ranges-contiguous
  (is-true (ranges-contiguous (range 4 5) (range 6 7)))
  (is-true (ranges-contiguous (range 6 7) (range 4 5)))

  (is-true (ranges-contiguous (range 5 6) (range 6 7)))
  (is-true (ranges-contiguous (range 6 7) (range 5 6)))

  (is-false (ranges-contiguous (range 3 4) (range 6 7)))
  (is-false (ranges-contiguous (range 6 7) (range 3 4))))


(test ranges-union
  (is (equal (list (range 4 7)) (ranges-union (range 4 5) (range 6 7))))
  (is (equal (list (range 4 7)) (ranges-union (range 6 7) (range 4 5))))
  (is (equal (list (range 5 7)) (ranges-union (range 5 6) (range 6 7))))
  (is (equal (list (range 5 7)) (ranges-union (range 6 7) (range 5 6))))
  (is (equal (list (range 3 4) (range 6 7)) (ranges-union (range 3 4) (range 6 7))))
  (is (equal (list (range 3 4) (range 6 7)) (ranges-union (range 6 7) (range 3 4)))))


(test ranges-intersection
  (is (equal (list (range 6 6)) (ranges-intersection (range 5 6) (range 6 7))))
  (is (equal (list (range 6 6)) (ranges-intersection (range 6 7) (range 5 6))))
  (is (equal nil (ranges-intersection (range 4 5) (range 6 7))))
  (is (equal nil (ranges-intersection (range 6 7) (range 4 5))))
  (is (equal nil (ranges-intersection (range 3 4) (range 6 7))))
  (is (equal nil (ranges-intersection (range 6 7) (range 3 4)))))


(test ranges-difference
  (is (equal (list (range 3 4)) (ranges-difference (range 3 4) (range 6 7))))
  (is (equal (list (range 6 7)) (ranges-difference  (range 6 7) (range 3 4))))
  (is (equal (list (range 4 5)) (ranges-difference (range 4 5) (range 6 7))))
  (is (equal (list (range 6 7)) (ranges-difference  (range 6 7) (range 4 5))))

  (is (equal (list (range 5 5)) (ranges-difference (range 5 6) (range 6 7))))
  (is (equal (list (range 7 7)) (ranges-difference (range 6 7) (range 5 6))))

  (is (equal nil (ranges-difference (range 5 6) (range 5 6))))
  (is (equal nil (ranges-difference (range 5 6) (range 5 7))))
  (is (equal nil (ranges-difference (range 5 6) (range 4 7))))

  (is (equal (list (range 4 4) (range 7 7)) (ranges-difference (range 4 7) (range 5 6)))))


(test ranges-symmetric-difference
  (is (equal (list (range 3 4) (range 6 7)) (ranges-symmetric-difference (range 3 4) (range 6 7))))
  (is (equal (list (range 3 4) (range 6 7)) (ranges-symmetric-difference (range 6 7) (range 3 4))))

  (is (equal (list (range 4 7)) (ranges-symmetric-difference (range 4 5) (range 6 7))))
  (is (equal (list (range 4 7)) (ranges-symmetric-difference (range 6 7) (range 4 5))))

  (is (equal (list (range 5 5) (range 7 7)) (ranges-symmetric-difference (range 5 6) (range 6 7))))
  (is (equal (list (range 5 5) (range 7 7)) (ranges-symmetric-difference (range 6 7) (range 5 6))))

  (is (equal (list (range 5 5) (range 7 7)) (ranges-symmetric-difference (range 6 6) (range 5 7))))
  (is (equal (list (range 5 5) (range 7 7)) (ranges-symmetric-difference (range 5 7) (range 6 6)))))
