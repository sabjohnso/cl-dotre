(in-package :cl-user)

(defpackage :dotre.character-class
  (:nicknames :cclass)
  (:use :cl :dotre.character-range)
  (:export
   #:character-class
   #:character-class-p
   #:valid-character-class
   #:valid-character-class-p
   #:range-to-character-class
   #:make-character-class
   #:ranges
   #:class-member
   #:class-union
   #:class-intersection
   #:class-difference
   #:class-symmetric-difference
   #:class-complement))

(in-package :dotre.character-class)

(deftype character-class ()
  'compound-range)

(defun character-class-p (arg)
  "Return `T' if the input is a character class.  Otherwise, return `NIL'."
  (typep arg 'character-class))

(deftype valid-character-class ()
  'valid-compound-range)

(defun valid-character-class-p (arg)
  "Return `T' if the input is a character class.  Otherwise, return `NIL'."
  (typep arg 'valid-character-class))

(declaim
 (ftype (function (range) character-class)
        range-to-character-class)

 (ftype (function (&rest range) character-class)
        make-character-class)

 (ftype (function (character-class character-class) character-class)
        class-union
        class-intersection
        class-difference
        class-symmetric-difference)

 (ftype (function (character-class) character-class)
        class-complement))

(defun range-to-character-class (range)
  (list range))

(defun rappend (xs ys)
  (labels ((recur (xs ys)
             (if (null xs) ys
                 (recur (cdr xs) (cons (car xs) ys)))))
    (recur xs ys)))

(defun make-character-class (&rest ranges)
  ;; Destructive sorting is okay here because the list is constructred
  ;; from the rest args.
  (if (null ranges) nil
      (let ((ranges (sort ranges #'range<)))
        (labels ((recur (previous remaining)
                   (if (null remaining) (reverse previous)
                       (recur
                        (rappend (ranges-union (car previous) (car remaining)) (cdr previous))
                        (cdr remaining)))))
          (recur (list (car ranges)) (cdr ranges))))))

(defun class-member (char-code class)
  (loop for range in class
        when (range-member char-code range)
          do (return t)
        finally (return nil)))

(defun class-union (class1 class2)
  (apply #'make-character-class (append class1 class2)))

(defun class-intersection (class1 class2)
  (apply #'make-character-class
         (loop for range1 in class1
         appending (loop for range2 in class2
                         appending (ranges-intersection range1 range2)))))

(defun class-difference (class1 class2)
  (if (null class2) class1
      (apply #'make-character-class
             (loop for range1 in class1
                   appending
                   (loop for range2 in class2
                         for fragments = (ranges-difference range1 range2)
                           then (loop for fragment in fragments
                                      appending (ranges-difference fragment range2))
                         finally (return fragments))))))

(defun class-symmetric-difference (class1 class2)
  (apply #'make-character-class
         (append (class-difference class1 class2)
                 (class-difference class2 class1))))

(defun class-complement (cls)
  (class-difference (range-to-character-class (range 0 char-code-limit)) cls))
