(in-package :cl-user)

(defpackage :dotre.character-range
  (:nicknames :cr)
  (:use :cl)
  (:export
   #:char-range
   #:range #:range-p
   #:compound-range #:compound-range-p
   #:range-member
   #:lower
   #:upper
   #:range< #:range<= #:range= #:range>= #:range>
   #:valid-range-p #:valid-range
   #:valid-compound-range-p #:valid-compound-range
   #:ranges-adjacent
   #:ranges-intersect
   #:ranges-contiguous
   #:ranges-union
   #:ranges-intersection
   #:ranges-difference
   #:ranges-symmetric-difference))

(in-package :dotre.character-range)

(deftype range ()
  "A `RANGE' is a pair of unsigned bytes representing the inclusive
  lower and upper bounds of character codes of a contiguous set of
  characters. A `RANGE' is valid if the lower bound is less than or
  equal to the upper bound"
  '(cons unsigned-byte unsigned-byte))


(defun range-p (arg)
  "Return `T' if the inpupt is a range.  Otherwise, return `NIL'"
  (typep arg 'range))


(defun compound-range-p (arg)
  "Return `T' if the input is a compound range.  Otherwise, return `NIL'."
  (and (listp arg)
       (every #'range-p arg)))


(deftype compound-range (&optional n)
  "A `COMPOUND-RANGE' is a list of ranges that together represent a
set of characters.  A `COMPOUND-RANGE' is valid if each of its mebers
is valid, the members are held in ascending order and there are no
pairs of adjacent members that are contiguous."
  (if (and n (not (eq n '*)))
      (labels ((recur (m accum)
                 (if (>= m n) accum
                     (recur (1+ m) `(cons range ,accum)))))
        (recur 0 'null))
      '(satisfies compound-range-p)))


(declaim
 (ftype (function (unsigned-byte unsigned-byte) range)
        range)

 (ftype (function (range) unsigned-byte)
        lower
        upper)

 (ftype (function (unsigned-byte range) boolean)
        range-member)

 (ftype (function (range range) boolean)
        range<
        range<=
        range=
        range>=
        range>
        ranges-intersect
        ranges-adjacent
        ranges-contiguous
        ordered-ranges-intersect/unsafe
        ordered-ranges-adjacent/unsafe
        ordered-ranges-contiguous/unsafe)

 (ftype (function (range range) (or (compound-range 1) (compound-range 2)))
        ranges-union
        ordered-ranges-union/unsafe)

 (ftype (function (range range) (or (compound-range 0) (compound-range 1)))
        ranges-intersection
        ordered-ranges-intersection/unsafe)

 (ftype (function (range range) (or (compound-range 0) (compound-range 1) (compound-range 2)))
        ranges-difference)

 (ftype (function (character character) range) char-range)

 (inline
  range lower upper
  range< range<= range= range>= range>
  ordered-ranges-intersect/unsafe
  ordered-ranges-adjacent/unsafe
  ordered-ranges-contiguous/unsafe
  ordered-ranges-intersection/unsafe
  ordered-ranges-union/unsafe))


(defun range (lower upper)
  "Return a character range in the form of `LOWER' and `UPPER' character code bounds."
  (assert (<= lower upper))
  (cons lower upper))


(defun lower (range)
  "Return the inclusive lower bound of the character range."
  (car range))


(defun upper (range)
  "return the inclusive upper bound of the character range."
  (cdr range))


(defun valid-range-p (arg)
  "Return `T' if the input is a valid `RANGE': the lower bound is less
than or equal to the upper bound.  Otherwise, return `NIL'."
  (and (typep arg 'range)
       (<= (lower arg)
           (upper arg))))

(deftype valid-range ()
  "a type describing valid ranges where the bounds are properly ordered."
  '(satisfies valid-range-p))


(defun range-member (char-code range)
  "Return `T' if the input character code, `CHAR-CODE' is in the input character range, `RANGE'.
Otherwise, return `NIL'."
  (and (<= (lower range) char-code)
       (>= (upper range) char-code)))


(defun range< (range1 range2)
  "Return `T' if `RANGE1' is less than `RANGE2'. Otherwise, return `NIL'.

Ordering is the lexical ordering of the bounds. If the lower bound of
`RANGE1' is less than the lower bound of `RANGE2', `RANGE1' is less
than `RANGE1'. When the lower bounds are equal and the upper bound  of
`RANGE1' is less than the upper bound of `RANGE2', `RANGE1' is less
than `RANGE2'. Otherwise, `RANGE1' is not less than `RANGE2'."
  (or (< (lower range1) (lower range2))
      (and (= (lower range1) (lower range2))
           (< (upper range1) (upper range2)))))


(defun range<= (range1 range2)
  "Return `T' if `RANGE1' is less than or equal to `RANGE2'. Otherwise, return `NIL'.

For details on the ordering of ranges, see `RANGE<'."
  (not (range< range2 range1)))


(defun range= (range1 range2)
  "Return `T' if `RANGE1' is equal to `RANGE2'. Otherwise, return `NIL'.

For details on the ordering of ranges, see `RANGE<'."
  (not (or (range< range1 range2) (range< range2 range1))))


(defun range>= (range1 range2)
  "Return `T' if `RANGE1' is greater than or equal to `RANGE2'. Otherwise, return `NIL'.

For details on the ordering of ranges, see `RANGE<'."
  (not (range< range1 range2)))


(defun range> (range1 range2)
  "Return `T' if `RANGE1' is greater than `RANGE2'. Otherwise, return `NIL'.

For details on the ordering of ranges, see `RANGE<'."
  (range< range2 range1))


(defun ordered-ranges-intersect/unsafe (range1 range2)
  "Return `T' if `RANGE1' and `RANGE2' have common members. Otherwise, return `NIL'.

This function is unsafe in the sense that it assumes the input ranges
are ordered: it assumes `RANGE1' is less than or equal to `RANGE2', without
verification."
  (>= (upper range1) (lower range2)))


(defun ranges-intersect (range1 range2)
  "Return `T' if `RANGE1' and `RANGE2' have common members. Otherwise, return `NIL'."
  (if (range< range1 range2)
      (ordered-ranges-intersect/unsafe range1 range2)
      (ordered-ranges-intersect/unsafe range2 range1)))


(defun ordered-ranges-adjacent/unsafe (range1 range2)
  "Return `T' if `RANGE1' is adjacent to `RANGE2'. Otherwise, return `NIL'.

This function is unsafe in the sense that it assumes the input ranges
are ordered: it assumes `RANGE1' is less than or equal to `RANGE2', without
verification."
  (= (1+ (upper range1))
     (lower range2)))


(defun ranges-adjacent (range1 range2)
  "Return `T' if `RANGE1' is adjacent to `RANGE2'. Otherwise, return `NIL'."
  (if (range< range1 range2)
      (ordered-ranges-adjacent/unsafe range1 range2)
      (ordered-ranges-adjacent/unsafe range2 range1)))


(defun ordered-ranges-contiguous/unsafe (range1 range2)
  "Return `T' if the input ranges form a contiguous range of values. Otherwise, return `NIL'.

This function is unsafe in the sense that it assumes the input ranges
are ordered: it assumes `RANGE1' is less than or equal to `RANGE2', without
verification."
  (>= (upper range1)  (1- (lower range2))))


(defun ranges-contiguous (range1 range2)
  "Return `T' if the input ranges form a contiguous range of values. Otherwise, return `NIL'."
  (if (range< range1 range2)
      (ordered-ranges-contiguous/unsafe range1 range2)
      (ordered-ranges-contiguous/unsafe range2 range1)))


(defun ordered-ranges-union/unsafe (range1 range2)
  "Return the simplified union of `RANGE1' and `RANGE2'.

This function is unsafe in the sense that it assumes the input ranges
are ordered: it assumes `RANGE1' is less than or equal to `RANGE2', without
verification."
  (if (ordered-ranges-contiguous/unsafe range1 range2)
      (list (range (lower range1) (max (upper range1) (upper range2))))
      (list range1 range2)))

(defun ranges-union (range1 range2)
  "Return the simplified union of `RANGE1' and `RANGE2'."
  (if (range< range1 range2)
      (ordered-ranges-union/unsafe range1 range2)
      (ordered-ranges-union/unsafe range2 range1)))


(defun ordered-ranges-intersection/unsafe (range1 range2)
  "Return the intersection of `RANGE1' and `RANGE2'.

This function is unsafe in the sense that it assumes the input ranges
are ordered: it assumes `RANGE1' is less than or equal to `RANGE2', without
verification."
  (if (ordered-ranges-intersect/unsafe range1 range2)
      (list (range (lower range2) (min (upper range1) (upper range2))))
      nil))

(defun ranges-intersection (range1 range2)
  "Return the intersection of `RANGE1' and `RANGE2'."
  (if (range< range1 range2)
      (ordered-ranges-intersection/unsafe range1 range2)
      (ordered-ranges-intersection/unsafe range2 range1)))

(defun ranges-difference (range1 range2)
  "Return the ranges equivalent to `RANGE1' less the members of `RANGE2'"
  (if (ranges-intersect range1 range2)
      (let ((lower-is-member (range-member (lower range1) range2))
            (upper-is-member (range-member (upper range1) range2)))
        (cond
          ((and lower-is-member upper-is-member) nil)
          (lower-is-member (list (range (1+ (upper range2)) (upper range1))) )
          (upper-is-member (list (range (lower range1) (1- (lower range2)))))
          (t (list (range (lower range1) (1- (lower range2)))
                   (range (1+ (upper range2)) (upper range1))))))
      (list range1)))

(defun ordered-ranges-symmetric-difference/unsafe (range1 range2)
  "Return a compound range that is the symmetric difference of the two input ranges.

This function is unsafe in the sense that it assumes the input ranges
are ordered: it assumes `RANGE1' is less than or equal to `RANGE2', without
verification."
  (if (ordered-ranges-intersect/unsafe range1 range2)
      (append (ranges-difference range1 range2)
              (ranges-difference range2 range1))
      (ordered-ranges-union/unsafe range1 range2)))

(defun ranges-symmetric-difference (range1 range2)
  "Return a compound range that is the symmetric difference of the two input ranges."
  (if (range< range1 range2)
      (ordered-ranges-symmetric-difference/unsafe range1 range2)
      (ordered-ranges-symmetric-difference/unsafe range2 range1)))


(defun valid-compound-range-p (arg)
  (and (compound-range-p arg)
       (loop for prev = nil then curr
             for curr in arg
             when (and prev (or (range< curr prev) (ranges-contiguous prev curr)))
               return nil
             finally (return t))))

(deftype valid-compound-range ()
  '(satisfies valid-compound-range-p))

(defun char-range (char1 char2)
  (range (char-code char1) (char-code char2)))
