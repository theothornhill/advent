(in-package #:advent-2020)

(defvar *day5-file* "2020/inputs/day5.txt")

(defun convert-to-num (string)
  (-<> string
    (cl-ppcre:regex-replace-all "F" <> "0")
    (cl-ppcre:regex-replace-all "B" <> "1")
    (cl-ppcre:regex-replace-all "L" <> "0")
    (cl-ppcre:regex-replace-all "R" <> "1")
    (parse-integer <> :radix 2)))

(defvar *ids* (mapcar #'convert-to-num (slurp *day5-file*)))

(defun day-5-part-1 ()
  (reduce #'max *ids*))

(defun day-5-part-2 ()
  (loop with max = (day-5-part-1)
        with min = (1+ (reduce #'min *ids*))
        for i from min to max
        unless (find i *ids*) do (return i)))
