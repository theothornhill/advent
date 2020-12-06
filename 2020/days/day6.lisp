(in-package #:advent-2020)

(defvar *day6-file* "2020/inputs/day6.txt")

(defun remove-spaces (string)
  (cl-ppcre:regex-replace-all " " string ""))

(defun list-of-chars (string)
  (cl-ppcre:split "" string))

(defun list-of-syms (string)
  (mapcar #'intern (cl-ppcre:split "" string)))

(defun map-list-of-syms (list)
  (mapcar #'list-of-syms list))

(defun make-set (list)
  (let (res)
    (dolist (i list)
      (pushnew i res :test #'string=))
    res))

(defun make-intersection (list)
  (reduce #'intersection list))

(defun day-6-part-1 ()
  (->> (slurp *day6-file*)
    (mapcar #'remove-spaces)
    (mapcar #'list-of-chars)
    (mapcar #'make-set)
    (mapcar #'length)
    (apply #'+)))

(defun day-6-part-2 ()
  (->> (slurp *day6-file*)
    (mapcar #'split-by-space)
    (mapcar #'map-list-of-syms)
    (mapcar #'make-intersection)
    (mapcar #'length)
    (apply #'+)))
