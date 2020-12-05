(in-package #:advent-2020)

(defun slurp (filename)
  (uiop:read-file-lines filename))

(defun split-by-space (string)
  (cl-ppcre:split #\Space string))
