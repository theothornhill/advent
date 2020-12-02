(in-package #:advent-2020)

(defvar *day1-file* "2020/inputs/day1.txt")

(defun is-it-2020?-part-1 ()
  (let ((numbers (mapcar #'parse-integer (slurp *day1-file*))))
    (dolist (numi numbers)
      (dolist (numj numbers)
        (unless (= numi numj)
          (when (= 2020 (+ numi numj))
            (return-from is-it-2020?-part-1 (* numi numj))))))))

(defun is-it-2020?-part-2 ()
  (let ((numbers (mapcar #'parse-integer (slurp *day1-file*))))
    (dolist (numi numbers)
      (dolist (numj numbers)
        (dolist (numk numbers)
          (unless (= numi numj numk)
            (when (= 2020 (+ numi numj numk))
              (return-from is-it-2020?-part-2 (* numi numj numk)))))))))
