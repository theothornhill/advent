(in-package #:advent-2020)

(defvar *day2-file* "2020/inputs/day2.txt")

(defun split-by-dash (string)
  (mapcar #'parse-integer (cl-ppcre:split "-" string)))

(defun remove-colon (string)
  (cl-ppcre:regex-replace ":" string ""))

(defun count-string (s string)
  (count-if (lambda (str) (string= s str)) string))

(defun valid-password-part-1-p (string)
  (let* ((split-space (split-by-space string))
         (split-dash (split-by-dash (first split-space)))
         (colon-removed (remove-colon (second split-space))))
    (<= (first split-dash)
        (count-string colon-removed (third split-space))
        (second split-dash))))

(defun count-valid-passwords-part-1 ()
  (count-if-not #'null
                (mapcar #'valid-password-part-1-p (slurp *day2-file*))))

;;; Part 2
(defun char-at-position-p (c p string)
  (if (string= c (aref string (1- p)))
      1
      0))

(defun valid-password-part-2-p (string)
  (let* ((split-space (split-by-space string))
         (split-dash (split-by-dash (first split-space)))
         (colon-removed (remove-colon (second split-space))))
    (logxor (char-at-position-p colon-removed (first split-dash) (third split-space))
            (char-at-position-p colon-removed (second split-dash) (third split-space)))))

(defun count-valid-passwords-part-2 ()
  (count-if-not #'zerop
                (mapcar #'valid-password-part-2-p (slurp *day2-file*))))
