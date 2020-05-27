(ql:quickload :cl-ppcre)
(setf cl-ppcre:*allow-named-registers* t)

(defun two-adjacent-p (str)
  "If any of the matches are only length 2, then we allow other
matches as well."
  (some (lambda (x) (= (length x) 2))
         (cl-ppcre:all-matches-as-strings
          "(?<reg>[0-9])(\\k<reg>){1,}" str)))

(defun never-decreases-p (str)
  (apply #'char<= (concatenate 'list str )))

(defun follows-rules-p (num)
  (let ((str (format nil "~a" num)))
    (and (two-adjacent-p str)
         (never-decreases-p str))))

(defun number-of-passwords ()
  (loop with count = 0
        for i from 183564 upto 657474
        do (if (follows-rules-p i)
               (incf count))
        finally (return count)))
