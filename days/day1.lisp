;; Part 1:

(defun read-fuel-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-integer line))))

(defun fuel-list (module)
  (let ((new-fuel (- (floor module 3) 2)))
    (if (and (plusp new-fuel) (> new-fuel 0))
        (cons new-fuel (fuel-list new-fuel)))))

(defun add-fuels (module)
  (reduce #'+ (fuel-list module)))

(defun fuel-required (filename)
  (let ((fuels (read-fuel-file filename)))
    (reduce #'+ (mapcar #'add-fuels fuels))))

(fuel-required "./inputs/day1-input.txt")
