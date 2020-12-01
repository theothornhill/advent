
(ql:quickload :cl-ppcre)

(defvar *point-table* (make-hash-table :test #'equalp))
(defparameter *x* 0)
(defparameter *y* 0)
(defparameter *number-of-steps* 0)

(defun clear-table ()
  (setf *point-table* (make-hash-table :test #'equalp)))

(defun add-point (p)
  "If point is seen, update it to an intersection, If not, just add it."
  (multiple-value-bind (value present) (gethash p *point-table*)
    (if present
        (setf (gethash p *point-table*) t)
        (setf (gethash p *point-table*) value))))

(defun remove-commas (path)
  "Helper for parsing."
  (cl-ppcre:split " " (cl-ppcre:regex-replace-all "," path " ")))

(defun read-paths (filename)
  (with-open-file (stream filename)
    (let ((paths (loop for steps = (read-line stream nil nil)
                       while steps
                       collect steps)))
      (mapcar #'remove-commas paths))))

(defun split-string-to-list (step)
  "Helper for parsing."
  (let ((x (cl-ppcre:split " " (cl-ppcre:regex-replace "([A-Z])" step  "\\& "))))
    (list (intern (car x)) (parse-integer (cadr x)))))

(defun parse (path)
  (mapcar #'split-string-to-list path))

(defun parse-paths (filename)
  (let ((paths (read-paths filename)))
    (mapcar #'parse paths)))

(defun get-direction (step)
  (ecase (car step)
    (R (lambda () (incf *x*)))
    (L (lambda () (decf *x*)))
    (U (lambda () (incf *y*)))
    (D (lambda () (decf *y*)))))

(defun run-step (step)
  "Takes a step: ex: (R 54) and execute movement."
  (let ((direction (get-direction step))
        out)
    (dotimes (i (cadr step))
      (funcall direction)
      (push (list *x* *y*) out))
    (nreverse out)))

(defun run-steps (path)
  (setf *x* 0)
  (setf *y* 0)
  (mapcan #'run-step path))

(defun add-path-to-hash-table (path)
  (mapcar #'add-point (remove-duplicates path :test #'equalp)))

(defun steps-to-intersection (path)
  "Run through every point in path - check if an intersection.
Then record number of steps to hit"
  (setf *number-of-steps* 0)
  (let (result)
    (dolist (step path)
      (incf *number-of-steps*)
      (if (eql (gethash step *point-table*) 't)
          (push (list step *number-of-steps*) result)))
    (nreverse result)))

(defun shortest-path (path-1 path-2)
  "Path is list with elements ((x y) steps) in structure."
  (let (result)
    (dolist (p1 path-1)
      (dolist (p2 path-2)
        (if (equalp (car p1) (car p2))
            (push (+ (cadr p1) (cadr p2)) result))))
    (sort result #'<)))

(defun run-paths (filename)
  (clear-table)
  (destructuring-bind (path-one path-two) (parse-paths filename)
    (let ((path-one-steps (run-steps path-one))
          (path-two-steps (run-steps path-two)))
      (add-path-to-hash-table path-one-steps)
      (add-path-to-hash-table path-two-steps)
      (shortest-path
       (steps-to-intersection path-one-steps)
       (steps-to-intersection path-two-steps)))))

(defun get-intersections ()
  (let (result)
    (maphash (lambda (k v) (if (eql v 't)
                          (push k result)))
             *point-table*)
    (mapcar (lambda (x) (+ (abs (car x)) (abs (cadr x)))) result)))

(defun day3 ()
  (values (car (run-paths "./inputs/day3-input.txt"))
          (car (sort (get-intersections) #'<))))
