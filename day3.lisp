
(ql:quickload :cl-ppcre)

(defvar *point-table* (make-hash-table :test #'equalp))
(defparameter *x* 0)
(defparameter *y* 0)
(defparameter *number-of-steps* 0)

(defun clear-table ()
  (setf *point-table* (make-hash-table :test #'equalp)))

(defun add-point (p)
  (multiple-value-bind (value present) (gethash p *point-table*)
    (if present
        (setf (gethash p *point-table*) t)
        (setf (gethash p *point-table*) value))))

(defun remove-commas (path)
  (cl-ppcre:split " " (cl-ppcre:regex-replace-all "," path " ")))

(defun read-paths (filename)
  (with-open-file (stream filename)
    (let ((paths (loop for steps = (read-line stream nil nil)
                       while steps
                       collect steps)))
      (mapcar #'remove-commas paths))))

(defun split-string-to-list (step)
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
  (let ((direction (get-direction step))
        out)
    (dotimes (i (cadr step))
      (funcall direction)
      (push (list *x* *y*) out))
    out))

(defun run-steps (path)
  (setf *x* 0)
  (setf *y* 0)
  (mapcan #'run-step path))

(defun run-paths (filename)
  (clear-table)
  (destructuring-bind (path-one path-two) (parse-paths filename)
    (add-path (run-steps path-one))
    (add-path (run-steps path-two))))

(defun add-path (path)
  (mapcar #'add-point (remove-duplicates path :test #'equalp)))

(defun get-intersections ()
  (let (result)
    (maphash (lambda (k v) (if (eql v 't)
                          (push k result)))
             *point-table*)
    (mapcar (lambda (x) (+ (abs (car x)) (abs (cadr x)))) result)))

(defun part-one ()
  (run-paths "./day3-input.txt")
  (car (sort (get-intersections) #'<)))
