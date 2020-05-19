(ql:quickload :cl-ppcre)

(defclass orbits ()
  ((name :initarg :name :initform nil :accessor name)
   (distance :initarg :distance :initform most-positive-fixnum :accessor distance)
   (parent :initarg :parent :initform nil :accessor parent)
   (children :initarg :children :initform nil :accessor children)))

(defvar *objects* (make-hash-table))
(defvar *center-of-mass* 'com)

(defun clear-objects ()
  (setf *objects* (make-hash-table)))

(defun read-orbits (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          collect (mapcar #'intern (cl-ppcre:split #\) line)))))

(defun make-object (name)
  (let ((o (make-instance 'orbits :name name)))
    (unless (gethash name *objects*)
      (setf (gethash name *objects*) o))
    o))

(defun set-object (destination source)
  (let ((dest (gethash destination *objects*))
        (src (gethash source *objects*)))
    (unless dest
      (setf dest (make-object destination)))
    (unless src
      (setf src (make-object source)))
    (unless (member src (children dest))
      (push src (children dest)))
    (unless (parent src)
      (setf (parent src) dest))))

(defun set-all-objects (filename)
  (clear-objects)
  (let ((orbits (read-orbits filename)))
    (mapcar (lambda (x) (apply #'set-object x)) orbits)))

(defun traverse-orbits (node depth)
  (+ depth
     (loop for child in (children node)
           summing (traverse-orbits child (1+ depth)))))

(defun shortest-path (current transfers)
  (unless (= (distance current) most-positive-fixnum)
      (return-from shortest-path (+ (distance current) transfers)))
  (if (> (distance current) transfers)
      (setf (distance current) transfers))
  (if (parent current)
      (shortest-path (parent current) (1+ transfers))))

(defun part-one ()
  (list 'part-one (traverse-orbits
                   (gethash *center-of-mass* *objects*) 0)))

(defun part-two ()
  ;; Start with negative transfers since we want it to be zero indexed
  (progn (shortest-path (gethash 'you *objects*) -1)
         (list 'part-two (shortest-path
                          (gethash 'san *objects*) -1))))

(defun day6 ()
  (set-all-objects "./day6-input.txt")
  (values (part-one)
          (part-two)))
