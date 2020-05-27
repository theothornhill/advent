
(in-package :intcode)

(defparameter *a* (make-instance 'intcode))
(defparameter *b* (make-instance 'intcode))
(defparameter *c* (make-instance 'intcode))
(defparameter *d* (make-instance 'intcode))
(defparameter *e* (make-instance 'intcode))

(defparameter *computers* (list *a* *b* *c* *d* *e*))

(defun run-threaded (a b)
  (bt:make-thread (lambda ()
                    (run-with-input a b))
                  :name (format nil "~a" a)))

(defun add-settings (args)
  ;; Set the first args on *e* since it feeds *a* anyways. Ugly...
  (setf (out *e*) (list 0 (first args)))
  (setf (out *a*) (list (second args)))
  (setf (out *b*) (list (third args)))
  (setf (out *c*) (list (fourth args)))
  (setf (out *d*) (list (fifth args))))

(defun run-day7 (&rest args)
  (map nil (lambda (computer)
             (init-intcode "./inputs/day7-input.txt" computer))
       *computers*)
  (add-settings args)
  (let ((threads
          (list (run-threaded *a* *e*)
                (run-threaded *b* *a*)
                (run-threaded *c* *b*)
                (run-threaded *d* *c*)
                (run-threaded *e* *d*))))
    (mapcar #'bt:join-thread threads)
    (car (out *e*))))

(defun all-permutations (list)
  ;; Stolen from Rainer Joswig stackoverflow
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (all-permutations (remove element list)))))))

(defun run-with-permutations (&rest args)
  (let (result)
    (mapcar (lambda (x) (push (apply #'run-day7 x) result))
            (all-permutations args))
    (car (sort result #'>))))

(defun day7 ()
  (format t "Running day 7... ")
  (values (run-with-permutations 0 1 2 3 4)
          (run-with-permutations 5 6 7 8 9)))
