
(in-package :intcode)

(defparameter *a* (make-instance 'intcode))
(defparameter *b* (make-instance 'intcode))
(defparameter *c* (make-instance 'intcode))
(defparameter *d* (make-instance 'intcode))
(defparameter *e* (make-instance 'intcode))

(defun run-day7-part-one (a b c d e &rest args)
  (let ((computers (list a b c d e)))
    (map nil (lambda (computer arg)
               (init-intcode "./day7-input.txt" computer arg))
         computers args)
    (run-with-input a a 0)
    (run-with-input b a)
    (run-with-input c b)
    (run-with-input d c)
    (run-with-input e d)))


(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun run-with-input (a b &optional (hardcode nil))
  (let ((result 0))
    (with-intcode a
      (with-instruction-pointer
        (let ((action (next-instruction)))
          (funcall action (setting a)) ;; do first read instruction
          (do ((action (next-instruction) (next-instruction)))
              ((eql action 'halt)
               (setf result (get-mem 0 (1- instruction-pointer))))
            (cond ((eql action 'input)
                   (cond ((out-buffer b)
                          (input (pop (out-buffer b))))
                         (hardcode
                          (input hardcode)
                          (setf hardcode nil))
                         (t
                          (return))))
                  ((eql action 'output)
                   (output nil))
                  (t (funcall action)))))))))

(defun day7-part-one ()
  (let (result)
    (mapcar (lambda (x) (push (apply #'run-day7-part-one
                                *a* *b* *c* *d* *e* x)
                         result))
            (all-permutations '(0 1 2 3 4)))
    (car (sort result #'>))))
