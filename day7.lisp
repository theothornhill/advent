
(in-package :intcode)

(defun run (&rest args)
  (init-intcode "./day7-input.txt")
  (let ((result 0))
    (dolist (arg args result)
      (with-intcode (instruction-pointer)
        (setf instruction-pointer 0)
        (let ((action (next-instruction)))
          (funcall action arg) ;; do first read instruction
          (do ((action (next-instruction) (next-instruction)))
              ((eql action 'halt)
               (setf result (get-mem 0 (1- instruction-pointer))))
            (cond ((eql action 'input)
                   (funcall action result))
                  ((eql action 'output)
                   (funcall action nil))
                  (t (funcall action)))))))))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun day7-part-one ()
  (let (result)
    (mapcar (lambda (x) (push (apply #'run x) result))
            (all-permutations '(0 1 2 3 4)))
    (car (sort result #'>))))


