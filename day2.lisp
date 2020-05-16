;; Day 2:

(defun read-op-codes (filename)
  (with-open-file (stream filename)
    (loop for num = (read-delimited-list #\, stream)
          while num
          append num)))

(defparameter *instruction-pointer* 0)
(defparameter *increment-counter* 4)

(defun actions (instruction)
  (ecase instruction
    (1 'add)
    (2 'mult)
    (99 'halt)))

(defmacro access (memory address)
  `(aref ,memory (aref ,memory ,address)))

(defun get-next-action (memory)
  (actions (aref memory *instruction-pointer*)))

(defmacro define-instruction (name operation)
  `(defun ,name (memory)
     (let ((x (+ *instruction-pointer* 1))
           (y (+ *instruction-pointer* 2))
           (z (+ *instruction-pointer* 3)))
       (incf *instruction-pointer* *increment-counter*)
       (setf (access memory z)
             (,operation (access memory x)
                         (access memory y))))))

(define-instruction add +)
(define-instruction mult *)

(defun halt (memory)
  (declare (ignorable memory))
  memory)

(defun run-computer (&rest args)
  (setf *instruction-pointer* 0)
  (do* ((memory (make-array (length args) :initial-contents args))
        (action (get-next-action memory) (get-next-action memory)))
       ((eql action 'halt) memory)
    (funcall action memory)))

(defun find-result (filename target)
  (let ((op-codes (read-op-codes filename)))
    (loop named r for noun below (length op-codes) do
      (loop for verb below (length op-codes) do
        (rplaca (cdr op-codes) noun)
        (rplaca (cddr op-codes) verb)
        (if (= (aref (apply #'computer op-codes) 0) target)
            (return-from r (+ (* 100 noun) verb)))))))

(assert (equalp (run-computer 1 0 0 0 99) #(2 0 0 0 99)))
(assert (equalp (run-computer 2 3 0 3 99) #(2 3 0 6 99)))
(assert (equalp (run-computer 2 4 4 5 99 0) #(2 4 4 5 99 9801)))
(assert (equalp (run-computer 1 1 1 4 99 5 6 0 99) #(30 1 1 4 2 5 6 0 99)))
