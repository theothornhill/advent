;; Day 2:

(in-package :intcode)

(defun run (filename noun verb)
  (with-intcode (memory)
    (init-intcode filename)
    (setf (aref memory 1) noun)
    (setf (aref memory 2) verb)
    (run-intcode)))

(defun find-result (filename target)
  (with-intcode (memory memory-size)
    (loop named r for noun below 100 do
      (loop for verb below 100 do
        (if (= (run filename noun verb) target)
            (return-from r (+ (* 100 noun) verb)))))))
