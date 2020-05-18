;;; IntCode computer

(ql:quickload :cl-ppcre)
(defpackage :intcode
  (:use :cl)
  (:import-from :cl-ppcre
                :split))

(in-package :intcode)

(defclass intcode ()
  ((memory
    :initarg :memory
    :accessor memory)
   (instruction-pointer
    :initarg :instruction-pointer
    :initform 0
    :accessor instruction-pointer)))

(defvar *intcode* (make-instance 'intcode))

(defun restart-computer ()
  (setf *intcode* (make-instance 'intcode)))

(defun read-op-codes (filename)
  (with-open-file (stream filename)
    (mapcar #'parse-integer (split "," (read-line stream nil nil)))))

(defmacro with-intcode ((&rest rest) &body body)
  "Macro with hardcoded INTCODE instance. For convenience."
  `(with-slots ,rest *intcode*
     ,@body))

(defun parse-instruction (instruction)
  (let ((i (format nil "~5,'0d" instruction)))
    ;; KLUDGE: This thing does too many things.
    ;; Handle case for halt value better, and maybe split the mv-bind?
    (values (if (string= (subseq i 3) "99")
                99
                (parse-integer (subseq i 4)))
            (reverse (mapcar #'parse-integer
                             (split "" (subseq i 0 3)))))))

(defmacro with-modes (&body body)
  "Anaphoric MODES variables"
  `(let* ((modes (nth-value
                  1 (parse-instruction
                     (get-mem 1 (instruction-pointer *intcode*)))))
          (first-param-mode (car modes))
          (second-param-mode (cadr modes)))
     ,@body))

(defmacro with-x (&body body)
  "Anaphoric X value"
  `(let ((x (+ (instruction-pointer *intcode*) 1)))
     ,@body))

(defmacro with-xy (&body body)
  "Anaphoric X Y values"
  `(let ((x (+ (instruction-pointer *intcode*) 1))
         (y (+ (instruction-pointer *intcode*) 2)))
     ,@body))

(defmacro with-xyz (&body body)
  "Anaphoric X Y Z values"
  `(let ((x (+ (instruction-pointer *intcode*) 1))
         (y (+ (instruction-pointer *intcode*) 2))
         (z (+ (instruction-pointer *intcode*) 3)))
     ,@body))

(defun instruction (instruction)
  (ecase (parse-instruction instruction)
    (1 'add)
    (2 'mult)
    (3 'input)
    (4 'output)
    (5 'jump-if-true)
    (6 'jump-if-false)
    (7 'less-than)
    (8 'equals)
    (99 'halt)))

(defun next-instruction ()
  (with-intcode (memory instruction-pointer)
    (instruction (aref memory instruction-pointer))))

(defun (setf set-mem) (value position)
  (with-intcode (memory)
    (setf (aref memory (aref memory position)) value)))

(defun get-mem (mode position)
  (with-intcode (memory)
    (if (zerop mode)
        (aref memory (aref memory position))
        (aref memory position))))

(defmacro with-instruction-pointer (&body body)
  `(with-intcode (instruction-pointer)
     ,@body))

(defmacro with-binary-expr (&body body)
  `(with-instruction-pointer
     (with-xyz
       (with-modes
         ,@body
         (incf instruction-pointer 4)))))

(defmacro with-jump (&body body)
  `(with-instruction-pointer
     (with-xy
       (with-modes
         ,@body
         (incf instruction-pointer 3)))))

(defmacro with-io (&body body)
  `(with-instruction-pointer
     (with-x
       ,@body
       (incf instruction-pointer 2))))

(defun add ()
  (with-binary-expr
    (setf (set-mem z) (+ (get-mem first-param-mode x)
                         (get-mem second-param-mode y)))))

(defun mult ()
  (with-binary-expr
    (setf (set-mem z) (* (get-mem first-param-mode x)
                         (get-mem second-param-mode y)))))

(defun input ()
  (with-io
    (setf (set-mem x) (parse-integer (read-line)))))

(defun output ()
  (with-io
    (print (get-mem 0 x))))

(defun jump-if-true ()
  (with-jump
    (unless (zerop (get-mem first-param-mode x))
      (setf instruction-pointer (get-mem second-param-mode y))
      (return-from jump-if-true))))

(defun jump-if-false ()
  (with-jump
    (when (zerop (get-mem first-param-mode x))
      (setf instruction-pointer (get-mem second-param-mode y))
      (return-from jump-if-false))))

(defun less-than ()
  (with-binary-expr
    (if (< (get-mem first-param-mode x)
           (get-mem second-param-mode y))
        (setf (set-mem z) 1)
        (setf (set-mem z) 0))))

(defun equals ()
  (with-binary-expr
    (if (= (get-mem first-param-mode x)
           (get-mem second-param-mode y))
        (setf (set-mem z) 1)
        (setf (set-mem z) 0))))

(defun halt () :halt)

(defun init-intcode (filename)
  (let* ((op-codes (read-op-codes filename))
         (mem-size (length op-codes)))
    (with-intcode (memory instruction-pointer)
      (setf instruction-pointer 0)
      (setf memory (make-array mem-size :initial-contents op-codes))
      :initialized)))

(defun run-intcode ()
  "Makes an assumption that memory is set, either from INIT-INTCODE or manually."
  (with-intcode (memory instruction-pointer)
    (setf instruction-pointer 0)
    (do* ((action (next-instruction) (next-instruction)))
         ((eql action 'halt) (aref memory 0))
      (funcall action))))

