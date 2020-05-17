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
   (memory-size
    :initarg :memory-size
    :accessor memory-size)
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

(defparameter *increment-counter* 4)

(defun actions (instruction)
  (ecase instruction
    (1 'add)
    (2 'mult)
    (99 'halt)))

(defun get-next-action ()
  (with-intcode (memory instruction-pointer)
    (actions (aref memory instruction-pointer))))

(defmacro def-instruction (name operation)
  `(defun ,name ()
    (with-intcode (instruction-pointer memory)
      (let ((x (+ instruction-pointer 1))
            (y (+ instruction-pointer 2))
            (z (+ instruction-pointer 3)))
        (incf instruction-pointer *increment-counter*)
        (setf (aref memory (aref memory z))
              (,operation (aref memory (aref memory x))
                 (aref memory (aref memory y))))))))

(def-instruction add +)
(def-instruction mult *)

(defun halt () :halt)

(defun init-intcode (filename)
  (let* ((op-codes (read-op-codes filename))
         (mem-size (length op-codes)))
    (with-intcode (memory instruction-pointer memory-size)
      (setf memory-size mem-size)
      (setf memory (make-array mem-size :initial-contents op-codes))
      :initialized)))

(defun run-intcode ()
  "Makes an assumption that memory is set, either from INIT-INTCODE or manually."
  (with-intcode (memory instruction-pointer)
    (setf instruction-pointer 0)
    (do* ((action (get-next-action) (get-next-action)))
         ((eql action 'halt) (aref memory 0))
      (funcall action))))

