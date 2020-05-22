;;; IntCode computer

(ql:quickload :cl-ppcre)
(defpackage :intcode
  (:use :cl)
  (:import-from :cl-ppcre
                :split))

(in-package :intcode)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (sym) `(,sym (gensym))) syms)
     ,@body))

(defclass intcode ()
  ((memory
    :initarg :memory
    :accessor memory)
   (setting
    :initarg :setting
    :accessor setting)
   (out-buffer
    :initarg :out-buffer
    :initform (list)
    :accessor out-buffer)
   (instruction-pointer
    :initarg :instruction-pointer
    :initform 0
    :accessor instruction-pointer)))

(defvar *intcode*)

(defun restart-computer (&optional (intcode *intcode*))
  (setf intcode (make-instance 'intcode)))

(defun read-op-codes (filename)
  (with-open-file (stream filename)
    (mapcar #'parse-integer (split "," (read-line stream nil nil)))))

(defun parse-instruction (instruction)
  (let ((i (format nil "~5,'0d" instruction)))
    ;; KLUDGE: This thing does too many things.
    ;; Handle case for halt value better, and maybe split the mv-bind?
    (values (if (string= (subseq i 3) "99")
                99
                (parse-integer (subseq i 4)))
            (reverse (mapcar #'parse-integer
                             (split "" (subseq i 0 3)))))))

(defmacro with-intcode (intcode &body body)
  `(let ((*intcode* ,intcode))
     ,@body))

(defmacro with-internals (slots &body body)
  `(with-slots ,slots *intcode*
     ,@body))

(defmacro with-modes (&body body)
  "Anaphoric MODES variables"
  `(let* ((modes (nth-value
                  1 (parse-instruction
                     (get-mem 1 (instruction-pointer *intcode*)))))
          (first-param-mode (car modes))
          (second-param-mode (cadr modes)))
     (declare (ignorable modes first-param-mode second-param-mode))
     ,@body))

(defmacro with-positions (args &body body)
  (let ((pointer 0))
    `(let ,(mapcar (lambda (arg)
                     `(,arg (+ (instruction-pointer *intcode*)
                               ,(incf pointer))))
            args)
       (declare (ignorable ,@args))
       ,@body)))

(defmacro with-instruction-pointer (&body body)
  `(with-internals (instruction-pointer)
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
  (with-internals (memory instruction-pointer)
    (instruction (aref memory instruction-pointer))))

(defun (setf set-mem) (value position)
  (with-internals (memory)
    (setf (aref memory (aref memory position)) value)))

(defun get-mem (mode position)
  (with-internals (memory)
    (if (zerop mode)
        (aref memory (aref memory position))
        (aref memory position))))

(defmacro with-binary-expr (&body body)
  `(with-instruction-pointer
     (with-positions (x y z)
       (with-modes
         ,@body
         (incf instruction-pointer 4)))))

(defmacro with-jump (&body body)
  `(with-instruction-pointer
     (with-positions (x y)
       (with-modes
         ,@body
         (incf instruction-pointer 3)))))

(defmacro with-io (&body body)
  `(with-instruction-pointer
     (with-positions (x)
       ,@body
       (incf instruction-pointer 2))))

(defmacro jump-result (name clause)
  `(,clause (zerop (get-mem first-param-mode x))
            (setf instruction-pointer (get-mem second-param-mode y))
            (return-from ,name)))

(defmacro binary-result (operation)
  `(,operation (get-mem first-param-mode x)
               (get-mem second-param-mode y)))

;;; Instruction set

(defun add ()
  (with-binary-expr
    (setf (set-mem z) (binary-result +))))

(defun mult ()
  (with-binary-expr
    (setf (set-mem z) (binary-result *))))

(defun input (in)
  (with-io
    (setf (set-mem x) (if in in (parse-integer (read-line))))))

(defun output (&optional (stream t))
  (with-io
    (with-internals (out-buffer)
      (if (out-buffer *intcode*)
          (push (get-mem 0 x) (cdr (last (out-buffer *intcode*))))
          (push (get-mem 0 x) (out-buffer *intcode*)))
      (format stream "~a" (get-mem 0 x)))))

(defun jump-if-true ()
  (with-jump
    (jump-result jump-if-true unless)))

(defun jump-if-false ()
  (with-jump
    (jump-result jump-if-false when)))

(defun less-than ()
  (with-binary-expr
    (if (binary-result <)
        (setf (set-mem z) 1)
        (setf (set-mem z) 0))))

(defun equals ()
  (with-binary-expr
    (if (binary-result =)
        (setf (set-mem z) 1)
        (setf (set-mem z) 0 ))))

(defun halt () :halt)

(defun init-intcode (filename intcode &optional (start nil))
  (with-intcode intcode
    (let* ((op-codes (read-op-codes filename))
           (mem-size (length op-codes)))
      (with-internals (memory setting out-buffer instruction-pointer)
        (setf instruction-pointer 0)
        (setf setting start)
        (setf out-buffer nil)
        (setf memory (make-array mem-size :initial-contents op-codes))
        :initialized))))

(defun run (intcode)
  "RUN dynamically binds an instance of INTCODE. "
  (with-intcode intcode
    (with-internals (memory instruction-pointer)
      (setf instruction-pointer 0)
      (do* ((action (next-instruction) (next-instruction)))
           ((eql action 'halt) (aref memory 0))
        (funcall action)))))
