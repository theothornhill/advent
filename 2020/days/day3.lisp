(in-package #:advent-2020)

;; Only global vars mode

(defvar *day3-file* "2020/inputs/day3.txt")

(defparameter *trees* 0)
(defparameter *i* 0)
(defparameter *j* 0)
(defparameter *area* (apply #'vector (slurp *day3-file*)))
(defparameter *height* 322)
(defparameter *width* 30)
(defparameter *current-j-steps* 0)
(defvar *tree-sign* #\#)
(defvar *j-steps* '(1 3 5 7 1))
(defvar *i-steps* '(1 1 1 1 2))

(defparameter *j-step* 3)
(defparameter *i-step* 1)
(defvar *reset* 0)

(defparameter *round* 0)
(defvar *max-rounds* 5)

(defparameter *round-results* nil)

(defun crashed-in-tree-p ()
  (string= (aref (aref *area* *i*) *j*) *tree-sign*))

(defun walk ()
  (tagbody
   :again
     (if (= *j* *width*) (setf *j* *reset*) (incf *j*))
     (incf *current-j-steps*)
     (if (> *current-j-steps* (1- *j-step*)) (go :done) (go :again))
   :done)
  (incf *i* *i-step*)
  (when (crashed-in-tree-p) (incf *trees*))
  (setf *current-j-steps* *reset*))

(defun crashed-in-tree-part-1 ()
  (setf *j-step* 3 *i-step* 1)
  (tagbody
   :walk-again
     (walk)
     (if (= *i* *height*) (go :done) (go :walk-again))
   :done)
  *trees*)

;; part 2

(defun crashed-in-tree-part-2 ()
  (tagbody
   :initialize
     (setf *round* *reset*
           *round-results* nil
           *i* *reset*
           *j* *reset*)
     (go :set-new-steps)
   :set-new-steps
     (setf *trees* *reset*)
     (setf *i-step* (nth *round* *i-steps*)
           *j-step* (nth *round* *j-steps*))
     (incf *round*)
     (go :walk)
   :walk
     (walk)
     (if (= *i* *height*) (go :maybe-go-again) (go :walk))
   :maybe-go-again
     (push *trees* *round-results*)
     (setf *i* *reset*)
     (setf *j* *reset*)
     (if (= *round* *max-rounds*) (go :done) (go :set-new-steps))
   :done)
  (apply #'* *round-results*))
