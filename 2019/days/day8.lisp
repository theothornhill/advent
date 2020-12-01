
(defun parse-layers (filename)
  (with-open-file (file filename :direction :input)
    (read-line file nil nil)))

(defun split-to-layers (filename)
  ;; picture-size: (* 25 6) = 150
  (cl-ppcre:all-matches-as-strings "[0-2]{150}"
                                   (parse-layers filename)))

(defun fewest-zeroes (filename)
  (let* ((layers (split-to-layers filename))
         ;; Just pick the first element first
         (fewest-zeroes (car layers))
         (number-of-zeroes (count #\0 fewest-zeroes)))
    (dolist (layer layers)
      (let ((zeroes-in-layer (count #\0 layer)))
        (when (< zeroes-in-layer number-of-zeroes)
          (setf fewest-zeroes layer)
          (setf number-of-zeroes zeroes-in-layer))))
    fewest-zeroes))

(defun multiply-ones-and-twos (line)
  (let ((ones (count #\1 line))
        (twos (count #\2 line)))
    (* ones twos)))

(defun decode-image (filename)
  (let ((layers (split-to-layers filename)))
    (loop with i = 0
          with str = (make-string 150)
          for c across (first layers)
          do (if (string= c #\2)
                 ;; We set the first number != 2, when searching through
                 ;; the layers.
                 (setf (aref str i) (find-color-in-layer layers i))
                 ;; Else just set the color.
                 (setf (aref str i) c))
             (incf i)
          finally (return (render-picture str)))))

(defun find-color-in-layer (layers position)
  "Find char at POSITION in every layer in LAYERS, then return the first
character not equal to 2."
  (let ((chopped-layers (mapcar (lambda (layer) (aref layer position))
                                layers)))
    (find-if (lambda (x) (or (string= #\1 x)
                        (string= #\0 x)))
             chopped-layers)))

(defun render-picture (string)
  (substitute #\Space #\0 (substitute #\# #\1 string)))


(defun day8 ()
  (format t "~%~a~%" (list 'part-one (multiply-ones-and-twos (fewest-zeroes "./inputs/day8-input.txt"))))
  (format t "~%~{~a~%~}~%" (cl-ppcre:all-matches-as-strings
                            "[#| ]{25}"
                            (decode-image "./inputs/day8-input.txt"))))
