(in-package #:advent-2020)

(defvar *day4-file* "2020/inputs/day4.txt")

(defun check-required-fields (passport)
  (and
   (assoc '|byr| passport)
   (assoc '|ecl| passport)
   (assoc '|eyr| passport)
   (assoc '|hcl| passport)
   (assoc '|hgt| passport)
   (assoc '|iyr| passport)
   (assoc '|pid| passport)
   t))

(defun valid-byr-p (passport)
  (let ((year (parse-integer (cdr (assoc '|byr| passport)))))
    (<= 1920 year 2002)))

(defun valid-iyr-p (passport)
  (let ((year (parse-integer (cdr (assoc '|iyr| passport)))))
    (<= 2010 year 2020)))

(defun valid-eyr-p (passport)
  (let ((year (parse-integer (cdr (assoc '|eyr| passport)))))
    (<= 2020 year 2030)))

(defun valid-hgt-p (passport)
  (let* ((val (cdr (assoc '|hgt| passport)))
         (num (read-from-string (cl-ppcre:scan-to-strings "[0-9]*" val)))
         (postfix (subseq val (- (length val) 2))))
    (unless (numberp (read-from-string postfix))
      (if (string= postfix "in")
          (<= 59 num 79)
          (<= 150 num 193)))))

(defun valid-hcl-p (passport)
  (let ((color (cdr (assoc '|hcl| passport))))
    (and
     (string= (aref color 0) "#")
     (cl-ppcre:all-matches "[a-f0-9]" (subseq color 1))
     t)))

(defun valid-ecl-p (passport)
  (let ((color (cdr (assoc '|ecl| passport))))
    (and
     (find color '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")  :test #'string=)
     t)))

(defun valid-pid-p (passport)
  (let ((id (cdr (assoc '|pid| passport))))
    (and
     (= (length id) 9)
     (numberp (read-from-string id)))))

(defun valid-part-2-passport-p (passport)
  (and
   (check-required-fields passport)
   (valid-byr-p passport)
   (valid-iyr-p passport)
   (valid-eyr-p passport)
   (valid-hgt-p passport)
   (valid-hcl-p passport)
   (valid-ecl-p passport)
   (valid-pid-p passport)
   t))

(defun alist (string)
  (let ((split (cl-ppcre:split ":" string)))
    (cons (intern (first split)) (second split))))

(defun alists (strings)
  (mapcar #'alist strings))

(defun generate-passports ()
  (mapcar #'alists
   (mapcar #'split-by-space (slurp *day4-file*))))

(defun valid-passport-count-part-1 ()
  (count-if-not #'null (mapcar #'check-required-fields (generate-passports))))

(defun valid-passport-count-part-2 ()
  (count-if-not #'null (mapcar #'valid-part-2-passport-p (generate-passports))))
