
(asdf:defsystem :advent
  :description "Advent of Code"
  :author "Theodor Thornhill <theothornhill@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:cl-ppcre :bordeaux-threads)
  :components ((:file "package")
               (:file "intcode")
               (:module "days"
                :components ((:file "day1")
                             (:file "day3")
                             (:file "day4")
                             (:file "day6")
                             (:file "day7")))))
