
(asdf:defsystem :advent
  :description "Advent of Code"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license  "GPLv3"
  :version "0.0.1"
  :depends-on (:cl-ppcre :bordeaux-threads)
  :components ((:module "2019"
                :components ((:file "package")
                             (:file "intcode")
                             (:module "days"
                              :components ((:file "day1")
                                           (:file "day3")
                                           (:file "day4")
                                           (:file "day6")
                                           (:file "day7")
                                           (:file "day8")))))
               (:module "2020"
                :components ((:file "package")
                             (:module "days"
                              :components ((:file "day1")))))))
