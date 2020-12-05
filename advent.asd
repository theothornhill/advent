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
                             (:file "utils")
                             (:module "days"
                              :components ((:file "day1")
                                           (:file "day2")
                                           (:file "day3")
                                           (:file "day4"))))))
  :in-order-to ((test-op (test-op "advent/tests"))))

(asdf:defsystem :advent/tests
  :description "Tests for advent of code"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (:rove
               :advent)
  :components ((:module "2020"
                :components ((:module "tests"
                              :components ((:file "package")
                                           (:file "2020-tests"))))))
  :perform (test-op (o c) (symbol-call :rove '#:run :advent/tests)))
