(in-package :advent/tests)

(deftest day1-part-1
  (testing "Day one part 1 should return correct number"
    (ok (= (is-it-2020?-part-1) 703131))))

(deftest day1-part-2
  (testing "Day one part 2 should return correct number"
    (ok (= (is-it-2020?-part-2) 272423970))))


(deftest day2-part-1
  (testing "Day two part 1 should return correct number"
    (ok (= (count-valid-passwords-part-1) 645))))

(deftest day2-part-2
  (testing "Day two part 2 should return correct number"
    (ok (= (count-valid-passwords-part-2) 737))))


(deftest day3-part-1
  (testing "Day three part 1 should return correct number"
    (ok (= (crashed-in-tree-part-1) 211))))

(deftest day3-part-2
  (testing "Day three part 2 should return correct number"
    (ok (= (crashed-in-tree-part-2) 3584591857))))
