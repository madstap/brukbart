(ns brukbart.core-test
  (:require [clojure.test :refer :all]
            [brukbart.core :refer :all]))

(deftest vars->map-test
  (testing "locals"
    (let [a 10 b 20 c 30]
      (is (= (vars->map a b c) {:a 10 :b 20 :c 30}))))

  (testing "globals"
    (do (def foo "bar")
        (def baz "quux")
        (is (= (vars->map foo baz) {:foo "bar" :baz "quux"}))))

  (testing "namespaced vars -> non namespaced keywords"
    (is (nil? (:clojure.core/juxt (vars->map clojure.core/juxt))))
    (is (= juxt   (:juxt (vars->map clojure.core/juxt))))))

(deftest levenshtein-test
  (testing ""
    (are [x str1 str2] (= x (levenshtein str1 str2))
      1 "asd" "gsd"
      1 "asd" "axd"
      1 "asd" "sd"
      2 "asd" "sdx"
      2 "asd" "sda"
      3 "das" "qwe"
      5 "asdfg" ""
      1 "" "x"
      6 "" "qwerty"
      )))

(deftest inject-indexed-test
  (testing  "Sequence arity"
    (is (= (list 0 :x 1 2 :x 3 4 :y 5 6 7 8 9)
           (inject-indexed {#{1 3} :x 5 :y} (range 10)))))

  (testing "Transducer arity"
    (is (= [0 1 2 :x 3 :x 4 5 6 7 8 9]
           (transduce (inject-indexed {#{3 4} :x}) conj [] (range 10)))))

  (testing "Str stuff"
    (is (= "165.107.985-49"
           (apply str (inject-indexed {3 \., 6 \., 9 \-} "16510798549"))))
    (is (= "92.576.596/0001-90"
           (apply str (inject-indexed {2 \., 5 \., 8 \/, 12 \-} "92576596000190")))))

  (testing "Works with sets as keys"
    (is (= "165.107.985-49"
           (apply str (inject-indexed {#{3 6} \., 9 \-} "16510798549"))))
    (is (= "92.576.596/0001-90"
           (apply str (inject-indexed {#{2 5} \., 8 \/, 12 \-}
                                      [9 2 5 7 6 5 9 6 0 0 0 1 9 0]))))))

(deftest interpose-indexed-test
  (testing "State doesn't leak"
    (is (let [xform (interpose-indexed :x #{1 2 3})
              a (into [] xform (range 10))
              b (into [] xform (range 10))]
          (= a b))))

  (testing "Transducer and sequence arities do the same thing."
    (is (= (into [] (interpose-indexed :x #{4 6 2}) (range 10))
           (into [] (interpose-indexed :x #{4 6 2} (range 10)))))))
