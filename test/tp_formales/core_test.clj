(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest controlar-aridad-igual-test
  (testing "controlar aridad igual"
    (is 
      (= (controlar-aridad-igual '(1 2 3) 3) 3)
    )
  )
)
