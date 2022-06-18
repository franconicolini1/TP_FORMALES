(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest controlar-aridad-igual-test
  (testing "controlar aridad igual"
    (is
     (= (controlar-aridad '(1 2 3) 3) 3))))

(deftest controlar-aridad-mayor-test
  (testing "controlar aridad mayor"
    (is
     (= (controlar-aridad '(1 2 3) 2) '('*error* 'too-many-arguments)))))

(deftest controlar-aridad-menor-test
  (testing "controlar aridad menor"
    (is
     (= (controlar-aridad '(1 2 3) 4) '('*error* 'too-few-args)))))

(deftest igual?-numbers-equal-test
  (testing "controlar aridad igual"
    (is
     (= (igual? 1 1) true))))

(deftest igual?-numbers-not-equal-test
  (testing "Controlar numeros diferentes"
    (is
     (= (igual? 1 2) false))))

(deftest igual?-simbols-equal-lower-test
  (testing "Controlar simbolor iguales en lowerCase"
    (is
     (= (igual? 'a 'a) true))))

(deftest igual?-simbols-equal-upper-test
  (testing "Controlar simbolos iguales en upperCase"
    (is
     (= (igual? 'A 'A) true))))

(deftest igual?-simbols-equal-lower-upper-test
  (testing "Controlar simbolos iguales en lowerCase y upperCase"
    (is
     (= (igual? 'a 'A) true))))

(deftest igual?-simbols-equal-upper-lower-test
  (testing "Controlar simbolos iguales en upperCase y lowerCase"
    (is
     (= (igual? 'A 'a) true))))

(deftest igual?-simbols-not-equal-lower-test
  (testing "Controlar simbolos diferentes en lowerCase"
    (is
     (= (igual? 'a 'b) false))))

(deftest igual?-listas-equal-lower-upper-test
  (testing "Controlar lista iguales en lowerCase y upperCase"
    (is
     (= (igual? '('a 'b 'c) '('A 'B 'C)) true))))

(deftest igual?-simbols-not-equal-lower-test
  (testing "Controlar listas diferentes en lowerCase y upperCase"
    (is
     (= (igual? '(a b c) '(A B D)) false))))

(deftest igual?-simbols-not-equal-lower-test
  (testing "Controlar nils iguales"
    (is
     (= (igual? nil nil) true))))

; user=> (igual? nil 'NIL)
; true
; user=> (igual? 'NIL nil)
; true
; user=> (igual? 'NIL 'NIL)
; true
; user=> (igual? nil ())
; true
; user=> (igual? 'NIL ())
; true
; user=> (igual? () ())
; true
; user=> (igual? () '(nil))
; false
; user=> (igual? "a" "a")
; true
; user=> (igual? "a" "A")
; false
; user=> (igual? 'a "a")
; false
; user=> (igual? 'a "A")
; false