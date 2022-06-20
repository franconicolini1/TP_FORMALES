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

(deftest igual?-empty-lists-equal-test
(testing "Controlar listas vacias"
  (is
    (= (igual? '() '()) true))))

(deftest igual?-empty-list-and-nil-equal-test
(testing "Controlar nil y lista vacia"
  (is
    (= (igual? '() nil) false))))

(deftest igual?-nil1
  (testing "Controlar nil and 'NIL"
    (is
      (= (igual? nil 'NIL) true))))

(deftest igual?-nil2
  (testing "Controlar 'NIL and nil"
    (is
      (= (igual? 'NIL nil) true))))

(deftest igual?-nil3
  (testing "Controlar 'NIL con 'NIL"
    (is
      (= (igual? 'NIL 'NIL) true))))

(deftest igual?-nil4
  (testing "Controlar 'NIL con ()"
    (is
      (= (igual? 'NIL '()) true))))

(deftest igual?-nil5
  (testing "Controlar () con '(nil)"
    (is
      (= (igual? '() '(nil)) false))))


(deftest igual?-string-lower
  (testing "Controlar string a con a"
    (is
      (= (igual? "a" "a") true))))

(deftest igual?-string-lower-upper
  (testing "Controlar string a con A"
    (is
      (= (igual? "a" "A") false))))

(deftest igual?-simbol-lower-string
  (testing "Controlar 'a con string a"
    (is
      (= (igual? 'a "a") false))))

(deftest igual?-simbol-upper-string
  (testing "Controlar 'a con A"
    (is
      (= (igual? 'a "A") false))))

(deftest error?-*error*-too-few-args
  (testing "Controlar '(*error* too-few-args)"
    (is
      (= (error? '(*error* too-few-args)) true))))

(deftest error?-list-*error*-too-few-args
  (testing "Controlar (list '*error* 'too-few-args)"
    (is
      (= (error? (list '*error* 'too-few-args)) true))))

(deftest error?-list-*ERROR*-too-few-args
  (testing "Controlar (list '*ERROR* 'too-few-args)"
    (is
      (= (error? (list '*ERROR* 'too-few-args)) true))))

(deftest error?-lista-error
  (testing "Controlar (list '*Error* 'too-few-args)"
    (is
      (= (error? (list '*Error* 'too-few-args)) true))))

(deftest error?-list-*error*
  (testing "Controlar list '*error*"
    (is
      (= (error? (list '*error*)) true))))

(deftest error?-too-few-args
  (testing "Controlar too-few-args"
    (is
      (= (error? (list 'too-few-args)) false))))

(deftest error?-parentesis
  (testing "Controlar ()"
    (is
      (= (error? '()) false))))

(deftest error?-*error*
  (testing "Controlar '*error*"
    (is
      (= (error? '*error*) false))))

(deftest error?-nil
  (testing "Controlar nil"
    (is
      (= (error? nil) false))))


(deftest revisar-fnc-*error*-too-few-args
  (testing "Controlar (*error* too-few-args)"
    (is
      (= (revisar-fnc '('*error* 'too-few-args)) ('*error* 'too-few-args)))))


(deftest revisar-fnc-too-few-args
  (testing "Controlar '(too-few-args)"
    (is
      (= (revisar-fnc '(too-few-args)) nil))))

(deftest revisar-fnc-*error*
  (testing "Controlar '*error*"
    (is
      (= (revisar-fnc '*error*) nil))))


(deftest revisar-fnc-nil
  (testing "Controlar nil"
    (is
      (= (revisar-fnc nil) nil))))


(deftest revisar-fnc-lista-vacia
  (testing "Controlar lista vacia"
    (is
      (= (revisar-fnc ()) nil))))


(deftest revisar-lae-nil
  (testing "Controlar nil"
    (is
      (= (revisar-fnc nil) nil))))


(deftest revisar-fnc-lista-numeros
  (testing "Controlar '(1 2 3)"
    (is
      (= (revisar-fnc '(1 2 3)) nil))))


(deftest revisar-lae-parentesis
  (testing "Controlar lista vacia"
    (is
      (= (revisar-fnc ()) nil))))


(deftest revisar-fnc-un-error
  (testing "Controlar '(1 (*error* too-few-args) 3)"
    (is
      (= (revisar-lae '(1 (*error* too-few-args) 3)) '(*error too-few-args)))))


(deftest revisar-lae-varios-errores
  (testing "Controlar '(1 (*error* too-few-args) (*error* too-many-args) 3)"
    (is
      (= (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3)) '(*error* too-few-args)))))
