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
     (= (controlar-aridad '(1 2 3) 2) (list '*error* 'too-many-args)))))

(deftest controlar-aridad-menor-test
  (testing "controlar aridad menor"
    (is
     (= (controlar-aridad '(1 2 3) 4) (list '*error* 'too-few-args)))))

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
     (= (igual? '(a b c) '(A B C)) true))))

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
     (= (igual? () ()) true))))

(deftest igual?-empty-list-and-nil-equal-test
  (testing "Controlar nil y lista vacia"
    (is
      (= (igual? () nil) false))))

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
      (= (igual? 'NIL ()) true))))

(deftest igual?-nil5
  (testing "Controlar () con '(nil)"
    (is
      (= (igual? () '(nil)) false))))


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
      (= (error? ()) false))))

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
      (= (revisar-lae nil) nil))))


(deftest revisar-lae-lista-numeros
  (testing "Controlar '(1 2 3)"
    (is
      (= (revisar-lae '(1 2 3)) nil))))


(deftest revisar-lae-parentesis
  (testing "Controlar lista vacia"
    (is
      (= (revisar-lae ()) nil))))


(deftest revisar-lae-un-error
  (testing "Controlar '(1 (*error* too-few-args) 3)"
    (is
      (= (revisar-lae '(1 (*error* too-few-args) 3)) '(*error* too-few-args)))))


(deftest revisar-lae-varios-errores
  (testing "Controlar '(1 (*error* too-few-args) (*error* too-many-args) 3)"
    (is
      (= (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3)) '(*error* too-few-args)))))

(deftest actualizar-amb-cargar
  (testing "Controlar '(a 1 b 2 c 3) 'd 4"
    (is
      (= (actualizar-amb '(a 1 b 2 c 3) 'd 4) '(a 1 b 2 c 3 d 4)))))


(deftest actualizar-amb-reemplazar
  (testing "Controlar '(a 1 b 2 c 3) 'b 4"
    (is
      (= (actualizar-amb '(a 1 b 2 c 3) 'b 4) '(a 1 b 4 c 3)))))


(deftest actualizar-amb-lista-error
  (testing "Controlar '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho)"
    (is
      (= (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho)) '(a 1 b 2 c 3)))))


(deftest actualizar-amb-lista-vacia
  (testing "Controlar () 'b 7"
    (is
      (= (actualizar-amb () 'b 7) '(b 7)))))


(deftest buscar-valor-que-esta
  (testing "Controlar 'c '(a 1 b 2 c 3 d 4 e 5)"
    (is
      (= (buscar 'c '(a 1 b 2 c 3 d 4 e 5)) 3))))
    

(deftest fnc-append-pocos-args
  (testing "Controlar '((1 2))"
    (is
      (= (fnc-append '((1 2))) '(*error* too-few-args)))))


(deftest fnc-append-muchos-args
  (testing "Controlar ((1 2) (3) (4 5) (6 7))"
    (is
      (= (fnc-append '((1 2) (3) (4 5) (6 7))) '(*error* too-many-args)))))


(deftest fnc-append-uno-es-numero
  (testing "Controlar '((1 2) 3)"
    (is
      (= (fnc-append '((1 2) 3)) '(*error* list expected 3)))))


(deftest fnc-append-uno-es-letra
  (testing "Controlar '((1 2) A)"
    (is
      (= (fnc-append '((1 2) A)) '(*error* list expected A)))))


(deftest fnc-append-dos-listas
  (testing "Controlar '((1 2) (3))"
    (is
      (= (fnc-append '((1 2) (3))) '(1 2 3)))))


(deftest fnc-append-lista-y-nil
  (testing "Controlar '((1 2) nil)"
    (is
      (= (fnc-append '((1 2) nil)) '(1 2)))))


(deftest fnc-append-lista-vacia-y-lista
  (testing "Controlar '(() (1 2))"
    (is
      (= (fnc-append '(() (1 2))) '(1 2)))))


(deftest fnc-append-nil-y-nil
  (testing "Controlar '(nil nil)"
    (is
      (= (fnc-append '(nil nil)) nil))))


(deftest fnc-append-dos-listas-vacias
  (testing "Controlar '(() ())"
    (is
      (= (fnc-append '(() ())) nil))))

    
(deftest fnc-env-dos-listas
  (testing "Controlar () '(a 1 b 2) '(c 3 d 4)"
    (is
      (= (fnc-env () '(a 1 b 2) '(c 3 d 4)) '(a 1 b 2 c 3 d 4)))))


(deftest fnc-env-muchos-args
  (testing "Controlar '(5) '(a 1 b 2) '(c 3 d 4)"
    (is
      (= (fnc-env '(5) '(a 1 b 2) '(c 3 d 4)) '(*error* too-many-args)))))


(deftest fnc-equal-numeros-iguales
  (testing "Controlar '(1 1)"
    (is
      (= (fnc-equal ''(1 1)) 't))))


(deftest fnc-equal-simbolos-upper-y-lower-case
  (testing "Controlar '(A a)"
    (is
      (= (fnc-equal '(A a)) 't))))


(deftest fnc-equal-strings-que-contienen-numeros
  (testing "Controlar '('1' '1')"
    (is
      (= (fnc-equal '("1" "1")) 't))))


(deftest fnc-equal-nil-y-NIL
  (testing "Controlar '(nil NIL)"
    (is
      (= (fnc-equal '(nil NIL)) 't))))


(deftest fnc-equal-numeros-diferentes
  (testing "Controlar '(1 2)"
    (is
      (= (fnc-equal '(1 2)) nil))))


(deftest fnc-equal-simbolos-diferentes
  (testing "Controlar '(A B)"
    (is
      (= (fnc-equal '(A B)) nil))))


(deftest fnc-equal-string-y-numero
  (testing "Controlar ('1' 1)"
    (is
      (= (fnc-equal '("1" 1)) nil))))


(deftest fnc-equal-lista-vacia
  (testing "Controlar ()"
    (is
      (= (fnc-equal ())) '(*error* too-few-args))))

    
(deftest fnc-env-lista-un-elemento
  (testing "Controlar '(A)"
    (is
      (= (fnc-env '(A)) '(*error* too-few-args)))))


(deftest fnc-env-lista-tres-elementos
  (testing "Controlar '(A a A)"
    (is
      (= (fnc-env '(A a A)) '(*error* too-many-args)))))
