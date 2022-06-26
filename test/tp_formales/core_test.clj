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
     (= (fnc-equal '(1 1)) 't))))

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

(deftest fnc-equal-lista-un-elemento
  (testing "Controlar '(A)"
    (is
     (= (fnc-equal '(A)) '(*error* too-few-args)))))

(deftest fnc-equal-lista-tres-elementos
  (testing "Controlar '(A a A)"
    (is
     (= (fnc-equal '(A a A)) '(*error* too-many-args)))))



    ; user=> (fnc-read ())
; 1
; user=> (fnc-read ())
; a
; a
; user=> (fnc-read ())
; "hola"
; "hola"
; user=> (fnc-read ())
; (hola mundo)
; (hola mundo)
; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read ())
; ()
; nil
; user=> (fnc-read ())
; nil
; nil


;; (deftest fnc-read-numero
;;   (testing "Controlar 1"
;;     (is
;;      (= (fnc-read ()) 1))))

;; (deftest fnc-read-dos-args
;;   (testing "Controlar '(1 2)"
;;     (is
;;      (= (fnc-read '(1 2)) (list '*error* 'not-implemented)))))

;; (deftest fnc-read-one-arg
;;   (testing "Controlar '(1)"
;;     (is
;;      (= (fnc-read '(1)) (list '*error* 'not-implemented)))))

;; (deftest fnc-read-dos-args
;;   (testing "Controlar '(1 2)"
;;     (is
;;      (= (fnc-read '(1 2)) (list '*error* 'not-implemented)))))

;; (deftest fnc-read-one-arg
;;   (testing "Controlar '(1)"
;;     (is
;;      (= (fnc-read '(1)) (list '*error* 'not-implemented)))))

;; (deftest fnc-read-dos-args
;;   (testing "Controlar '(1 2)"
;;     (is
;;      (= (fnc-read '(1 2)) (list '*error* 'not-implemented)))))

;; (deftest fnc-read-one-arg
;;   (testing "Controlar '(1)"
;;     (is
;;      (= (fnc-read '(1)) (list '*error* 'not-implemented)))))


(deftest fnc-read-one-arg
  (testing "Controlar '(1)"
    (is
     (= (fnc-read '(1)) (list '*error* 'not-implemented)))))

(deftest fnc-read-dos-args
  (testing "Controlar '(1 2)"
    (is
     (= (fnc-read '(1 2)) (list '*error* 'not-implemented)))))

(deftest fnc-terpri-sin-args
  (testing "Controlar ()"
    (is
     (= (fnc-terpri ()) nil))))

(deftest fnc-terpri-one-arg
  (testing "Controlar '(1)"
    (is
     (= (fnc-terpri '(1)) (list '*error* 'not-implemented)))))

(deftest fnc-terpri-dos-args
  (testing "Controlar '(1 2)"
    (is
     (= (fnc-terpri '(1 2)) (list '*error* 'not-implemented)))))

(deftest fnc-add-lista-sin-args
  (testing "Controlar ()"
    (is
     (= (fnc-add ()) '(*error* too-few-args)))))

(deftest fnc-add-lista-un-args
  (testing "Controlar '(3)"
    (is
     (= (fnc-add '(3)) '(*error* too-few-args)))))

(deftest fnc-add-lista-dos-args-numericos
  (testing "Controlar '(3 4)"
    (is
     (= (fnc-add '(3 4)) 7))))

(deftest fnc-add-lista-tres-args-numericos
  (testing "Controlar '(3 4 5)"
    (is
     (= (fnc-add '(3 4 5)) 12))))

(deftest fnc-add-lista-cuatro-args-numericos
  (testing "Controlar '(3 4 5 6)"
    (is
     (= (fnc-add '(3 4 5 6)) 18))))

(deftest fnc-add-lista-numeros-y-letras
  (testing "Controlar '(A 4 5 6)"
    (is
     (= (fnc-add '(A 4 5 6)) '(*error* number-expected A)))))

(deftest fnc-add-lista-numeros-y-letras-2
  (testing "Controlar '(3 A 5 6)"
    (is
     (= (fnc-add '(3 A 5 6)) '(*error* number-expected A)))))

(deftest fnc-add-sin-args
  (testing "Controlar '(3 4 A 6)"
    (is
     (= (fnc-add '(3 4 A 6)) '(*error* number-expected A)))))

(deftest fnc-sub-lista-sin-args
  (testing "Controlar ()"
    (is
     (= (fnc-sub ()) '(*error* too-few-args)))))

(deftest fnc-sub-lista-un-args
  (testing "Controlar '(3)"
    (is
     (= (fnc-sub '(3)) -3))))

(deftest fnc-sub-lista-dos-args-numericos
  (testing "Controlar '(3 4)"
    (is
     (= (fnc-sub '(3 4)) -1))))

(deftest fnc-sub-lista-tres-args-numericos
  (testing "Controlar '(3 4 5)"
    (is
     (= (fnc-sub '(3 4 5)) -6))))

(deftest fnc-sub-lista-cuatro-args-numericos
  (testing "Controlar '(3 4 5 6)"
    (is
     (= (fnc-sub '(3 4 5 6)) -12))))

(deftest fnc-sub-lista-numeros-y-letras
  (testing "Controlar '(A 4 5 6)"
    (is
     (= (fnc-sub '(A 4 5 6)) '(*error* number-expected A)))))

(deftest fnc-sub-lista-numeros-y-letras-2
  (testing "Controlar '(3 A 5 6)"
    (is
     (= (fnc-sub '(3 A 5 6)) '(*error* number-expected A)))))

(deftest fnc-sub-sin-args
  (testing "Controlar '(3 4 A 6)"
    (is
     (= (fnc-sub '(3 4 A 6)) '(*error* number-expected A)))))

(deftest fnc-lt-lista-sin-args
  (testing "Controlar '()"
    (is
     (= (fnc-lt '()) '(*error* too-few-args)))))

(deftest fnc-lt-lista-un-args-numericos
  (testing "Controlar '(1)"
    (is
     (= (fnc-lt '(1)) '(*error* too-few-args)))))

(deftest fnc-lt-lista-dos-args-numericos-diferentes-primero-menor
  (testing "Controlar '(1 2)"
    (is
     (= (fnc-lt '(1 2)) 't))))

(deftest fnc-lt-lista-dos-args-numericos-iguales
  (testing "Controlar '(1 1)"
    (is
     (= (fnc-lt '(1 1)) nil))))

(deftest fnc-lt-lista-numeros-primero-mayor
  (testing "Controlar '(2 1))"
    (is
     (= (fnc-lt '(2 1)) nil))))

(deftest fnc-lt-lista-letra-y-numero
  (testing "Controlar '(A 1)"
    (is
     (= (fnc-lt '(A 1)) '(*error* number-expected A)))))

(deftest fnc-lt-lista-numero-y-letra
  (testing "Controlar '(1 A)"
    (is
     (= (fnc-lt '(1 A)) '(*error* number-expected A)))))

(deftest fnc-lt-tres-args
  (testing "Controlar '(1 2 3)"
    (is
     (= (fnc-lt '(1 2 3)) '(*error* too-many-args)))))

(deftest fnc-gt-lista-sin-args
  (testing "Controlar '()"
    (is
     (= (fnc-gt '()) '(*error* too-few-args)))))

(deftest fnc-gt-lista-un-args-numericos
  (testing "Controlar '(1)"
    (is
     (= (fnc-gt '(1)) '(*error* too-few-args)))))

(deftest fnc-gt-lista-dos-args-numericos-diferentes-primero-menor
  (testing "Controlar '(1 2)"
    (is
     (= (fnc-gt '(1 2)) nil))))

(deftest fnc-gt-lista-dos-args-numericos-iguales
  (testing "Controlar '(1 1)"
    (is
     (= (fnc-gt '(1 1)) nil))))

(deftest fnc-gt-lista-numeros-primero-mayor
  (testing "Controlar '(2 1))"
    (is
     (= (fnc-gt '(2 1)) 't))))

(deftest fnc-gt-lista-letra-y-numero
  (testing "Controlar '(A 1)"
    (is
     (= (fnc-gt '(A 1)) '(*error* number-expected A)))))

(deftest fnc-gt-lista-numero-y-letra
  (testing "Controlar '(1 A)"
    (is
     (= (fnc-gt '(1 A)) '(*error* number-expected A)))))

(deftest fnc-gt-tres-args
  (testing "Controlar '(1 2 3)"
    (is
     (= (fnc-gt '(1 2 3)) '(*error* too-many-args)))))

(deftest fnc-ge-lista-sin-args
  (testing "Controlar '()"
    (is
     (= (fnc-gt '()) '(*error* too-few-args)))))

(deftest fnc-ge-lista-un-args-numericos
  (testing "Controlar '(1)"
    (is
     (= (fnc-ge '(1)) '(*error* too-few-args)))))

(deftest fnc-ge-lista-dos-args-numericos-diferentes-primero-menor
  (testing "Controlar '(1 2)"
    (is
     (= (fnc-ge '(1 2)) nil))))

(deftest fnc-ge-lista-dos-args-numericos-iguales
  (testing "Controlar '(1 1)"
    (is
     (= (fnc-ge '(1 1)) 't))))

(deftest fnc-ge-lista-numeros-primero-mayor
  (testing "Controlar '(2 1))"
    (is
     (= (fnc-ge '(2 1)) 't))))

(deftest fnc-ge-lista-letra-y-numero
  (testing "Controlar '(A 1)"
    (is
     (= (fnc-ge '(A 1)) '(*error* number-expected A)))))

(deftest fnc-ge-lista-numero-y-letra
  (testing "Controlar '(1 A)"
    (is
     (= (fnc-ge '(1 A)) '(*error* number-expected A)))))

(deftest fnc-ge-tres-args
  (testing "Controlar '(1 2 3)"
    (is
     (= (fnc-ge '(1 2 3)) '(*error* too-many-args)))))

(deftest fnc-reverse-lista-vacia
  (testing "Controlar ()"
    (is
     (= (fnc-reverse ()) '(*error* too-few-args)))))

(deftest fnc-reverse-lista-un-arg-numerico
  (testing "Controlar '(1)"
    (is
     (= (fnc-reverse '(1)) '(*error* list expected 1)))))

(deftest fnc-reverse-lista-un-arg-cadena
  (testing "Controlar '(A)"
    (is
     (= (fnc-reverse '(A)) '(*error* list expected A)))))

(deftest fnc-reverse-lista-lista-un-numero
  (testing "Controlar '((1))"
    (is
     (= (fnc-reverse '((1))) '(1)))))

(deftest fnc-reverse-lista-lista-tres-numeros
  (testing "Controlar '((1 2 3))"
    (is
     (= (fnc-reverse '((1 2 3))) '(3 2 1)))))

(deftest fnc-reverse-lista-de-listas
  (testing "Controlar '((1 2 3)(4))"
    (is
     (= (fnc-reverse '((1 2 3) (4))) '(*error* too-many-args)))))

(deftest evaluar-escalar-numero-no-esta
  (testing "Controlar 32 '(v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 32 '(v 1 w 3 x 6))))))

(deftest evaluar-escalar-string-no-esta
  (testing "Controlar 'chau' '(v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list "chau" '(v 1 w 3 x 6))))))

(deftest evaluar-escalar-simbolo-lower-si-esta
  (testing "Controlar 'z '(v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list "hola" '(v 1 w 3 x 6))))))

(deftest evaluar-escalar-simbolo-upper-si-esta-en-lower
  (testing "Controlar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list "hola" '(v 1 w 3 x 6))))))

(deftest evaluar-escalar-simbolo-esta-pero-queda-el-mismo-valor
  (testing "Controlar 'w '(v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 3 '(v 1 w 3 x 6))))))

(deftest evaluar-escalar-simbolo-esta
  (testing "Controlar 'x '(v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-escalar 'x '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 5 '(v 1 w 3 x 6))))))

(deftest evaluar-escalar-simbolo-no-esta
  (testing "Controlar 'n '(v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list '(*error* unbound-symbol n) '(v 1 w 3 x 6))))))

(deftest evaluar-de-un-parametros
  (testing "Controlar '(de f (x)) '(x 1)"
    (is
     (= (evaluar-de '(de f (x)) '(x 1)) (list 'f (list 'x 1 'f '(lambda (x))))))))

(deftest evaluar-de-dos-parametros
  (testing "Controlar '(de f (x) 2) '(x 1)"
    (is
     (= (evaluar-de '(de f (x) 2) '(x 1)) (list 'f (list 'x 1 'f '(lambda (x) 2)))))))

(deftest evaluar-de-suma
  (testing "Controlar '(de f (x) (+ x 1)) '(x 1)"
    (is
     (= (evaluar-de '(de f (x) (+ x 1)) '(x 1)) (list 'f (list 'x 1 'f '(lambda (x) (+ x 1))))))))

(deftest evaluar-de-simple-suma
  (testing "Controlar '(de f (x y) (+ x y)) '(x 1)"
    (is
     (= (evaluar-de '(de f (x y) (+ x y)) '(x 1)) (list 'f (list 'x 1 'f (list 'lambda '(x y) '(+ x y))))))))

(deftest evaluar-de-varios-parametros
  (testing "Controlar '(de f (x y) (prin3 x) (terpri) y) '(x 1)"
    (is
     (= (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1)) (list 'f (list 'x 1 'f (list 'lambda '(x y) '(prin3 x) '(terpri) 'y)))))))

(deftest evaluar-de-sin-funcion-ni-lista-de-parametros
  (testing "Controlar '(de) '(x 1)"
    (is
     (= (evaluar-de '(de) '(x 1)) (list '(*error* list expected nil) '(x 1))))))

(deftest evaluar-de-sin-lista-de-parametros
  (testing "Controlar '(de f) '(x 1)"
    (is
     (= (evaluar-de '(de f) '(x 1)) (list '(*error* list expected nil) '(x 1))))))

(deftest evaluar-de-numero-como-parametro-en-vez-de-lista
  (testing "Controlar '(de f 2) '(x 1)"
    (is
     (= (evaluar-de '(de f 2) '(x 1)) (list '(*error* list expected 2) '(x 1))))))

(deftest evaluar-de-dos-numeros-como-parametro-en-vez-de-lista
  (testing "Controlar '(de f 2 3) '(x 1)"
    (is
     (= (evaluar-de '(de f 2 3) '(x 1)) (list '(*error* list expected 2) '(x 1))))))

(deftest evaluar-de-sin-lista-de-parametros
  (testing "Controlar '(de (f)) '(x 1)"
    (is
     (= (evaluar-de '(de (f)) '(x 1)) (list '(*error* list expected nil) '(x 1))))))

(deftest evaluar-de-parametro-que-no-es-lista
  (testing "Controlar '(de 2 x) '(x 1)"
    (is
     (= (evaluar-de '(de 2 x) '(x 1)) (list '(*error* list expected x) '(x 1))))))

(deftest evaluar-de-funcion-numero
  (testing "Controlar '(de 2 (x)) '(x 1)"
    (is
     (= (evaluar-de '(de 2 (x)) '(x 1)) (list '(*error* symbol expected 2) '(x 1))))))

(deftest evaluar-de-funcion-nil
  (testing "Controlar '(de nil (x) 2) '(x 1)"
    (is
     (= (evaluar-de '(de nil '(x) 2) '(x 1)) (list '(*error* cannot-set nil) '(x 1))))))

(deftest evaluar-if-caso-true
  (testing "Controlar '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list nil (list nil nil 't 't 'v 1 'w 3 'x 6))))))

;; (deftest evaluar-if-numero-distinto-de-0
;;   (testing "Controlar '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
;;     (is
;;      (= (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list nil (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-nil
  (testing "Controlar '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list nil (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-simbolo
  (testing "Controlar '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list nil (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-simbolo-y-numero-distinto-de-cero
  (testing "Controlar '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola'))"
    (is
     (= (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 9 (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-z-y-numero-9
  (testing "Controlar '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 9 (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest  evaluar-if-w-y-numero-9
  (testing "Controlar '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 9 (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-r-y-numero-9
  (testing "Controlar '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list (list '*error* 'unbound-symbol 'r) (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-nil-y-numero-9
  (testing "Controlar '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list nil (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-nil-y-numero-9-y-z
  (testing "Controlar '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list "hola" (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-nil-numeros-9-1-2-3-z
  (testing "Controlar '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list "hola" (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-nil-numero-9-w
  (testing "Controlar '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 3 (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-nil-numeros-9-8
  (testing "Controlar '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 8 (list nil nil 't 't 'v 1 'w 3 'x 6))))))

(deftest evaluar-if-nil-a-numero-8
  (testing "Controlar '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
    (is
     (= (evaluar-if '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 8 (list nil nil 't 't 'v 1 'w 3 'x 6))))))

;; (deftest evaluar-if-gt-2-0-a-8-error
;;   (testing "Controlar '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
;;     (is
;;      (= (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list (list '*error* 'unbound-symbol 'a) (list 'gt 'gt nil nil 't 't 'v 1 'w 3 'x 6))))))

;; (deftest evaluar-if-gt-2-0-a-8-success
;;   (testing "Controlar '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
;;     (is
;;      (= (evaluar-if '(if '(gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 '(gt gt nil nil t t v 1 w 3 x 6))))))

;; (deftest evaluar-if-nil-a-numero-8
;;   (testing "Controlar '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z 'hola')"
;;     (is
;;      (= (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 '(gt gt nil nil t t v 1 w 3 x 6 m 8))))))

(deftest evaluar-or-sin-params
  (testing "Controlar '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-or '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list nil (list nil nil 't 't 'w 5 'x 4))))))

(deftest  evaluar-or-un-param
  (testing "Controlar '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-or '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list nil (list nil nil 't 't 'w 5 'x 4))))))

(deftest evaluar-or-t-param
  (testing "Controlar '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-or '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list 't (list nil nil 't 't 'w 5 'x 4))))))

(deftest evaluar-or-w-param
  (testing "Controlar '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-or '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list 5 (list nil nil 't 't 'w 5 'x 4))))))

(deftest evaluar-or-r-param
  (testing "Controlar '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-or '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list (list '*error* 'unbound-symbol 'r) (list nil nil 't 't 'w 5 'x 4))))))

(deftest evaluar-or-y-param
  (testing "Controlar '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-or '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list nil (list nil nil 't 't 'w 5 'x 4))))))

(deftest evaluar-or-numero-6-param
  (testing "Controlar '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-or '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list 6 (list nil nil 't 't 'w 5 'x 4))))))

(deftest evaluar-or-nil-numero-6-params
  (testing "Controlar '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-or '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list 6 (list nil nil 't 't 'w 5 'x 4))))))

;; (deftest evaluar-or-setq-b-numero-8-params
;;   (testing "Controlar '(or (setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;      (= (evaluar-or '(or '(setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list 8 (list nil nil 't 't 'w 5 'x 4 'b 8))))))

;; (deftest evaluar-or-nil-numero-6-nil-params
;;   (testing "Controlar '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;      (= (evaluar-or '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list 6 (list nil nil 't 't 'w 5 'x 4))))))

;; (deftest evaluar-or-nil-numero-6-r-nil-params
;;   (testing "Controlar '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;      (= (evaluar-or '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list 6 (list nil nil 't 't 'w 5 'x 4))))))

;; (deftest evaluar-or-nil-nil-nil-nil
;;   (testing "Controlar '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;      (= (evaluar-or '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) (list nil (list nil nil 't 't 'w 5 'x 4))))))

;; (deftest evaluar-setq-sin-params
;;   (testing "Controlar '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;      (= (evaluar-setq '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) (list (list '*error* 'list 'expected nil) (list nil nil 't 't '+ 'add 'w 5 'x 4))))))

;; (deftest evaluar-setq-m-param
;;   (testing "Controlar '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;      (= (evaluar-setq '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) (list (list '*error* 'list 'expected nil) (list nil nil 't 't '+ 'add 'w 5 'x 4))))))

;; (deftest evaluar-setq-m-numero-7-params
;;   (testing "Controlar '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;      (= (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) (list 7 (list nil nil 't 't '+ 'add 'w 5 'x 4 'm 7))))))

;; (deftest evaluar-setq-x-numero-7-params
;;   (testing "Controlar '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;      (= (evaluar-setq '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) (list 7 (list nil nil 't 't '+ 'add 'w 5 'x 7))))))

(deftest evaluar-setq-x-x-+-1-params
  (testing "Controlar '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
    (is
     (= (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) (list 2 (list nil nil 't 't '+ 'add 'w 5 'x 2))))))

;; (deftest evaluar-setq-x-x-+-1-2-params
;;   (testing "Controlar '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(y nil z 3)"
;;     (is
;;       (= (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4)) '(5 '(nil nil t t + add w 5 x 5))))))

;; (deftest evaluar-setq-nil
;;   (testing "Controlar '(setq nil) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;       (= (evaluar-setq '(setq nil) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '('(*error* list expected nil) '(nil nil t t + add w 5 x 4))))))

;; (deftest evaluar-setq-nil-numero-7-params
;;   (testing "Controlar '(setq nil 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;       (= (evaluar-setq '(setq nil 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '('(*error* cannot-set nil) '(nil nil t t + add w 5 x 4))))))

;; (deftest evaluar-setq-numeros-7-8
;;   (testing "Controlar '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;       (= (evaluar-setq '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '('(*error* symbol expected 7) '(nil nil t t + add w 5 x 4))))))

;; (deftest evaluar-setq-lista
;;   (testing "Controlar '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)"
;;     (is
;;       (= (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(8 '(nil nil t t + add w 5 x 7 m 8))))))

;; (deftest evaluar-setq-4-params
;;   (testing "Controlar '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3)"
;;     (is
;;       (= (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(14 '(nil nil t t + add w 5 x 7 m 14))))))

;; (deftest evaluar-setq-3-params
;;   (testing "Controlar '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3)"
;;     (is
;;       (= (evaluar-setq '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '('(*error* list expected nil) '(nil nil t t + add w 5 x 7))))))

;; (deftest evaluar-setq-6-params
;;   (testing "Controlar '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3)"
;;     (is
;;       (= (evaluar-setq '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(9 '(nil nil t t + add w 5 x 7 y 8 z 9))))))