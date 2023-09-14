#lang eopl
; Juan José Moreno Jaramillo 2310038-3743
; Eyder Santiago Suárez
; Lenin Esteban Carabali Moreno


;; Funciones adicionales (auxiliares que usamos para el desarrollo del taller)

; Propósito: concatenar dos listas en una sola
(define (my-append lista1 lista2)
  (if (null? lista1)
      lista2
      (cons (car lista1) (my-append (cdr lista1) lista2))))

; Propósito: retornar la longitud de una lista
(define (my-length L1)
  (if
   (null? L1)
     0
    (+ 1 (my-length (cdr L1)))))

; PUNTO 1
; invert:
; uso ( invert L) -> El propósito de la función invert es tomar una lista
; de pares representados como listas de dos elementos 
; (x, y) y devolver una nueva lista con los pares invertidos,
; es decir, en el orden (y, x).
;
; invert : L -> L'
; 
;  <list> ::= '() 
;         ::= ((<scheme-value> <scheme-value>) <list>)


(define invert
  (lambda (L)
    (if (null? L) '()
        (let*
            (
             (head (car L))
             (x (car head))
             (y (cadr head))
             )
          (cons (list y x) (invert (cdr L)) )))))

; pruebas
(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))

;======================================================

; PUNTO: 2
; Propósito: retornar una lista con cada elemento de la lista encerrado en paréntesis
; ejemplo: si se tiene (2 3 4) debería retornar ((2) (3) (4))

; <OperacionB>::= <int>
;            ::= (<OperacionB> 'suma <OperacionB>)
;            ::= (<OperacionB> 'resta <OperacionB>)
;            ::= (<OperacionB> 'multiplica <OperacionB>)


(define (down L)
  (if (null? L)
      '() ; Si la lista está vacía, retornamos una lista vacía.
      (cons (list (car L)) (down (cdr L))))) ; Se crea una lista con el primer elemento, se repite con el resto

; pruebas
(down '(1 2 3))
(down '((una) (buena) (idea)))
(down '(un (objeto (mas)) complicado))

;======================================================

; PUNTO: 4
; filter-in
; uso ( filter-in P L) ->  El propósito de la
; función filter-in es crear una nueva lista
; que contiene elementos de la lista original L que 
; satisfacen un predicado P
;
; filter-in : P x L -> L'
; 
;  <list> ::= '()
;         ::= <scheme-value> <list>


(define filter-in
  (lambda (P L)
    (if (null? L) '()
        (if (not (P (car L))) (filter-in P (cdr L))
           (cons (car L) (filter-in P (cdr L)))))))

; Pruebas
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))

;======================================================

; PUNTO: 5
; Propósito: Buscar y retornar la posición en la que sse encuentre el primer elemento que cumpla con la condición ejemplo: primero número (a 2)
; retornaría posición 1.

(define (list-index P L)
  (define (list-index-helper index L)
    (cond
      ((null? L) #f)                      
      ((P (car L)) index)                   
      (else (list-index-helper (+ index 1) (cdr L)))))

  (list-index-helper 0 L))

; Pruebas
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))

;======================================================

; PUNTO 7
; cartesian-product
; uso ( cartesian-product L1 L1) -> El propósito de 
; la función cartesian-product es calcular y 
; proporcionar una lista de todas las posibles combinaciones de 
; elementos entre dos conjuntos representados por las listas L1 y 
; L2, devolviendo estas combinaciones como tuplas en una lista.
;
; cartesian-product : L1 x L2 -> L3 tal que { (x, y) | x ∈ L1, y ∈ L2 }
; 
;  <list> ::= '() 
;         ::= ((<int | symbol> <int | symbol>) <list>)

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1) '()
        (my-append (cartesian-product-aux2 (car L1) L2) (cartesian-product (cdr L1) L2)))))

(define cartesian-product-aux2
  (lambda (L1 L2)
    (if (null? L2) '()
        (cons (list L1 (car L2)) (cartesian-product-aux2 L1 (cdr L2))))))
;Pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))

;======================================================

; PUNTO: 8
; Propósito: Se busca que se retorne F(a) = b, sienda a la L1 y b la L2 por ejemplo:
; Se tiene que la operación es una multiplicación por 2 y los elementos de la lista
; son L1 (1 2 3) y L2 (2 3 4). Por lo tanto, los únicos elementos que cumplen con
; la indicación es (1 2) por 1 por la función (* 2) es igual al 2 de la L2.

(define mapping
  (lambda (F L1 L2)
    (cond
      ((null? L1) empty)
      ((null? L2) empty)
      ((= (F (car L1)) (car L2))
         (cons (list (car L1) (car L2))
               (mapping F (cdr L1) (cdr L2))))
    (else
     (mapping F (cdr L1) (cdr L2))))))           

;Pruebas
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

;========================================================

; PUNTO: 10
; up
; uso (up L) -> El propósito de la función up es eliminar un par de paréntesis 
; de cada elemento del nivel superior de una lista
;
; up : L -> L'
; 
; <list>  ::= '() 
;         ::= (<scheme-value> <list>)

(define up
  (lambda (L)
    (cond
      [(null? L) '()]
      [(list? (car L)) (my-append (car L) (up (cdr L)))]
      [else (cons (car L) (up (cdr L)))])))

; Pruebas
(up '((1 2) (3 4)))
(up '((x (y)) z))

;========================================================

; PUNTO: 11
; Propósito: se busca operar los elementos de las lista con la función que se defina
; Por ejemplo: si la función es suma, sería  sumar el primer elemento de la Lista 1
; Con el primer elemento de la lista 2, y así sucesivamente con todos los elementos.

(define (zip F L1 L2)
  (if (= (my-length L1) (my-length L2))
      (if (null? L1)
          '()
          (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2))))
      '()))

; Pruebas
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))

;======================================================

; PUNTO: 13 
; Elabore una función llamada (operate lrators lrands) donde
; lrators es una lista de funciones binarias de tamaño n y lrands es una lista
; de números de tama~no n + 1. La función retorna el resultado de aplicar
; sucesivamente las operaciones en lrators a los valores en lrands.

(define operate
  (lambda (lrators lrands)
    (operate-aux (rev lrators) (rev lrands))))

(define operate-aux
  (lambda (lrators-rev lrands-rev)
    (if (null? lrators-rev)
        (car lrands-rev)
        (let
            (
             (operator (car lrators-rev))
             (number (car lrands-rev))
             )
          (operator (operate-aux (cdr lrators-rev) (cdr lrands-rev)) number)))))

(define (rev lst)
 (if (null? lst)
     '() (my-append (rev (cdr lst)) (list (car lst)))))

;======================================================

; PUNTO: 14
; Propósito: Se busca obtener la ruta hasta encontrar el número dentro de un arbol binario
; definido, por ejemplo quiero buscar un el 7 en mi arbol, y este se encuentra dos nodos
; a la derecha, al ingresar el número que busco y el arbol donde debo buscarlo debería
; retornarme derecha derecha


(define (path n BST)
  (define (path-helper n BST current-path)
    (cond
      ((null? BST) empty) ; Si el árbol está vacío, retornamos una lista vacía.
      ((= n (car BST)) current-path) ; Si encontramos n, retornamos la ruta actual.
      ((< n (car BST)) (path-helper n (cadr BST) (my-append current-path '(left)))) ; Si n es menor, vamos a la izquierda.
      (else (path-helper n (caddr BST) (my-append current-path '(right)))))) ; Si n es mayor, vamos a la derecha.

  (path-helper n BST '())) ; Iniciamos la búsqueda desde la raíz con una ruta vacía.

; Pruebas
(path 17 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))

;======================================================

; Punto: 16
; Dada la siguiente gramática sobre operaciones binarias:
; <OperacionB>::= <int>
;            ::= (<OperacionB> 'suma <OperacionB>)
;            ::= (<OperacionB> 'resta <OperacionB>)
;            ::= (<OperacionB> 'multiplica <OperacionB>)

; Implementa una función llamada (Operar-binarias operacionB) que recibe
; como parámetro una operación binaria válida y retorna el resultado de hacer
; las operaciones suma, resta y multiplicaciónn correspondientes.


(define (Operar-binarias operacion)
  (cond
    [(number? operacion) operacion] ; Si es un número, simplemente lo retornamos
    [(list? operacion)
     (let ((izquierda (car operacion))
           (operador (cadr operacion))
           (derecha (caddr operacion)))
       (cond
         [(eq? operador 'suma) (+ (Operar-binarias izquierda) (Operar-binarias derecha))]
         [(eq? operador 'resta) (- (Operar-binarias izquierda) (Operar-binarias derecha))]
         [(eq? operador 'multiplica) (* (Operar-binarias izquierda) (Operar-binarias derecha))]
         [else ("Operador desconocido")]))]
    [else ("Operación no válida")]))

;======================================================

; PUNTO: 17
; Propósito prod-scalar-matriz-aux: su propósito es una multiplicación escalar
; entre una fila de una matriz y un vector (una lista de números).
; Se busca calcular el producto escalar entre la fila y el vector de manera recursiva.

(define prod-scalar-matriz-aux
  (lambda (fila vec)
    (if (or (null? fila) (null? vec))
        '()
        (cons (* (car fila) (car vec))
              (prod-scalar-matriz-aux (cdr fila) (cdr vec))))))

; Propósito prod-scalar-matriz: calcular el producto escalar entre una matriz
; una lista de listas) y un vector (una lista de números).
; Esta función utiliza la función auxiliar prod-scalar-matriz-aux para realizar el cálculo
; en cada fila de la matriz y luego combina los resultados en una nueva matriz.

(define prod-scalar-matriz
  (lambda (mat vec)
    (if (or (null? mat) (null? vec))
        '() ; Si la matriz o el vector están vacíos, el resultado es una lista vacía.
        (cons (prod-scalar-matriz-aux (car mat) vec)
              (prod-scalar-matriz (cdr mat) vec)))))

;pruebas
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))