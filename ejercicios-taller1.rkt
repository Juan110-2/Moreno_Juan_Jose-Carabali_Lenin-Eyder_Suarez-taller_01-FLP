#lang eopl
; Juan José Moreno Jaramillo - 2310038-3743
; Eyder Santiago Suárez Chávez - 2322714-3743
; Lenin Esteban Carabali Moreno - 2310025-3743

;======================================================

; Uso (my-append L1 L2) -> Propósito: concatenar dos listas en una sola
;
; L1 L2 -> L3
;
;  <list> ::= '() 
;         ::= ((<scheme-value>) <list1>) (<scheme-value>) <list2>))

(define (my-append lista1 lista2)
  (cond
    ((null? lista1) lista2)
    (else (cons (car lista1) (my-append (cdr lista1) lista2)))))

; pruebas
(my-append '((a 1) (a 2)) '((1 b) (2 b))) ; salida -> ((a 1) (a 2) (1 b) (2 b))
(my-append '("hola") '("Mundo")); salida -> ("hola" "Mundo")

;======================================================

; Uso (my-length L1) -> Propósito: retornar la longitud de una lista
;
; my-length : (list 'a) -> integer
;
;  <list> ::= '() 
;         ::= ((<scheme-value>) <list>)

(define (my-length L1)
  (cond
    ((null? L1) 0)
    (else (+ 1 (my-length (cdr L1))))))

; pruebas
(my-length '((a 1) (a 2) (1 b) (2 b))) ; salida -> 4
(my-length '("hola" "Mundo")) ; salida -> 2

;======================================================

; Funciones auxiliares usadas


;======================================================

; PUNTO 1
; invert:
; uso ( invert L) -> El propósito de la función invert es tomar una lista
; de pares representados como listas de dos elementos 
; (x, y) y devolver una nueva lista con los pares invertidos,
; es decir, en el orden (y, x).
;
; invert : L -> L'
; 
;  <list> ::= <scheme-value> <scheme-value>
;  <scheme-value> ::= {int | symbol}*

(define invert
  (lambda (L)
    (cond
      [(null? L) '()]
      [else
        (let*
          [(head (car L))
           (x (car head))
           (y (cadr head))]
          (cons (list y x) (invert (cdr L))))])))

; pruebas
(invert '((a 3) (b 8) (1 e) (f k)))  ; salida -> ((3 a) (8 b) (e 1) (k f))
(invert '((3 9) (1 91) ("h" r) (x e) ("h" "w"))) ; salida -> ((9 3) (91 1) (r "h") (e x) ("w" "h"))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o))); salida -> (("racket" "es") ("muy" "genial") (29 17) (o 81))

;======================================================

; PUNTO: 2
; Uso (down L) -> El propósito es retornar una lista con cada elemento de la lista encerrado en paréntesis
; ejemplo: si se tiene (2 3 4) debería retornar ((2) (3) (4))
;
; L -> (list (L))
;
; <list> ::= '()
;        ::= (<scheme-value> <list>)

(define (down L)
  (cond
    ((null? L) '())
    (else (cons (list (car L)) (down (cdr L))))))

; pruebas
(down '(1 2 3))  ; salida -> ((1) (2) (3))
(down '((una) (buena) (idea))) ; salida -> (((una)) ((buena)) ((idea)))
(down '(un (objeto (mas)) complicado)) ; salida -> ((un) ((objeto (mas))) (complicado))
(down '(una (estructura (de (datos (anidada)))))) ; salida -> ((una) ((estructura (de (datos (anidada))))))

;======================================================

;PUNTO: 3

; list-set:
; uso (list-set L n x) -> El propósito de la función list-set es recibir una lista L, un número n
; y un elemento x, (L n x). La entrada n define la posición (empezando en cero) en la que estará
; anidada el elemento x dentro de la lista L que el programa retorne.

; list-set : L n x -> (Ln(x)L)

; (<List> <Integer> <List>) :: '()
;                           :: (<List(<scheme-value>)> )



; list-set: list number any -> list
; Retorna una lista igual a L, pero con el elemento x en la posición n
(define (list-set L n x)
  (cond
    [(null? L) '()] ; Si la lista está vacía, se retorna la lista vacía
    [(= n 0) (cons x (cdr L))] ; Si n es cero, se reemplaza el primer elemento de L por x
    [else (cons (car L) (list-set (cdr L) (- n 1) x))] ; Si no, se conserva el primer elemento de L y se aplica la función recursivamente al resto de L, decrementando n en uno
  )
)

; Pruebas
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(a b))

;======================================================

; PUNTO: 4
; filter-in
; uso ( filter-in P L) ->  El propósito de la
; función filter-in es crear una nueva lista
; que contiene elementos de la lista original L que 
; satisfacen un predicado P
;
; filter-in : P L -> L'
; 
;  <list> ::= '()
;         ::= (<scheme-value> <list>)

(define filter-in
  (lambda (P L)
    (cond
      [(null? L) '()]
      [(not (P (car L))) (filter-in P (cdr L))]
      [else (cons (car L) (filter-in P (cdr L)))])))


; Pruebas
(filter-in number? '(a 2 (1 3) b 7 y ((a))))  ; salida -> (2 7)
(filter-in symbol? '(a 2 (1 3) b 7 y ((a))))  ; salida -> (a b y)
(filter-in string? '(a 2 (1 3) b 7 y ((a))))  ; salida -> ()

;======================================================

; PUNTO: 5
; Uso (list-index P L) -> El propósito es buscar y retornar la posición en la que se encuentre
; el primer elemento que cumpla con la condición ejemplo: primero número (a 2) retornaría posición 1.
;
; list-index : P L -> Integer | Boolean
;
; <list>::= '()
;       ::= (<scheme-value> (<scheme-value> <list>))

(define (list-index P L)
  (define (list-index-helper index L)
    (cond
      ((null? L) #f)                      
      ((P (car L)) index)                   
      (else (list-index-helper (+ index 1) (cdr L)))))

  (list-index-helper 0 L))

; Pruebas
(list-index number? '(a 2 (1 3) b 7)) ; salida -> 1
(list-index symbol? '(a (b c) 17 foo)) ; salida -> 0
(list-index symbol? '(1 2 (a b) 3)) ; salida -> #f
(list-index list? '(1 2 (a b) (x y z) 3)) ; salida -> 2

;======================================================

;PUNTO: 6
; swapper:
; uso (swapper E1 E2 L) -> El propósito de la función swapper es tomar dos expresiones (E1 E2), y una lista (L),
; la lista admite solo expresiones de E1 y E2. Devuelve dentro de la lista L, las expresiones de tipo E1
; intercambiadas con las expresiones de tipo E2, es decir (E1 E2 '(E1 E2 E2 E1)) -> '(E2 E1 E1 E2).

; swapper : E1 E2 L -> 'L

; (<Expresion> <Expresion> <List>) ::
;                                  :: (<List(<Expresion> <Expresion>)> 


(define (swapper E1 E2 L)
  (cond [(null? L) '()] ; Si la lista está vacía, devolver una lista vacía
        [(equal? E1 (car L)) (cons E2 (swapper E1 E2 (cdr L)))] ; Si el primer elemento de la lista 'L' es igual a e1, reemplazarlo por e2 y seguir con el resto de la lista
        [(equal? E2 (car L)) (cons E1 (swapper E1 E2 (cdr L)))] ; Si el primer elemento de la lista 'L' es igual a e2, reemplazarlo por e1 y seguir con el resto de la lista
        [else (cons (car L) (swapper E1 E2 (cdr L)))] ; Si el primer elemento no es ni e1 ni e2, mantenerlo y seguir con el resto de la lista
        ))


; pruebas
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))
;======================================================

; PUNTO: 7

; El proposito de es tomar el valor de la posción 0 en la lista 1 y crear una lista con todos los valores de la lista 2

; cartesian-product-aux : L1 x L2 -> L3 tal que { (L1, y) | L1, y ∈ L2 }
; 
;  <list> ::= '() | <scheme-value>
;         ::= {(<L1> <list>)}*

; <L1> ::= car de lista L1 (symbol)
; <scheme-value> ::= {int | symbol}*

(define cartesian-product-aux2
  (lambda (L1 L2)
    (if (null? L2) '()
        (cons (list L1 (car L2)) (cartesian-product-aux2 L1 (cdr L2))))))

; Pruebas
(cartesian-product-aux2 'a '(x y)) ; salida -> ((a x) (a y))
(cartesian-product-aux2 'r '(5 6 7)) ; salida -> ((r 5) (r 6) (r 7))
(cartesian-product-aux2 'j '(2 0)) ; salida -> ((j 2) (j 0))

; cartesian-product
; uso ( cartesian-product L1 L1) -> El propósito de 
; la función cartesian-product es calcular y 
; proporcionar una lista de todas las posibles combinaciones de 
; elementos entre dos conjuntos representados por las listas L1 y 
; L2, devolviendo estas combinaciones como tuplas en una lista.
;
; cartesian-product : L1 x L2 -> L3 tal que { (x, y) | x ∈ L1, y ∈ L2 }
; 
;  <list> ::= '() | <scheme-value>
;         ::= (<list> <list>)
;
; <scheme-value> ::= {int | symbol}*

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1) '()
        (my-append (cartesian-product-aux2 (car L1) L2) (cartesian-product (cdr L1) L2)))))

; Pruebas
(cartesian-product '(a b c) '(x y)) ; salida -> ((a x) (a y) (b x) (b y) (c x) (c y))
(cartesian-product '(p q r) '(5 6 7)) ; salida -> ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))
(cartesian-product '(a "as" c) '(2 0)) ; salida -> ((a 2) (a 0) ("as" 2) ("as" 0) (c 2) (c 0))


;======================================================

; PUNTO: 8
; Uso (mapping (lambda (d) (Function d number)) L1 L2) -> el proposito es buscar que se retorne
; F(a) = b, sienda a la L1 y b la L2 por ejemplo:
; Se tiene que la operación es una multiplicación por 2 y los elementos de la lista
; son L1 (1 2 3) y L2 (2 3 4). Por lo tanto, los únicos elementos que cumplen con
; la indicación es (1 2) por 1 por la función (* 2) es igual al 2 de la L2.
; mapping : ('a -> 'b) (list 'a) (list 'b) -> (list (list 'a 'b))

; mapping : F L1 L2 -> L3
;
; <espression> ::= (lambda <function>) <list1> <list2>
; <list1>      ::= {int}*
; <list2>      ::= {int}*
;

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
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6)) ; salida -> ((1 2) (2 4) (3 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6)) ; salida -> ((2 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12)) ; salida -> ()
(mapping (lambda (d) (+ d 10)) (list 1 2 3 4) (list 5 6 7 8)) ; salida -> ()

;========================================================

; PUNTO: 9

; inversions:

; uso (inversions L) -> El propósito de la función inversions es tomar una lista y determinar su número de
; inversiones, se contará como inversión si cumple el requisito que el valor ingresado dentro de la lista sea
; menor al número de la posición de la lista siguiente "i < j" en el extremo izquierdo, e "ai > aj" sea menor 
; que el número siguiente en el extremo derecho.

; inversions : L


; inversions <list L> -> <integer result>
; <list> :: 0
;        :: (<integer>)

; <count-inversions> <integer x> <list lst> :: 0
;                                           :: (<integer>)




(define (inversions L)
  (define (count-inversions x lst)
    (cond [(null? lst) 0] ; si la lista está vacía, no hay inversiones
           [(> x (car lst)) (+ 1 (count-inversions x (cdr lst)))] ; si el elemento es mayor que el primero de la lista, hay una inversión y se sigue con el resto de la lista
          [else (count-inversions x (cdr lst))] ; si el elemento es menor o igual que el primero de la lista, no hay inversión y se sigue con el resto de la lista
          )) (cond [(null? L) 0] ; si la lista está vacía, no hay inversiones
                    [else (+ (count-inversions (car L) (cdr L)) ; se cuentan las inversiones del primer elemento con el resto de la lista
                             (inversions (cdr L)))] ; se suman las inversiones del resto de la lista
) )



; Pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 2 1))



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

(up '((1 a) (z y 4)))  ; salida -> (1 a z y 4)
(up '((x (y)) z ("hola"))) ; salida -> (x (y) z "hola")
(up '(a (2 4) 9 ((s)))) ; salida -> (a 2 4 9 (s))

;========================================================

; PUNTO: 11
; Uso (zip function L1 L2) -> En el propósito se busca operar los elementos de las lista
; con la función que se defina
; Por ejemplo: si la función es suma, sería  sumar el primer elemento de la Lista 1
; Con el primer elemento de la lista 2, y así sucesivamente con todos los elementos.
;
; <espression> ::= <function> <list1> <list2>
; <list1>      ::= {int}*
; <list2>      ::= {int}*

(define (zip F L1 L2)
  (cond
    ((= (my-length L1) (my-length L2))
     (cond
       ((null? L1) '())
       (else (cons (F (car L1) (car L2))
                   (zip F (cdr L1) (cdr L2))))))
    (else '())))

; Pruebas
(zip + '(1 4) '(6 2))  ; salida -> (7 6)
(zip * '(11 5 6) '(10 9 8)) ; salida -> (110 45 48)
(zip / '(10 25 18) '(5 5 6)) ; salida -> (2 5 3)

;======================================================

; PUNTO 12
; filter-acum: 
; Uso (filter-acum a b F acum filter) -> El propósito de la función filter-acum es aplicar a la
; función binaria F a todos los elementos que est ́an en el intervalo [a, b], y que cumplan todo el 
; predicado de la función filter, si se cumple el resultado se irá guardando en la variable acum
; y que devuelva al final el valor de acum

; filter-acum <integer a> <integer b> <binary function F> <integer acum> <predicate function filter> -> <integer result>

; <accumulate-from-a-to-b> ::= <integer acum>
;                          ::= <integer element>

; <iter> ::= <integer acum>
;                          ::= <integer element>




(define (filter-acum a b F acum filter)
  (define (accumulate-from-a-to-b current-accumulator current-element)
    (cond ((and (>= current-element a)
                 (<= current-element b)
                 (filter current-element))
           (F current-accumulator current-element))
          (else current-accumulator)))
  
  (define (iter current-element current-accumulator)
    (cond ((> current-element b)
           current-accumulator)
          (else
           (iter (+ current-element 1) (accumulate-from-a-to-b current-accumulator current-element)))))
  
  (iter a acum))


; Pruebas
 (filter-acum 1 10 + 0 odd?)
 (filter-acum 1 10 + 0 even?)



;======================================================

; PUNTO: 13 

; funciones auxiliares
; operate-aux lrators-rev lrands-rev el proposito es organizar las operaciones
; y los números para poderlos operar.

; operate-aux : lrators-rev lrands-rev -> number

; <lrators-rev>  ::= ({<operators>}*) 
; <operators> ::= + | - | * | /
;
; <lrands> ::= ({<int>}*)

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

; Pruebas
(operate-aux (list / * -) '(4 4 7 8)) ; salida -> 1 (opera de atras hacia delante)
(operate-aux (list * * -) '(4 5 7 8)) ; salida -> 20
; El proposito de esta fucnión auxiliar es revertir la lista para poder organizar
; el orden en que se ejecutan las operciones por la jerarquia en matematicas.
;
; rev : lst -> lst'
;
; <rev>  ::= ({<scheme-value>}*) 

(define (rev lst)
 (if (null? lst)
     '() (my-append (rev (cdr lst)) (list (car lst)))))

(rev '(1 2 3 4)) ; salida -> (4 3 2 1)
(rev '(3 4 7 1)) ; salida -> (1 7 4 3)


; operate 
; uso (operate lrators lrands) ->
; El propósito de la función (operate lrators lrands) es realizar 
; una serie de operaciones utilizando los operadores en la lista  
; lrators y los números en la lista lrands, retornando el resultado 
; final de estas operaciones.
;
; operate : lrators lrands -> int 
; recibe dos listas, hace las operaciones y retorna un entero
; 
; <lrators>  ::= ({<operators>}*) 
; <operators> ::= + | - | * | /
;
; <lrands> ::= ({<int>}*)

(define operate
  (lambda (lrators lrands)
    (operate-aux (rev lrators) (rev lrands))))


;Pruebas

(operate (list + * + - *) '(1 2 8 4 11 6)) ; salida -> 102
(operate (list * * -) '(4 5 7 8)) ; salida -> 132
(operate (list / * -) '(4 4 7 8)) ; salida -> -1


;======================================================

; PUNTO: 14
; (Path n BST) -> El propósito es buscar obtener la ruta hasta encontrar el número dentro de un arbol binario
; definido, por ejemplo quiero buscar un el 7 en mi arbol, y este se encuentra dos nodos
; a la derecha, al ingresar el número que busco y el arbol donde debo buscarlo debería
; retornarme derecha derecha
;
; number tree -> L1
;
; <expression>  ::= <number> <binary-tree>
; <binary-tree> ::= '() 
;               ::= ( <int> <binary-tree> <binary-tree> ) 
       

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
(31 () ()))))  ; salida -> (right left left)

(path 12 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ())))) ; salida -> (left right)

;======================================================

; PUNTO: 15

; uso (count-odd-and-even arbol) -> propósito: contar la cantidad de pares e impares
; dentro del arbol
;
; tree -> <list (num1 num2)>
;
; <expression> :== '()
;              ::= ( <int> <binary-tree> <binary-tree> ) 

(define count-odd-and-even
  (lambda (arbol)
    (define (count-odd-and-even-helper nodo)
      (cond ((null? nodo) '(0 0))
            (else
             (let* ((left-counts (count-odd-and-even-helper (cadr nodo)))
                    (right-counts (count-odd-and-even-helper (caddr nodo)))
                    (current-counts
                     (if (even? (car nodo))
                         (list (+ 1 (car left-counts) (car right-counts))
                               (+ (cadr left-counts) (cadr right-counts)))
                         (list (+ (car left-counts) (car right-counts))
                               (+ 1 (cadr left-counts) (cadr right-counts))))))
               current-counts))))
    (count-odd-and-even-helper arbol)))

; pruebas
(count-odd-and-even '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))

(count-odd-and-even '(5 (4 () ()) (3 () (2 () ()))))

(count-odd-and-even '(7 (2 (1 () ()) (4 () ())) (9 (8 () ()) (6 () ()))))


;======================================================

; Punto: 16
; Operar-binarias
; El propósito de la función (Operar-binarias operacionB) es aplicar 
; una operación binaria válida (como suma, resta o multiplicación) a dos 
; valores específicos y retornar el resultado (entero) de esas operaciones.
; 
; Operar-binarias : operacion -> int
;
; <OperacionB>::= <int>
;            ::= (<OperacionB> 'suma <OperacionB>)
;            ::= (<OperacionB> 'resta <OperacionB>)
;            ::= (<OperacionB> 'multiplica <OperacionB>)ty


(define Operar-binarias
  (lambda (operacion)
    (cond
      [(number? operacion) operacion] ; Si es un número, simplemente lo retornamos
      [(list? operacion)
       (let ((izquierda (car operacion))
             (operador (cadr operacion))
             (derecha (caddr operacion)))
         (cond
           [(equal? operador 'suma) (+ (Operar-binarias izquierda) (Operar-binarias derecha))]
           [(equal? operador 'resta) (- (Operar-binarias izquierda) (Operar-binarias derecha))]
           [(equal? operador 'multiplica) (* (Operar-binarias izquierda) (Operar-binarias derecha))]
           [else ("Operador desconocido")]))]
      [else ("Operación no válida")])))

; Pruebas
(Operar-binarias '( (2 multiplica (4 suma 1) ) multiplica ( (2 multiplica 4) resta 1 ) ) )  ; salida -> 70
(Operar-binarias '(2 suma 12) )  ; salida -> 14
(Operar-binarias '( (2 resta (4 suma 1) ) multiplica ( (2 multiplica 4) multiplica 2 ) ) )  ; salida -> -48
;======================================================

; PUNTO: 17

; Uso(prod-scalar-matriz-aux L1 L2) -> Propósito prod-scalar-matriz-aux: su propósito
; es una multiplicación escalar
; entre una fila de una matriz y un vector (una lista de números).
; Se busca calcular el producto escalar entre la fila y el vector de manera recursiva.
;
; {int]* {int}* -> {int}*
;
; <espression> ::= {int}* {int}*

(define prod-scalar-matriz-aux
  (lambda (fila vec)
    (cond
      ((or (null? fila) (null? vec)) '())
      (else (cons (* (car fila) (car vec))
                  (prod-scalar-matriz-aux (cdr fila) (cdr vec)))))))

; Uso (prod-scalar-matriz L1 L2) -> Propósito prod-scalar-matriz: calcular el producto
; escalar entre una matriz
; una lista de listas) y un vector (una lista de números).
; Esta función utiliza la función auxiliar prod-scalar-matriz-aux para realizar el cálculo
; en cada fila de la matriz y luego combina los resultados en una nueva matriz.
;
; {int]* {int}* -> {int}*
;
; <espression> ::= {int}* {int}*

(define prod-scalar-matriz
  (lambda (mat vec)
    (cond
      ((or (null? mat) (null? vec)) '())
      (else (cons (prod-scalar-matriz-aux (car mat) vec)
                  (prod-scalar-matriz (cdr mat) vec))))))

;pruebas
(prod-scalar-matriz '((1 1) (2 2)) '(2 3)) ; salida -> ((2 3) (4 6))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3)) ; salida -> ((2 3) (4 6) (6 9))
(prod-scalar-matriz '((0 0) (0 0) (0 0)) '(2 3)) ; salida -> ((0 0) (0 0) (0 0))
(prod-scalar-matriz '((1 2 3) (4 5 6)) '(2 3 4 5)) ; salida -> ((2 6 12) (8 15 24))

;=====================================================

; PUNTO 18
; pascal
; El propósito de la función `(pascal N)` es generar y retornar la fila número
; `N` del Triángulo de Pascal, donde cada elemento de la fila es calculado como
; la suma de los dos números directamente encima de él en la fila anterior.
;
; pascal : n -> list
;
; <pascal>::= <int>

(define pascal
  (lambda (n)
    (define (get-element lst index)
      (if (= index 0)
          (car lst)
          (get-element (cdr lst) (- index 1))))
    
    (define (build-row prev prev-padded next-prev new-row i)
      (cond ((< i n)
             (build-row prev prev-padded next-prev
                        (my-append new-row
                                   (list (+ (get-element prev-padded i)
                                            (get-element next-prev i))))
                        (+ i 1)))
            (else new-row)))
    
    (cond ((= n 0)
           (list 1))
          (else
           (let* ((prev (pascal (- n 1)))
                  (prev-padded (my-append prev (list 0)))
                  (next-prev (my-append (list 0) prev))
                  (new-row '())
                  (i 0))
             (build-row prev prev-padded next-prev new-row i))))))

; Pruebas :
(pascal 2) ; salida -> (1 1)
(pascal 7) ; salida -> (1 6 15 20 15 6 1)
