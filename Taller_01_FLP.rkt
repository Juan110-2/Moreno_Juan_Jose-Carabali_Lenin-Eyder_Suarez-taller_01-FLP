#lang eopl

(define (my-append lista1 lista2)
  (if (null? lista1)
      lista2
      (cons (car lista1) (my-append (cdr lista1) lista2))))

(define (my-length L1)
  (if
   (null? L1)
     0
    (+ 1 (my-length (cdr L1)))))

;PUNTO 1
;Elabore una función llamada invert que recibe un argumento:
;una lista L, sin embargo, esta lista L se compone de pares x; y que a su vez
;son listas (de tama~no 2). La función debe retornar una lista similar a L, con
;pares ordenados invertidos, es decir, y; x.

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

;======================================================

; PUNTO: 2
; Elabore una función llamada down que recibe como argumento
; una lista L, y lo que debe realizar dicha funcion es retornar una lista con
; cada elemento de L asociado a un nivel mas de parentesis comparado con su
; estado original en L.

(define (down L)
  (if (null? L)
      '() ; Si la lista está vacía, retornamos una lista vacía.
      (cons (list (car L)) (down (cdr L))))) ; Se crea una lista con el primer elemento, se repite con el resto

;======================================================

;PUNTO: 3
;Elabore una función llamada list-set que reciba tres argumentos:
;una lista L, un número n y un elemento x. La función debe retornar una
;lista similar a la que recibe (L), pero debe tener en la posición ingresada n
;(indexando desde cero) el elemento x. Ejemplos:


; list-set: list number any -> list
; Retorna una lista igual a L, pero con el elemento x en la posición n
(define (list-set L n x)
  (cond
    [(null? L) '()] ; Si la lista está vacía, se retorna la lista vacía
    [(= n 0) (cons x (cdr L))] ; Si n es cero, se reemplaza el primer elemento de L por x
    [else (cons (car L) (list-set (cdr L) (- n 1) x))] ; Si no, se conserva el primer elemento de L y se aplica la función recursivamente al resto de L, decrementando n en uno
  )
)

;======================================================

;PUNTO: 4
;Elabore una funciónn llamada filter-in que debe recibir dos argumentos:
;un predicado P y una lista L. La función retorna una lista que
;contiene los elementos que pertenecen a L y que satisfacen el predicado P. 

(define filter-in
  (lambda (P L)
    (if (null? L) '()
        (if (not (P (car L))) (filter-in P (cdr L))
           (cons (car L) (filter-in P (cdr L)))))))

;======================================================


; PUNTO: 5
; Elabore una función llamada list-index que debe recibir dos
; argumentos: un predicado P y una lista L. La función retorna (desde una
; posición inicial 0) el primer elemento de la lista que satisface el predicado
; L. Si llega a suceder que ningún elemento satisface el predicado recibido, la
; función debe retornar #f.

(define (list-index P L)
  (define (list-index-helper index L)
    (cond
      ((null? L) #f)                      
      ((P (car L)) index)                   
      (else (list-index-helper (+ index 1) (cdr L)))))

  (list-index-helper 0 L))

;======================================================

;PUNTO 6

;Elabore una función llamada swapper que recibe 3 argumentos:
;un elemento E1, otro elemento E2, y una lista L. La función retorna una lista
;similar a L, sólo que cada ocurrencia anterior de E1 será reemplazada por E2
;y cada ocurrencia anterior de E2 ser ́a reemplazada por E1 (Los elementos
;E1 y E2 deben pertenecer a L)
;car devuelve el primer valor de la lista


(define (swapper E1 E2 l)
  (cond [(null? l) '()] ; Si la lista está vacía, devolver una lista vacía
        [(equal? E1 (car l)) (cons E2 (swapper E1 E2 (cdr l)))] ; Si el primer elemento de la lista 'L' es igual a e1, reemplazarlo por e2 y seguir con el resto de la lista
        [(equal? E2 (car l)) (cons E1 (swapper E1 E2 (cdr l)))] ; Si el primer elemento de la lista 'L' es igual a e2, reemplazarlo por e1 y seguir con el resto de la lista
        [else (cons (car l) (swapper E1 E2 (cdr l)))] ; Si el primer elemento no es ni e1 ni e2, mantenerlo y seguir con el resto de la lista
        ))





;======================================================

;PUNTO 7
;Elabore una función llamada cartesian-product que recibe como
;argumentos 2 listas de símbolos sin repeticiones L1 y L2. La función debe
;retornar una lista de tuplas que representen el producto cartesiano entre L1
;y L2. Los pares pueden aparecer en cualquier orden.

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1) '()
        (my-append (cartesian-product-aux2 (car L1) L2) (cartesian-product (cdr L1) L2)))))

(define cartesian-product-aux2
  (lambda (L1 L2)
    (if (null? L2) '()
        (cons (list L1 (car L2)) (cartesian-product-aux2 L1 (cdr L2))))))

;========================================================

;PUNTO 9
;Elabore una función llamada inversions que recibe como entrada
;una lista L, y determina el ńumero de inversiones de la lista L. De manera formal, sea A = (a1a2...an)
;una lista de n n ́umeros diferentes, si i < j (posici ́on)
;y ai > aj (dato en la posición) entonces la pareja (i j) es una inversión de A.


(define (inversions L)
  (define (count-inversions x lst)
    (cond [(null? lst) 0] ; si la lista está vacía, no hay inversiones
           [(> x (car lst)) (+ 1 (count-inversions x (cdr lst)))] ; si el elemento es mayor que el primero de la lista, hay una inversión y se sigue con el resto de la lista
          [else (count-inversions x (cdr lst))] ; si el elemento es menor o igual que el primero de la lista, no hay inversión y se sigue con el resto de la lista
          )) (cond [(null? L) 0] ; si la lista está vacía, no hay inversiones
                    [else (+ (count-inversions (car L) (cdr L)) ; se cuentan las inversiones del primer elemento con el resto de la lista
                             (inversions (cdr L)))] ; se suman las inversiones del resto de la lista
) )

;========================================================


;PUNTO 10
;Elabore una función llamada up que recibe como entrada una
;lista L, y lo que debe realizar la función es remover un par de paréntesis a
;cada elemento del nivel más alto de la lista. Si un elemento de este nivel
;no es una lista (no tiene paréntesis), este elemento es incluido en la salida
;resultante sin modfcación alguna.

(define up
  (lambda (L)
    (cond
      [(null? L) '()]
      [(list? (car L)) (my-append (car L) (up (cdr L)))]
      [else (cons (car L) (up (cdr L)))])))

;========================================================

; PUNTO: 11
; Elabore una función llamada zip que recibe como entrada tres
; parámetros: una función binaria (función que espera recibir dos argumentos)
; F, y dos listas L1 y L2, ambas de igual tamaño. El procedimiento zip
; debe retornar una lista donde la posición n-ésima corresponde al resultado
; de aplicar la función F sobre los elementos en la posición n-ésima en L1 y
; L2.


(define (zip F L1 L2)
  (if (= (my-length L1) (my-length L2))
      (if (null? L1)
          '()
          (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2))))
      '()))

;======================================================

;PUNTO 12

;;Elabore una función llamada filter-acum que recibe como entrada 5 par ́ametros: dos n ́umeros a y b, una función binaria F, un valor inicial
;;acum y una funci ́on unaria filter. El procedimiento filter-acum aplicar ́a la
;;funci ́on binaria F a todos los elementos que est ́an en el intervalo [a, b] y que
;;a su vez todos estos elementos cumplen con el predicado de la funci ́on filter,
;;el resultado se debe ir conservando en acum y debe retornarse el valor final
;;de acum. E


(define (filter-acum a b F acum filter)
  (define (accumulate-from-a-to-b current-accumulator current-element)
    (if (and (>= current-element a)
             (<= current-element b)
             (filter current-element))
        (F current-accumulator current-element)
        current-accumulator))
  
  (define (iter current-element current-accumulator)
    (if (> current-element b)
        current-accumulator
        (iter (+ current-element 1) (accumulate-from-a-to-b current-accumulator current-element))))
  
  (iter a acum))




;========================================================

; PUNTO: 14
; Elabore una función llamada path que recibe como entrada dos
; parámetros: un número n y un árbol binario de búsqueda (representado
; con listas) BST (el árbol debe contener el número entero n). La función
; debe retornar una lista con la ruta a tomar (iniciando desde el nodo raíz
; del árbol), indicada por cadenas left y right, hasta llegar al número n
; recibido. Si el número n es encontrado en el nodo raíz, el procedimiento
; debe retornar una lista vacía.


(define (path n BST)
  (define (path-helper n BST current-path)
    (cond
      ((null? BST) '()) ; Si el árbol está vacío, retornamos una lista vacía.
      ((= n (car BST)) current-path) ; Si encontramos n, retornamos la ruta actual.
      ((< n (car BST)) (path-helper n (cadr BST) (my-append current-path '(left)))) ; Si n es menor, vamos a la izquierda.
      (else (path-helper n (caddr BST) (my-append current-path '(right)))))) ; Si n es mayor, vamos a la derecha.

  (path-helper n BST '())) ; Iniciamos la búsqueda desde la raíz con una ruta vacía.

;========================================================

;PUNTO: 15
;Elabore una función llamada (count-odd-and-even arbol) que
;toma un  ́arbol binario y retorna una lista con dos elementos correspondientes
;a la cantidad de pares e impares en arbol.


(define count-odd-and-even
  (lambda (arbol)
    (define (count-odd-and-even-helper nodo)
      (cond ((null? nodo) '(0 0))
            (else
             (let* ((left-counts (count-odd-and-even-helper (cadr nodo)))
                    (right-counts (count-odd-and-even-helper (caddr nodo)))
                    (current-counts
                     (if (odd? (car nodo))
                         (list (+ 1 (car left-counts) (car right-counts))
                               (+ (cadr left-counts) (cadr right-counts)))
                         (list (+ (car left-counts) (car right-counts))
                               (+ 1 (cadr left-counts) (cadr right-counts))))))
               current-counts))))
    (count-odd-and-even-helper arbol)))

;======================================================

; PUNTO: 17
; Elabore una función llamada (prod-scalar-matriz mat vec) que recibe una matriz mat
; representada como una lista de listas y un vector vec representado como una lista,
; y retorna el resultado de realizar la multiplicación matriz por vector.
