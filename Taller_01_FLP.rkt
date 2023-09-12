#lang eopl

(define (my-append lista1 lista2)
  (if (null? lista1)
      lista2
      (cons (car lista1) (my-append (cdr lista1) lista2))))

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
             (head (car L)) ;tal vez se pueda quitar
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

; PUNTO: 4
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

; PUNTO: 11
; Elabore una función llamada zip que recibe como entrada tres
; parámetros: una función binaria (función que espera recibir dos argumentos)
; F, y dos listas L1 y L2, ambas de igual tamaño. El procedimiento zip
; debe retornar una lista donde la posición n-ésima corresponde al resultado
; de aplicar la función F sobre los elementos en la posición n-ésima en L1 y
; L2.

(define (my-length L1)
  (if
   (null? L1)
     0
    (+ 1 (my-length (cdr L1)))))

(define (zip F L1 L2)
  (if (= (my-length L1) (my-length L2))
      (if (null? L1)
          '()
          (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2))))
      '()))

;======================================================

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

;======================================================

; PUNTO: 17
; Elabore una función llamada (prod-scalar-matriz mat vec) que recibe una matriz mat
; representada como una lista de listas y un vector vec representado como una lista,
; y retorna el resultado de realizar la multiplicación matriz por vector.

