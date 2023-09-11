#lang eopl

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
             (head (car L)) ;tal vex se pueda quitar
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
      (cons (list (car L)) (down (cdr L)))))


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

