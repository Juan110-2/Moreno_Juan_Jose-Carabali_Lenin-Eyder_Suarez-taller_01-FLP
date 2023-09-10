#lang racket

; PUNTO: 2
; Elabore una función llamada down que recibe como argumento
; una lista L, y lo que debe realizar dicha funcion es retornar una lista con
; cada elemento de L asociado a un nivel mas de parentesis comparado con su
; estado original en L.

(define (down L)
  (if (null? L)
      '() ; Si la lista está vacía, retornamos una lista vacía.
      (cons (list (car L)) (down (cdr L)))))