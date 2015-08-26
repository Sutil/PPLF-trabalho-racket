#lang racket

(require rackunit)
(require rackunit/text-ui)


;#
;(define exemplo-tests
;  (test-suite
;   "exemp tests"
;   (check-equal? (cons-fim 3 empty) (list 3))
;   (check-equal? (cons-fim 3 (list 5)) (list 5 3))
;   (check-equal? (cons-fim 8 (list 2 5)) (list 2 5 8))))


;;;;;;;;;;;;;;;; Exercício 3.1 ;;;;;;;;;

;Defina uma função que verifique se um determinado elemento está em uma lista.

(define esta-na-lista-tests
  (test-suite
   "está na lista?"
   (check-equal? (esta-na-lista? empty 1) #f)
   (check-equal? (esta-na-lista? (list 5) 5) #t)
   (check-equal? (esta-na-lista? (list 2 5 3 7) 3) #t)
   (check-equal? (esta-na-lista? (list 2 5 3 7) 4) #f)))

(define (esta-na-lista? lst n)
  (cond
    [(empty? lst) #f]
    [(equal? n (first lst)) #t]
    [else (esta-na-lista? (rest lst) n)]))


;;;;;;;;;;;; Exercício 3.2 ;;;;;;;;;;;;;;
;;Defina uma função que receba com entrada uma lista lst e um elemento a e devolva uma lista que é
;;como lst mas sem as ocorrências de a.

(define remove-n-tests
  (test-suite
   "remove n da lista"
   (check-equal? (remove-n empty 1) empty)
   (check-equal? (remove-n (list 5) 5) empty)
   (check-equal? (remove-n (list 2 5 3 5 7 5) 5) (list 2 3 7))))

(define (remove-n lst n)
  (cond
    [(empty? lst) empty]
    [(if(equal? (first lst) n)
        (remove-n (rest lst) n)
        (cons (first lst) (remove-n (rest lst) n)))]))
    

;;;; Exercício 3.7 ;;;;;;
;;Defina uma função
;;que receba como entrada uma lista lst de número naturais e devolva uma lista
;que é como lst mas sem números pares

(define par-tests
  (test-suite
   "pares"
   (check-equal? (par? 7) #f)
   (check-equal? (par? 8) #t)))


(define pares-nataurais-tests
  (test-suite
   "pares naturais"
   (check-equal? (sem-pares-naturais empty) empty)
   (check-equal? (sem-pares-naturais (list 5)) (list 5))
   (check-equal? (sem-pares-naturais (list 2)) empty)
   (check-equal? (sem-pares-naturais (list 2 5 3 6 7 5)) (list 5 3 7 5))))

(define (par? x)
    (equal? (modulo x 2) 0))

(define (sem-pares-naturais lst)
  (cond
    [(empty? lst) empty]
    [(not(par? (first lst))) (cons (first lst) (sem-pares-naturais (rest lst))) ]
    [else (sem-pares-naturais (rest lst))]))


;;;;;;; Exercício 3.8 ;;;;;;
;Defina uma função que devolva o último elemento de uma lista.
;Use a função error (com uma string de mensagem como argumento) para indicar erro se a lista for vazia.

(define ultimo-numero-tests
  (test-suite
   "último número"
   (check-exn exn:fail? (thunk (ultimo-numero empty)))
   (check-equal? (ultimo-numero (list 5)) 5)
   (check-equal? (ultimo-numero (list 2 5 3 6 7)) 7)))

(define (ultimo-numero lst)
  (cond
    [(empty? lst) (error "Lista vazia")]
    [(empty? (rest lst)) (first lst)]
    [else (ultimo-numero (rest lst))]))


;;;;;;; Exercício 3.11 ;;;;;;

(define (insere-ordenado n lst)
  (cond
    [(empty? lst) (list n)]
    [(n . < . (first lst)) (cons n lst)]
    [else (cons (first lst)
                (insere-ordenado n (rest lst)))]))

(define ordena-crescente-tests
  (test-suite
   "ordena crescente"
   (check-equal? (ordena-crescente empty) empty)
   (check-equal? (ordena-crescente (list 2 5 3 6 7)) (list 2 3 5 6 7))
   (check-equal? (ordena-crescente (list 2 5 3 6 7 1 3)) (list 1 2 3 3 5 6 7))))


(define (ordena-crescente-lista lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [else (ordena-crescente-lista (rest lst1) (insere-ordenado (first lst1) lst2) )  ]))

(define (ordena-crescente lst)
  (ordena-crescente-lista lst empty))

          

;;;;;;; Exercício 3.12 ;;;;;;

(define remove-duplicados-tests
  (test-suite
   "remove duplicados"
   (check-equal? (remove-duplicados empty) empty)
   (check-equal? (remove-duplicados (list 2 2 5 3 6 6 7)) (list 2 5 3 6 7))
   (check-equal? (remove-duplicados (list 2 2 5 5 5 5 3 3 6 6 7 7 1 1 3 3)) (list 2 5 3 6 7 1 3))))

(define (remove-duplicados lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) (cons (first lst) empty)]
    [else(if (equal? (first lst) (first (rest lst)))
             (remove-duplicados (rest lst))
             (cons (first lst) (remove-duplicados (rest lst))))]
    ))

;;;;;;; Exercício 3.13 ;;;;;;


;;;;;;; Exercício 3.15 ;;;;;;

;;;;;;; Exercício 3.16 ;;;;;;

;;;;;;; Exercício 3.17 ;;;;;;



;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes esta-na-lista-tests
                remove-n-tests
                par-tests
                pares-nataurais-tests
                ultimo-numero-tests
                ordena-crescente-tests
                remove-duplicados-tests
                )
     