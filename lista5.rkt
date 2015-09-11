#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;; Exercício 5.3 ;;;;;;;;;;;;;;;;
;;Defina uma função que devolve um nova lista sem os n primeiros elementos de uma dada lista.
;;Lista Natural -> Lista
;;Devolve uma lista sem a ocorrencia dos n primeiros elementos ou erro se a lista for vazia.

(define exercicio-5-3-tests
  (test-suite
   "Exercício 5.2"
   (check-equal? (apaga (list 10) 0) (list 10))
   (check-exn exn:fail? (thunk (apaga empty 1)))
   (check-equal? (apaga (list 10 40 70 20 3) 2) (list 70 20 3))
   ))

(define (apaga lst n)
  (cond
    [(zero? n) lst]
    [(empty? lst) (error "Lista vazia")]
    [else (apaga (rest lst) (sub1 n))])) 


;;;;;;;;;;;;;;;; Exercício 5.4 ;;;;;;;;;;;;;;;;
;;Defina uma função que devolva uma nova lista que é como a lista de entrada mas sem o elemento de
;;uma dada posição.
;;Lista Natural -> Lista
;;Devolve a lista de entrada mas sem n-esimo elemento ou erro se a lista for vazia.

(define exercicio-5-4-tests
  (test-suite
   "Exercício 5.4"
   (check-equal? (apaga-em (list 10) 0) empty)
   (check-exn exn:fail? (thunk (apaga-em empty 1)))
   (check-equal? (apaga-em (list 10 40 70 20 3) 2) (list 10 40 20 3))
   (check-equal? (apaga-em (list 3 6 1 2) 2) (list 3 6 2))
   ))

(define (apaga-em lst p)
  (cond
    [(zero? p) (rest lst)]
    [(empty? lst) (error "Lista vazia")]
    [else (cons (first lst)(apaga-em (rest lst) (sub1 p)))]))
  

;;;;;;;;;;;;;;;; Exercício 5.5 ;;;;;;;;;;;;;;;;
;;Defina uma função que devolva uma lista que é como a lista de entrada mas com um dado elemento
;;inserido em uma dada posição.
;; Lista Natural Natural -> Lista
;;Devolve a lista de entrada mas com o n na posição p da lista ou erro se a posição for inválida.

(define exercicio-5-5-tests
  (test-suite
   "Exercício 5.5"
   (check-equal? (insere-em (list 10) 1 0) (list 1 10))
   (check-equal? (insere-em (list 10 40 70 20 3) 7 2) (list 10 40 7 70 20 3))
   (check-exn exn:fail? (thunk (insere-em (list 3 6 1 2) 2 5)))
   ))

(define (insere-em lst n p)
  (cond
    [(> p (add1 (length lst))) (error "Posição maior que lista") ]
    [(and (empty? lst) (zero? p))  (cons n empty)]
    [(zero? p) (cons n lst)]
    [else (cons (first lst) (insere-em (rest lst) n (sub1 p)))]))


;;;;;;;;;;;;;;;; Exercício 5.6 ;;;;;;;;;;;;;;;;
;;Defina uma função que devolva uma sub-lista de uma lista com um intervalo especificado.
;;Lista Natural Natural -> Lista
;;Devolve uma lista que é sublista da lst cujo o primeiro elemento é i e o último é f ou
;;erro se a lista for vazia ou o intervalo (i f) for inválido.

(define exercicio-5-6-tests
  (test-suite
   "Exercício 5.6"
   (check-equal? (sub-lista (list 10 20 30 40 50) 1 1) empty)
   (check-equal? (sub-lista (list 10 20 30 40 50) 1 4) (list 20 30 40))
   (check-exn exn:fail? (thunk (sub-lista empty 2 5)))
   (check-exn exn:fail? (thunk (sub-lista (list 1 2 3) 1 0)))
   ))

(define (sub-lista lst i f)
  (cond
    [(empty? lst) (error "Lista Vazia")]
    [(> i f) (error "Intervalo inválido")]
    [(= i f) empty]
    [(zero? i) (cons (first lst) (sub-lista (rest lst) i (sub1 f)))]
    [else (sub-lista (rest lst) (sub1 i) (sub1 f))]))


;;;;;;;;;;;;;;;; Exercício 5.7 ;;;;;;;;;;;;;;;;
;;Defina uma função que devolva uma nova lista que é como a lista de entrada mas com os
;;elementos rotacionados n posições a esquerda.
;;Lista Natural -> Natural
;;Devolve uma lista como lst, mas com os elementos r posições a esquerda ou 
;;erro caso a lista seja vazia.

(define exercicio-5-7-tests
  (test-suite
   "Exercício 5.7"
   (check-equal? (rotacao-esq (list 10 20 30 40 50) 1) (list 20 30 40 50 10))
   (check-equal? (rotacao-esq (list 10 20 30 40 50) 4) (list 50 10 20 30 40))
   (check-equal? (rotacao-esq (list 10 20 30) 3) (list 10 20 30))
   (check-exn exn:fail? (thunk (rotacao-esq empty 2)))
   ))

(define (rotacao-esq lst r)
  (cond
    [(empty? lst) (error "Lista vazia")]
    [(zero? r) lst]
    [else  (rotacao-esq
            (append (rest lst) (list (first lst)))
            (sub1 r)) ]))


;;;;;;;;;;;;;;;; Exercício 5.8 ;;;;;;;;;;;;;;;;
;;Dado duas listas de entrada, ltsa e ltsb, defina uma função que devolva uma 
;;nova lista com os elementos de ltsa seguidos dos elementos de ltsb.
;;Lista Lista -> Lista 
;;Devolve uma lista com todos os elementos de lst1 seguido dos elementos de lst2.

(define exercicio-5-8-tests
  (test-suite
   "Exercício 5.8"
   (check-equal? (juncao empty empty) empty)
   (check-equal? (juncao (list 3 7 12) (list 2 4 5)) (list 3 7 12 2 4 5))
   (check-equal? (juncao (list 1) (list 2)) (list 1 2))
   ))

(define (juncao lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (cons (first lst1) (juncao (rest lst1) lst2))]))

;;;;;;;;;;;; Executa tests ;;;;;;;;;;;;
;; Teste ... -> Void
;; Executa um conjunto de testes.

(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.

(executa-testes exercicio-5-3-tests
                exercicio-5-4-tests
                exercicio-5-5-tests
                exercicio-5-6-tests
                exercicio-5-7-tests
                exercicio-5-8-tests
                )
