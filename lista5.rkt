#lang racket

(require rackunit)
(require rackunit/text-ui)


;;;;;; Exercício 5.3 ;;;;;
(define apaga-tests
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

;;;;;; Exercício 5.4 ;;;;;

(define apaga-em-tests
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
  

;;;;;; Exercício 5.5 ;;;;;

(define insere-em-tests
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

;;;;;; Exercício 5.6 ;;;;;

(define sub-lista-tests
  (test-suite
   "Exercício 5.6"
   (check-equal? (sub-lista (list 10 20 30 40 50) 1 4) (list 20 30 40))
   (check-exn exn:fail? (thunk (sub-lista empty 2 5)))
   (check-exn exn:fail? (thunk (sub-lista (list 1 2 3) 1 0)))
   ))

(define (sub-lista lst i f)
  (cond
    [(empty? lst) (error "Lista Vazia")]
    [(> i f) (error "Intervalo inválido")]
    [else (and (zero? i)
          empty)]))

;;;;;; Exercício 5.7 ;;;;;

;;;;;; Exercício 5.8 ;;;;;



;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes apaga-tests
                apaga-em-tests
                insere-em-tests
                sub-lista-tests
                )