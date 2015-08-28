#lang racket

(require rackunit)
(require rackunit/text-ui)


;;;;;; Exercício 5.3 ;;;;;
(define apaga-tests
  (test-suite
   "Exercício 5.1"
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

;;;;;; Exercício 5.5 ;;;;;

;;;;;; Exercício 5.6 ;;;;;

;;;;;; Exercício 5.7 ;;;;;

;;;;;; Exercício 5.8 ;;;;;











;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes apaga-tests
                )