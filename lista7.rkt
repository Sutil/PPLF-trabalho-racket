#lang racket
(require rackunit)
(require rackunit/text-ui)



;;;;;; Exercício 7.1 -> 3.1 ;;;;;

;;;;;; Exercício 7.1 -> 3.7 ;;;;;

;;;;;; Exercício 7.1 -> 3.9 ;;;;;


;;;;;; Exercício 7.1 -> 4.1 ;;;;;

;;;;;; Exercício 7.1 -> 4.4 ;;;;;



;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes 
                )