#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;; Exercício 6.8 ;;;

;;;; Exercício 6.10 ;;;

;;;; Exercício 6.11 ;;;

;;;; Exercício 6.12 ;;;;


;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes 
                )