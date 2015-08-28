#lang racket

(require rackunit)
(require rackunit/text-ui)

;;; Exercício 4.1 ;;;

(define fatorial-tests
  (test-suite
   "Exercício 4.1"
   (check-equal? (fatorial 1) 1)
   (check-equal? (fatorial 2) 2)
   (check-equal? (fatorial 3) 6)
   (check-equal? (fatorial 5) 120)
   ))

(define (fatorial n)
  (cond
    [(<= n 1) 1]
    [else (* n (fatorial (sub1 n)))]))


;;; Exercício 4.2 ;;;;

(define operacoes-tests
  (test-suite
   "Exercício 4.2"
   (check-equal? (soma 5 4) 9)
   (check-equal? (soma 2 8) 10)
   (check-equal? (subtracao 8 2) 6)
   (check-equal? (subtracao 5 5) 0)
   (check-equal? (multiplicacao 5 5) 25)
   (check-equal? (multiplicacao 2 5) 10)
   ))

(define (soma x y)
  (cond
    [(zero? x) y]
    [else (soma(sub1 x) (add1 y))]))

(define (subtracao x y)
  (cond
    [(zero? y) x]
    [else (subtracao(sub1 y) (sub1 x))]))

(define (multiplicacao x y)
  (define (mult-acc x y acc)
    (cond
      [(zero? (sub1 x)) y]
      [else (mult-acc (sub1 x) (soma y acc) acc)]))
  (mult-acc x y y)
)


;;; Exercício 4.5 ;;;;

;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes fatorial-tests
                operacoes-tests
                
                )