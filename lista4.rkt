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

(define quant-primos-tests
  (test-suite
   "Exercício 4.5"
   (check-equal? (primo? 1) #t)
   (check-equal? (primo? 5) #t)
   (check-equal? (primo? 2) #t)
   (check-equal? (primo? 3) #t)
   (check-equal? (primo? 4) #f)
   (check-equal? (primo? 15) #f)
   (check-equal? (quant-primos 1 1) 1)
   (check-equal? (quant-primos 1 2) 2)
   (check-equal? (quant-primos 1 4) 3)
   (check-equal? (quant-primos 5 19) 6)
   ))

(define (fator? x n)
  (cond
    [(= x 1) #f]
    [(zero? (modulo n x)) #t]
    [else (fator? (sub1 x) n)]))

(define (primo? n)
  (cond
    [(= n 1) #t]
    [else (not(fator? (sub1 n) n))]))

(define (quant-primos i f)
    (cond
      [(> i f) 0]
      [(primo? i) (add1 (quant-primos (add1 i) f))]
      [else (quant-primos (add1 i) f)]))



;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes fatorial-tests
                operacoes-tests
                quant-primos-tests
                )