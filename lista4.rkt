#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;; Exercício 4.1 ;;;;;;;;;;;;
;;Defina uma função que calcule o fatorial de um número.
;;Natural -> Natural
;;Devolve o fatorial de um numero

(define exercicio-4-1-tests
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


;;;;;;;;;;;; Exercício 4.2 ;;;;;;;;;;;;
;;Utilizando apenas as funções primitivas zero?, add1 e sub1, escreva as funções +, - e *. 
;;Cada função deve receber como parâmetro dois números naturais e executar a operação aritmética apropriada.
;;Natural Natural -> Natural
;;Definição individual.

(define exercicio-4-2-tests
  (test-suite
   "Exercício 4.2"
   (check-equal? (soma 5 4) 9)
   (check-equal? (soma 2 8) 10)
   (check-equal? (subtracao 8 2) 6)
   (check-equal? (subtracao 5 5) 0)
   (check-equal? (multiplicacao 5 5) 25)
   (check-equal? (multiplicacao 2 5) 10)
   ))

;;Devolve a soma de dois numeros naturais.
(define (soma x y)
  (cond
    [(zero? x) y]
    [else (soma(sub1 x) (add1 y))]))

;;Devolve a subtração entre dois numeros naturais.
(define (subtracao x y)
  (cond
    [(zero? y) x]
    [else (subtracao(sub1 y) (sub1 x))]))

;;Devolve a multiplicação entre dois numeros naturais.
(define (multiplicacao x y)
  (define (mult-acc x y acc)
    (cond
      [(zero? (sub1 x)) y]
      [else (mult-acc (sub1 x) (soma y acc) acc)]))
  (mult-acc x y y)
)


;;;;;;;;;;;; Exercício 4.5 ;;;;;;;;;;;;
;;Defina uma função que conte quantos números primos existem em um dado intervalo.
;;Natural Natural -> Natural
;;Devolve a quantidade de numeros primos existentes entre i f

(define exercicio-4-5-tests
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


;;;;;;;;;;;; Executa tests ;;;;;;;;;;;;
;; Teste ... -> Void
;; Executa um conjunto de testes.

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

;; Chama a função para executar os testes.

(executa-testes exercicio-4-1-tests
                exercicio-4-2-tests
                exercicio-4-5-tests
                )
