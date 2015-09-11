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
   "Exercicio 4.5"
   (check-equal? (conta-primos 2 1) "Erro intervalo invalido.")
   (check-equal? (conta-primos 0 0) 0)
   (check-equal? (conta-primos 1 1) 0)
   (check-equal? (conta-primos 0 3) 2)
   (check-equal? (conta-primos 0 100) 25)
   ))

(define (conta-primos x y)
  (cond
    [(> x y) "Erro intervalo invalido."]
    [(> x (sub1 y)) 0]
    [else
     (if (primo? y)
           (add1 (conta-primos x (sub1 y)))
           (conta-primos x (sub1 y)))]))

;;Natural -> Boolean
;;Devolve verdadeiro se um numero é primo, falso caso contrario
(define primo?-test
  (test-suite
   "Primo?"
   (check-equal? (primo? 0) #f)
   (check-equal? (primo? 1) #f)
   (check-equal? (primo? 2) #t)
   (check-equal? (primo? 3) #t)
   (check-equal? (primo? 4) #f)
   (check-equal? (primo? 5) #t)
   ))

(define (primo? x)
  (cond
    [(<= x 1) #f]
    [else
     (not(tem-divisor-entre-2-i? x (sub1 x)))]))

;;Natural -> Natural
;;Devolve verdadeiro se existe ao menos um divisor de x entre 2 e i,
;;falso caso contrario.
(define tem-divisor-entre-2-i?-test
  (test-suite
   "tem-divisor-entre-2-i?"
   (check-equal? (tem-divisor-entre-2-i? 0 1) #f)
   (check-equal? (tem-divisor-entre-2-i? 2 1) #f)
   (check-equal? (tem-divisor-entre-2-i? 2 2) #t)
   (check-equal? (tem-divisor-entre-2-i? 5 4) #f)
   (check-equal? (tem-divisor-entre-2-i? 5 9) #t)
   ))

(define (tem-divisor-entre-2-i? x d)
  (cond
    [(<= d 1) #f]
    [else
     (if(zero? (modulo x d))
          #t
          (tem-divisor-entre-2-i? x (sub1 d)))]))


;;;;;;;;;;;; Executa tests ;;;;;;;;;;;;
;; Teste ... -> Void
;; Executa um conjunto de testes.

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

;; Chama a função para executar os testes.

(executa-testes exercicio-4-1-tests
                exercicio-4-2-tests
                primo?-test
                tem-divisor-entre-2-i?-test
                exercicio-4-5-tests
                )
