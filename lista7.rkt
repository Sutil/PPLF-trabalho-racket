#lang racket
(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;; Exercício 7.1 ;;;;;;;;;;;;;;;;;
;; ACUMULADORES
;;Quando usamos acumuladores, conseguimos fazer recursão em calda 
;;resolvendo, a cada iteração, uma parte do problema. Aqui o processo se
;;torna Itarativo, desta forma, o compilador consegue otimizar a execução da função.
;;Por outro lado, quando não usamos acumuladores, a resolução do problema acontece 
;;no retorno da recursao e o compilador não consegue otimizar.

;;;;;;;;;;;;; Exercício 7.1 -> 3.2 ;;;;;;;;;;;;;
;;Defina uma função que receba com entrada uma lista lst e um elemento a e 
;;devolva uma lista que é como lst mas sem as ocorrências de a.
;;Lista Natural -> Lista
;;Devolve uma lista mas sem qualquer ocorrencia de n.

(define exercicio-7-1-3-2-tests
  (test-suite
   "Exercício 7.1 -> 3.2"
   (check-equal? (remove-n empty 1) empty)
   (check-equal? (remove-n (list 5) 5) empty)
   (check-equal? (remove-n (list 2 5 3 7 5) 5) (list 2 3 7))
   ))

(define (remove-n lst n)
  (define (iter lst0 n0 acc)
    (cond
      [(empty? lst0) acc]
      [(equal? n0 (first lst0)) (iter (rest lst0) n0 acc)]
      [else (iter (rest lst0) n0 (append acc (list(first lst0))))]))
  (iter lst n empty))


;;;;;;;;;;;;; Exercício 7.1 -> 3.7 ;;;;;;;;;;;;;
;;Defina uma função que receba como entrada uma lista lst de 
;;número naturais e devolva uma lista que é como lst mas sem números pares
;;Lista -> Lista
;;Devolve uma lista, mas sem a ocorencia de numeros pares.

(define exercicio-7-1-3-7-tests
  (test-suite
   "Exercício 7.1 -> 3.7"
   (check-equal? (sem-pares-naturais empty) empty)
   (check-equal? (sem-pares-naturais (list 5)) (list 5))
   (check-equal? (sem-pares-naturais (list 2)) empty)
   (check-equal? (sem-pares-naturais (list 2 5 3 6 7 5)) (list 5 3 7 5))))

(define (par? x)
    (equal? (modulo x 2) 0))

(define (sem-pares-naturais lst)
  (define (iter lst0 acc)
    (cond
      [(empty? lst0) acc]
      [(par? (first lst0)) (iter (rest lst0) acc)]
      [else (iter (rest lst0) (append acc (list (first lst0))))]))
  (iter lst empty))


;;;;;;;;;;;;; Exercício 7.1 -> 3.9 ;;;;;;;;;;;;;
;;Defina uma função que encontre o valor máximo de uma lista de números.
;;Lista -> Número
;;Devolve o valor máximo de lst. Se a lista for vazia, gera um erro.

(define exercicio-7-1-3-9-tests
  (test-suite
   "Exercício 7.1 -> 3.9"
   (check-exn exn:fail? (thunk (maximo empty)))
   (check-equal? (maximo (list 4)) 4)
   (check-equal? (maximo (list 2 4 8 3)) 8)
   (check-equal? (maximo (list 8 4 8 3)) 8)))

(define (max2 a b)
  (if (>= a b) a b))

(define (maximo lst)
  (define (iter lst0 max-acc)
    (cond
      [(empty? lst0) max-acc]
      [else (iter (rest lst0) (max2 max-acc (first lst0)))]))

  (cond
    [(empty? lst) (error "Lista vazia")]
    [else (iter lst (first lst))]))


;;;;;;;;;;;;; Exercício 7.2 -> 4.1 ;;;;;;;;;;;;;
;;Defina uma função que calcule o fatorial de um número.
;;Natural -> Natural
;;Devolve o fatorial de um numero

(define exercicio-7-2-4-1-tests
  (test-suite
   "Exercício 7.2 -> 4.1"
   (check-equal? (fatorial 1) 1)
   (check-equal? (fatorial 2) 2)
   (check-equal? (fatorial 3) 6)
   (check-equal? (fatorial 5) 120)
   ))

(define (fatorial n)
  (define (iter x acc)
    (cond
      [(<= x 1) acc]
      [else (iter (sub1 x) (* acc x))]))
  (iter n 1))


;;;;;;;;;;;;; Exercício 7.2 -> 4.5 ;;;;;;;;;;;;;
;;Defina uma função que conte quantos números primos existem em um dado intervalo.
;;Natural Natural -> Natural
;;Devolve a quantidade de numeros primos existentes entre i f

(define exercicio-7-2-4-5-tests
  (test-suite
   "Exercício 7.2 -> 4.5"
   (check-equal? (quant-primos 1 1) 0)
   (check-equal? (quant-primos 1 2) 1)
   (check-equal? (quant-primos 1 4) 2)
   (check-equal? (quant-primos 5 19) 6)
   ))

(define (fator? x n)
  (cond
    [(= x 1) #f]
    [(zero? (modulo n x)) #t]
    [else (fator? (sub1 x) n)]))

(define (primo? n)
  (cond
    [(= n 1) #f]
    [else (not(fator? (sub1 n) n))]))

(define (quant-primos i f)
    (define (iter i f acc)
      (cond
        [(> i f) acc]
        [(primo? i) (iter (add1 i) f (add1 acc))]
        [else (iter (add1 i) f acc)]))
  (iter i f 0))
      

;;;;;;;;;;;; Executa tests ;;;;;;;;;;;;
;; Teste ... -> Void
;; Executa um conjunto de testes.

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

;; Chama a função para executar os testes.

(executa-testes exercicio-7-1-3-2-tests
                exercicio-7-1-3-7-tests
                exercicio-7-1-3-9-tests
                exercicio-7-2-4-1-tests
                exercicio-7-2-4-5-tests
                )
