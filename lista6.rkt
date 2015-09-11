#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;; Exercício 6.8 ;;;;;;;;;;;;;;;;
;;Defina uma função que receba como parâmetro um predicado (função de um argumento que retorna
;;um valor booleano) e uma lista, e conte quantos elementos da lista satisfazem o predicado.
;;Predicado Lista -> Natural
;;Devolve o numero de elementos de lst que atende ao predicado pred?.

(define exercicio-6-8-tests
  (test-suite
   "Exercício 6.8"
   (check-equal? (cont positive? (list 1 -1 2 3 -2 5)) 4)
   (check-equal? (cont negative? (list 1 -1 2 3 -2 5)) 2)
   (check-equal? (cont positive? empty) 0)
   ))

(define (cont pred? lst)
  (define (iter pred? lst acc)
    (cond
      [(empty? lst) acc]
      [(pred? (first lst)) (iter pred? (rest lst) (add1 acc))]
      [else (iter pred? (rest lst) acc)]))
  (iter pred? lst 0))

;;;;;;;;;;;;;;;; Exercício 6.10 ;;;;;;;;;;;;;;;;
;;Defina uma função concatena que receba como parâmetro um número 
;;variável de listas e calcule a concatenação de todos os parâmetros.
;;Lista(as) -> Lista
;;Devolve a lista com todas as listas de entrada concatenada

(define exercicio-6-10-tests
  (test-suite
   "Exercício 6.10"
   (check-equal? (concatena (list 1 2 3) (list 4) (list 5 6)) (list 1 2 3 4 5 6))
   (check-equal? (concatena (list 3) (list 4)) (list 3 4))
   (check-equal? (concatena (list 1 2)) (list 1 2))
   ))

(define (cctn listas)
  (cond
    [(empty? listas) empty]
    [(empty? (rest listas)) (first listas)]
    [else (append (first listas) (cctn (rest listas)))]))

  
(define (concatena . listas)
  (cctn listas))
  
  
;;;;;;;;;;;;;;;; Exercício 6.11 ;;;;;;;;;;;;;;;;
;;A função map pré definida no Racket aceita como parâmetro uma função de aridade n e n listas do
;;mesmo tamanho, e aplica a função a todos os primeiros elementos das listas, depois aplica a função a
;;todos os segundos elementos das listas e assim por diante, retornando a lista de resultados. 
;;Defina a função mapeia que funciona como a função map pré-definida.
;;Função Lista(as) -> Lista
;;Devolve uma lista com cada elemento criado a partir da(s) lista(s) inicial(ais) aplicado a função f. 

(define exercicio-6-11-tests
  (test-suite
   "Exercício 6.11"
   (check-equal? (mapeia + (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 12 15 18))
   (check-equal? (mapeia - (list 10 20 30) (list 5 10 15)) (list 5 10 15))
   (check-equal? (mapeia list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))
   ))

(define (mapeia f . listas)

  (define (map-duas-listas f lst1 lst2)
    (cond
      [(not (equal? (length lst1) (length lst2))) (error "tamanhos diferentes")]
      [(empty? lst1) empty]
      [else (cons (f (first lst1) (first lst2)) (map-duas-listas f (rest lst1) (rest lst2)) )]))

  (define (mapeia-acc f acc listas)
  (cond
    [(empty? listas) acc]
    [else (mapeia-acc f (map-duas-listas f acc (first listas)) (rest listas))]))
  
  (cond
    [(empty? listas) empty]
    [else (mapeia-acc f (first listas) (rest listas))]))
    

;;;;;;;;;;;;;;;; Exercício 6.12 ;;;;;;;;;;;;;;;;
;;Defina uma função que receba um ou mais inteiros como parâmetro e retorne 
;;uma lista com os parâmetros que tenha a mesma paridade do primeiro argumento.
;;Natural(ais) -> Lista
;;Devolve uma lista com os n(s) elemento(s) que tenha(m) a mesma paridade do primeiro elemento.

(define exercicio-6-12-tests
  (test-suite
   "Exercício 6.12"
   (check-equal? (paridade 1 2 3) (list 1 3))
   (check-equal? (paridade 4) (list 4))
   (check-equal? (paridade 2 3 4 5 8 12) (list 2 4 8 12))
   ))

(define (paridade n . ns)
  (cond
    [(even? n) (cons n (filter even? ns))]
    [else (cons n (filter odd? ns))]))


;;;;;;;;;;;; Executa tests ;;;;;;;;;;;;
;; Teste ... -> Void
;; Executa um conjunto de testes.

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

;; Chama a função para executar os testes.

(executa-testes exercicio-6-8-tests
                exercicio-6-10-tests
                exercicio-6-11-tests
                exercicio-6-12-tests
                )
