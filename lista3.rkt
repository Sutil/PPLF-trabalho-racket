#lang racket

(require rackunit)
(require rackunit/text-ui)

;#
;(define exemplo-tests
;  (test-suite
;   "exemp tests"
;   (check-equal? (cons-fim 3 empty) (list 3))
;   (check-equal? (cons-fim 3 (list 5)) (list 5 3))
;   (check-equal? (cons-fim 8 (list 2 5)) (list 2 5 8))))

;;;;;;;;;;;;;;;; Exercício 3.1 ;;;;;;;;;
;Defina uma função que verifique se um determinado elemento está em uma lista.
;Lista -> Boolean
;Devolve verdadeiro se o elemento n está na lista, falso caso contrario.

(define exercicio-3-1-test
  (test-suite
   "Exercício 3.1"
   (check-equal? (esta-na-lista? empty 1) #f)
   (check-equal? (esta-na-lista? (list 5) 5) #t)
   (check-equal? (esta-na-lista? (list 2 5 3 7) 3) #t)
   (check-equal? (esta-na-lista? (list 2 5 3 7) 4) #f)))

(define (esta-na-lista? lst n)
  (cond
    [(empty? lst) #f]
    [(equal? n (first lst)) #t]
    [else (esta-na-lista? (rest lst) n)]))


;;;;;;;;;;;; Exercício 3.2 ;;;;;;;;;;;;;;
;;Defina uma função que receba com entrada uma lista lst e um elemento a e devolva uma lista que é
;;como lst mas sem as ocorrências de a.
;;Lista Natural -> Lista
;;Devolve uma lista mas sem qualquer ocorrencia de n.

(define exercicio-3-2-tests
  (test-suite
   "Exercício 3.2"
   (check-equal? (remove-n empty 1) empty)
   (check-equal? (remove-n (list 5) 5) empty)
   (check-equal? (remove-n (list 2 5 3 5 7 5) 5) (list 2 3 7))))

(define (remove-n lst n)
  (cond
    [(empty? lst) empty]
    [(if(equal? (first lst) n)
        (remove-n (rest lst) n)
        (cons (first lst) (remove-n (rest lst) n)))]))
    

;;;;;;;;;;;; Exercício 3.7 ;;;;;;;;;;;;
;;Defina uma função que receba como entrada uma lista lst de 
;;número naturais e devolva uma lista que é como lst mas sem números pares
;;Lista -> Lista
;;Devolve uma lista, mas sem a ocorencia de numeros pares.

(define par-tests
  (test-suite
   "pares"
   (check-equal? (par? 7) #f)
   (check-equal? (par? 8) #t)))


(define exercicio-3-7-tests
  (test-suite
   "Exercício 3.7"
   (check-equal? (sem-pares-naturais empty) empty)
   (check-equal? (sem-pares-naturais (list 5)) (list 5))
   (check-equal? (sem-pares-naturais (list 2)) empty)
   (check-equal? (sem-pares-naturais (list 2 5 3 6 7 5)) (list 5 3 7 5))))

(define (par? x)
    (equal? (modulo x 2) 0))

(define (sem-pares-naturais lst)
  (cond
    [(empty? lst) empty]
    [(not(par? (first lst))) (cons (first lst) (sem-pares-naturais (rest lst))) ]
    [else (sem-pares-naturais (rest lst))]))

;;;;;;;;;;;; Exercício 3.8 ;;;;;;;;;;;;
;;Defina uma função que devolva o último elemento de uma lista.
;;Use a função error (com uma string de mensagem como argumento) para indicar erro se a lista for vazia.
;;Lista -> Numero
;;Devolve o ultimo elemento de uma lista ou mensagem de erro caso ela seja vazia.

(define exercicio-3-8-tests
  (test-suite
   "Exercício 3.8"
   (check-exn exn:fail? (thunk (ultimo-numero empty)))
   (check-equal? (ultimo-numero (list 5)) 5)
   (check-equal? (ultimo-numero (list 2 5 3 6 7)) 7)))

(define (ultimo-numero lst)
  (cond
    [(empty? lst) (error "Lista vazia")]
    [(empty? (rest lst)) (first lst)]
    [else (ultimo-numero (rest lst))]))


;;;;;;;;;;;; Exercício 3.11 ;;;;;;;;;;;;
;;Defina uma função que receba como entrada uma lista de números e devolva uma lista como os
;;mesmos valores de entrada mas em ordem crescente. (Lembre-se de aplicar a receita de projeto, não
;;tente implementar um método de ordenação qualquer, a receita te levará a implementar um método
;;específico). Dica: use a função insere-ordenado.
;; Lista -> Lista
;; Devolve uma nova lista com os mesmos elemento de lst, mas em ordem crescente.

(define (insere-ordenado n lst)
  (cond
    [(empty? lst) (list n)]
    [(n . < . (first lst)) (cons n lst)]
    [else (cons (first lst)
                (insere-ordenado n (rest lst)))]))

(define exercicio-3-11-tests
  (test-suite
   "Exercício 3.11"
   (check-equal? (ordena-crescente empty) empty)
   (check-equal? (ordena-crescente (list 2 5 3 6 7)) (list 2 3 5 6 7))
   (check-equal? (ordena-crescente (list 2 5 3 6 7 1 3)) (list 1 2 3 3 5 6 7))))


(define (ordena-crescente-lista lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [else (ordena-crescente-lista (rest lst1) (insere-ordenado (first lst1) lst2) )  ]))

(define (ordena-crescente lst)
  (ordena-crescente-lista lst empty))

          
;;;;;;;;;;;; Exercício 3.12 ;;;;;;;;;;;;
;;Defina uma função que receba como entrada uma lista lst e devolva uma nova lista que
;;é como lst com apenas uma ocorrência dos elementos repetidos consecutivos.
;;Lista -> Lista
;;Devolve uma lista sem a ocorrencia de elementos consecutivos duplicados.

(define exercicio-3-12-tests
  (test-suite
   "Exercício 3.12"
   (check-equal? (remove-duplicados empty) empty)
   (check-equal? (remove-duplicados (list 2 2 5 3 6 6 7)) (list 2 5 3 6 7))
   (check-equal? (remove-duplicados (list 2 2 5 5 5 5 3 3 6 6 7 7 1 1 3 3)) (list 2 5 3 6 7 1 3))))

(define (remove-duplicados lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) (cons (first lst) empty)]
    [else(if (equal? (first lst) (first (rest lst)))
             (remove-duplicados (rest lst))
             (cons (first lst) (remove-duplicados (rest lst))))]
    ))


;;;;;;;;;;;; Exercício 3.13 ;;;;;;;;;;;;
;;Defina uma função que receba como entrada uma lista aninhada lst e devolva uma nova lista aninhada
;;como os mesmo elementos de lst mas em ordem reversa.
;;Lista aninhada -> Lista aninhada
;;Devolve uma lista aninhada na ordem inversa a lista de entrada.

(define exercicio-3-13-tests
  (test-suite
   "Exercício 3.13"
   (check-equal? (reverter (list (list 2 3) 8 (list 9 (list 10 11) 50) (list 10) 70)) (list 70 (list 10) (list 50 (list 11 10) 9) 8 (list 3 2)) )
   (check-equal? (reverter empty) empty)
   (check-equal? (reverter (list 2 3 8)) (list 8 3 2))
   (check-equal? (reverter (list (list 8) 2 )) (list 2 (list 8)))
   ))

(define (reverter lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (append (reverter (rest lst)) (list(reverter(first lst)))) ]
    [else
     (append (reverter (rest lst)) (list(first lst)))]))


;;;;;;;;;;;; Exercício 3.15 ;;;;;;;;;;;;
;;Defina uma função que receba como entrada uma árvore binária t e um número n e devolva uma nova
;;árvore binária que é como t mas com n somado a cada elemento.
;;Árvore_binaria Natural -> Árvore_binaria
;;Devolve uma árvore binaria, mas com n somado a cada um dos elementos da árvore.

(struct arvore-bin (v esq dir) #:transparent)
(define t1 (arvore-bin 1 empty empty))
(define t2 (arvore-bin 2 t1 empty))
(define t3 (arvore-bin 3 t1 t2))
(define t4 (arvore-bin 4 t2 t3))

(define tt1 (arvore-bin 2 empty empty))
(define tt2 (arvore-bin 3 (arvore-bin 2 empty empty) empty))

(define exercicio-3-15-tests
  (test-suite
   "Exercício 3.15"
   (check-equal? (soma-n-arvore empty 0) empty)
   (check-equal? (soma-n-arvore t1 1) tt1)
   (check-equal? (soma-n-arvore t2 1) tt2)
   ))

(define (soma-n-arvore t n)
  (cond
    [(empty? t) empty]
    [else (arvore-bin
      (+ n (arvore-bin-v t))
      (soma-n-arvore (arvore-bin-esq t) n)
      (soma-n-arvore (arvore-bin-dir t) n))]))


;;;;;;;;;;;; Exercício 3.16 ;;;;;;;;;;;;
;;Defina uma função que verifique se uma árvore binária é uma árvore binária de busca.
;;Uma árvore binária de busca tem as seguintes propriedades: 
;; 1) A subárvore a esquerda contém valores nos nós menores que o valor no nó raiz.
;; 2) A subárvore a direita contém valores nos nós maiores que o valor no nó raiz.
;; 3) As subárvores a esquerda e a direita também são árvores binárias de busca.
;;Arvore_binária -> Boolean
;;Devolve verdadeiro se a árvore binária for uma árvore binária de busca ou falso caso contrario.

(define av1 (arvore-bin 1 empty empty))
(define av2 (arvore-bin 2 av1 empty))
(define av3 (arvore-bin 3 av1 av2))


(define exercicio-3-16-tests
  (test-suite
   "Exercicio 3.16"
   (check-equal? (arvore-busca? empty) #t)
   (check-equal? (arvore-busca? av1) #t)
   (check-equal? (arvore-busca? av2) #t)
   (check-equal? (arvore-busca? av3) #f)
   ))

(define (raiz-de-busca? t)
  (and
   (or (empty? (arvore-bin-esq t))
       (<
        (arvore-bin-v (arvore-bin-esq t))
        (arvore-bin-v t)))
   (or (empty? (arvore-bin-dir t))
       (>
        (arvore-bin-v (arvore-bin-dir t))
        (arvore-bin-v t)))
   ))

(define (arvore-busca? t)
  (cond
    [(empty? t) #t]
    [else (and(raiz-de-busca? t)
         (arvore-busca? (arvore-bin-dir t))
         (arvore-busca? (arvore-bin-esq t)))]))
   

;;;;;;;;;;;; Exercício 3.17 ;;;;;;;;;;;;
;;Defina uma função que verifique se um elemento está em uma árvore binária de busca.
;;Ávore_binária_busca Natural -> Boolean
;;Devolve verdadeiro se n está na árvore binária de busca.

(define exercicio-3-17-tests
  (test-suite
   "Exercicio 3.17"
   (check-equal? (is-in-tree? empty 1) #f)
   (check-equal? (is-in-tree? av1 2) #f)
   (check-equal? (is-in-tree? av2 1) #t)
   ))

(define (is-in-tree? t n)
  (cond
    [(empty? t) #f]
    [(equal? n (arvore-bin-v t)) #t]
    [(< n (arvore-bin-v t)) (is-in-tree? (arvore-bin-esq t) n) ]
    [else (is-in-tree? (arvore-bin-dir t) n)]
    ))


;;;;;;;;;;;; Executa tests ;;;;;;;;;;;;
;; Teste ... -> Void
;; Executa um conjunto de testes.

(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.

(executa-testes exercicio-3-1-test
                exercicio-3-2-tests
                par-tests
                exercicio-3-7-tests
                exercicio-3-8-tests
                exercicio-3-11-tests
                exercicio-3-12-tests
                exercicio-3-13-tests
                exercicio-3-15-tests
                exercicio-3-16-tests
                exercicio-3-17-tests
                )
