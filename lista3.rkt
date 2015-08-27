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

(define esta-na-lista-tests
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

(define remove-n-tests
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
    

;;;; Exercício 3.7 ;;;;;;
;;Defina uma função
;;que receba como entrada uma lista lst de número naturais e devolva uma lista
;que é como lst mas sem números pares

(define par-tests
  (test-suite
   "pares"
   (check-equal? (par? 7) #f)
   (check-equal? (par? 8) #t)))


(define pares-nataurais-tests
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


;;;;;;; Exercício 3.8 ;;;;;;
;Defina uma função que devolva o último elemento de uma lista.
;Use a função error (com uma string de mensagem como argumento) para indicar erro se a lista for vazia.

(define ultimo-numero-tests
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


;;;;;;; Exercício 3.11 ;;;;;;

(define (insere-ordenado n lst)
  (cond
    [(empty? lst) (list n)]
    [(n . < . (first lst)) (cons n lst)]
    [else (cons (first lst)
                (insere-ordenado n (rest lst)))]))

(define ordena-crescente-tests
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

          

;;;;;;; Exercício 3.12 ;;;;;;

(define remove-duplicados-tests
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

;;;;;;; Exercício 3.13 ;;;;;;

(define reverter-tests
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


;;;;;;; Exercício 3.15 ;;;;;;
(struct arvore-bin (v esq dir) #:transparent)
(define t1 (arvore-bin 1 empty empty))
(define t2 (arvore-bin 2 t1 empty))
(define t3 (arvore-bin 3 t1 t2))
(define t4 (arvore-bin 4 t2 t3))

(define tt1 (arvore-bin 2 empty empty))
(define tt2 (arvore-bin 3 (arvore-bin 2 empty empty) empty))



(define soma-n-arvore-tests
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



;;;;;;; Exercício 3.16 ;;;;;;

(define av1 (arvore-bin 1 empty empty))
(define av2 (arvore-bin 2 av1 empty))
(define av3 (arvore-bin 3 av1 av2))


(define arvore-busca-tests
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
   


;;;;;;; Exercício 3.17 ;;;;;;

(define esta-na-arvore-tests
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




;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes esta-na-lista-tests
                remove-n-tests
                par-tests
                pares-nataurais-tests
                ultimo-numero-tests
                ordena-crescente-tests
                remove-duplicados-tests
                reverter-tests
                soma-n-arvore-tests
                arvore-busca-tests
                esta-na-arvore-tests
                )
     