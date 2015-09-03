#lang racket
(require rackunit)
(require rackunit/text-ui)



;;;;;; Exercício 7.1 -> 3.2 ;;;;;

(define remove-n-tests
  (test-suite
   "Exercício 3.2"
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


;;;;;; Exercício 7.1 -> 3.7 ;;;;;

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
  (define (iter lst0 acc)
    (cond
      [(empty? lst0) acc]
      [(par? (first lst0)) (iter (rest lst0) acc)]
      [else (iter (rest lst0) (append acc (list (first lst0))))]))
  (iter lst empty))

;;;;;; Exercício 7.1 -> 3.9 ;;;;;


;;;;;; Exercício 7.1 -> 4.1 ;;;;;

;;;;;; Exercício 7.1 -> 4.5 ;;;;;



;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes remove-n-tests
                pares-nataurais-tests
                )