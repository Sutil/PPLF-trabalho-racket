#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;; Exercício 6.8 ;;;

(define cont-tests
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

;;;; Exercício 6.10 ;;;

(define concatena-tests
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
 

;;;; Exercício 6.11 ;;;

#;(define mapeia-tests
  (test-suite
   "Exercício 6.11"
   (check-equal? (mapeia + (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 12 15 18))
   ;(check-equal? (concatena (list 3) (list 4)) (list 3 4))
   ;(check-equal? (concatena (list 1 2)) (list 1 2))
   ))

;;;; Exercício 6.12 ;;;;

(define paridade-tests
  (test-suite
   "Exercício 6.10"
   (check-equal? (paridade 1 2 3) (list 1 3))
   (check-equal? (paridade 4) (list 4))
   (check-equal? (paridade 2 3 4 5 8 12) (list 2 4 8 12))
   ))

(define (paridade n . ns)
  (cond
    [(even? n) (cons n (filter even? ns))]
    [else (cons n (filter odd? ns))]))


;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes cont-tests
                concatena-tests
                paridade-tests
                )