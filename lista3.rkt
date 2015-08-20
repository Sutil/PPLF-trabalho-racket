#lang racket

(require rackunit)
(require rackunit/text-ui)

;#
(define exemplo-tests
  (test-suite
   "exemp tests"
   (check-equal? (cons-fim 3 empty) (list 3))
   (check-equal? (cons-fim 3 (list 5)) (list 5 3))
   (check-equal? (cons-fim 8 (list 2 5)) (list 2 5 8))))


;;;;;;;;;;;;;;;; Exercício 3.1 ;;;;;;;;;

(define esta-na-lista-tests
  (test-suite
   "está na lista?"
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

(define remove-n-tests
  (test-suite
   "remove n da lista"
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









;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes esta-na-lista-tests
                remove-n-tests)
     