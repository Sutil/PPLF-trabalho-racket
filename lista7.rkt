#lang racket
(require rackunit)
(require rackunit/text-ui)



;;;;;; Exercício 7.1 -> 3.2 ;;;;;

(define remove-n-tests
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


;;;;;; Exercício 7.1 -> 3.7 ;;;;;

(define pares-nataurais-tests
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

;;;;;; Exercício 7.1 -> 3.9 ;;;;;

(define maximo-tests
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
      

;;;;;; Exercício 7.2 -> 4.1 ;;;;;

(define fatorial-tests
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

;;;;;; Exercício 7.2 -> 4.5 ;;;;;

(define quant-primos-tests
  (test-suite
   "Exercício 7.2 -> 4.5"
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
    (define (iter i f acc)
      (cond
        [(> i f) acc]
        [(primo? i) (iter (add1 i) f (add1 acc))]
        [else (iter (add1 i) f acc)]))
  (iter i f 0))
      

;;;;;;;; Executa tests ;;;;;;;;;;;;;

(define (executa-testes . testes)
  (run-tests (test-suite "Execução tests" testes))
  (void))

(executa-testes remove-n-tests
                pares-nataurais-tests
                maximo-tests
                fatorial-tests
                quant-primos-tests
                )