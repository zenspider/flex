#lang racket

;; MIT Scheme -> Racket

(define get-arity procedure-arity)
(define list-head take)
(define list-tail drop)

(module+ test
  (require rackunit)
  (require check-sexp-equal)
  )

;;; 2.1 Combinators

;; 2.1.1 Function Combinators

(define (compose f g)
  (λ args
    (f (apply g args))))

(module+ test
  (define-simple-check (check-compose fn)
    (define act ((fn (λ (x) (list 'foo x))
                     (λ (x) (list 'bar x))) 'z))
    (check-equal? '(foo (bar z)) act))

  (check-compose compose))

(define (compose/2 f g)
  (define (the-composition . args)      ; doesn't seem to help in racket
    (f (apply g args)))
  the-composition)

(module+ test
  (check-compose compose/2))

(define ((iterate n) f)
  (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))

(module+ test
  (check-equal? 390625 (((iterate 3) (λ (x) (* x x))) 5)))

(define (parallel-combine h f g)
  (λ args
    (h (apply f args)
       (apply g args))))

(module+ test
  (check-sexp-equal? '((foo a b c) (bar a b c))
                     ((parallel-combine list
                                        (λ (x y z) (list 'foo x y z))
                                        (λ (u v w) (list 'bar u v w)))
                      'a 'b 'c)))

;;;; Arity

(define (spread-combine/1 h f g)
  (let ((n (get-arity f)))
    (λ args
      (h (apply f (list-head args n))
         (apply g (list-tail args n))))))

(module+ test
  (define-simple-check (check-spread-combine fn)
    (check-equal? '((foo a b) (bar c d e))
                  ((fn list
                     (λ (x y) (list 'foo x y))
                     (λ (u v w) (list 'bar u v w)))
                   'a 'b 'c 'd 'e)))

  (check-spread-combine spread-combine/1))

(define arity-table (make-hasheq))

(define (restrict-arity proc nargs)
  (hash-set! arity-table proc nargs))

(define (spread-combine/2 h f g)
  (let ([n (get-arity f)]
        [m (get-arity g)])
    (let ([t (+ n m)])
      (define (the-combination . args)
        (unless (= (length args) t)
          (error 'spread-combine/2 "arity ~s expected ~s" (length args) t))
        (h (apply f (list-head args n))
           (apply g (list-tail args n))))
      (restrict-arity the-combination t)
      the-combination)))

(module+ test
  (check-spread-combine spread-combine/2))

;;; done

(module+ test
  (displayln 'done))
