#lang racket

(provide (all-defined-out))

(module+ test
  (require rackunit))

;;; Environment
(define (lookup env v)
  (hash-ref env v (λ () (make-parameter v))))
(define (extend env v e)
  (hash-set! env v e))

(define (occurs v t)
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    (t (equal? v t))))

(define (extract-p? p)
  (if (parameter? p) (p) p))

;;; return #t for success
(define (unify t1 t2)
  (match* (t1 t2)
    [(_ t2) #:when (and (parameter? t2)
                        (symbol? (t2)))
            (if (or (eqv? t1 (t2)) (not (occurs (t2) t1)))
                (begin (t2 (extract-p? t1))
                       #t)
                (error (format "~a occurs in ~a" (t2) (extract-p? t1))))]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1)]
    [(a `(or ,p* ...))
     (let/cc return
       (for-each
        (λ (p)
          (if (unify a p)
              (return #t)
              (void)))
        p*))]
    [(`(or ,p* ...) _)
     (unify t2 t1)]
    [(`(,a* ...) `(,b* ...))
     (andmap unify a* b*)]
    [(_ _) (eqv? t1 (extract-p? t2))]))

(define (eval tm env)
  (match tm
    [`(= ,a ,b)
     (let ([a (eval a env)]
           [b (eval b env)])
       (unless (unify a b)
         (error (format "cannot unify ~a and ~a" (extract-p? a) (extract-p? b)))))]
    [`(quote ,x) x]
    [`(,a* ...)
     (map (λ (a) (eval a env)) a*)]
    [x (cond
         [(member x '(or + -)) x]
         [(symbol? x)
          (let ([v (lookup env x)])
            (extend env x v)
            v)]
         [else x])]))

(define (pretty tm)
  (match tm
    [`(,x* ...)
     (map pretty x*)]
    [x (extract-p? x)]))
(define (run tm* [env (make-hash)])
  (for-each (λ (tm)
              (displayln (pretty (eval tm env))))
            tm*))

(module+ test
  (test-case
   "unify x z"
   (define env (make-hash))
   (run '((= x 'z))
        env)
   (check-equal? ((lookup env 'x)) 'z))
  (test-case
   "unify z x"
   (define env (make-hash))
   (run '((= 'z x))
        env)
   (check-equal? ((lookup env 'x)) 'z))
  (test-case
   "unify (s z) (s x)"
   (define env (make-hash))
   (run '((= '(s z) ('s x)))
        env)
   (check-equal? ((lookup env 'x)) 'z))
  (test-case
   "unify +"
   (define env (make-hash))
   (run
    '((= (+ 'z n n)
         (+ 'z '(s z) r)))
    env)
   (check-equal? ((lookup env 'r)) '(s z))))
