#lang racket

(module+ test
  (require rackunit))

;;; Environment
(define (lookup env v)
  (hash-ref env v))
(define (extend env v e)
  (hash-set! env v e))

(define (occurs v t)
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    (t (equal? v t))))

(define (unify t1 t2)
  (match* (t1 t2)
    [(_ t2) #:when (parameter? t2)
            (if (or (eqv? t1 (t2)) (not (occurs (t2) t1)))
                (t2 t1)
                (error (format "~a occurs in ~a" (t2) t1)))]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1)]
    [(`(,a* ...) `(,b* ...))
     (for-each unify a* b*)]
    [(_ _)
     (unless (eqv? t1 t2)
       (error (format "cannot unify ~a and ~a" t1 t2)))]))

(define (eval tm env)
  (match tm
    [`(= ,a ,b)
     (unify (eval a env) (eval b env))]
    [`(,a* ...)
     (map (λ (a) (eval a env)) a*)]
    [x (cond
         [(symbol? x)
          (let* ([v (if (hash-has-key? env x)
                        (lookup env x)
                        (make-parameter x))])
            (extend env x v)
            v)]
         [else x])]))

(define (run tm* [env (make-hash)])
  (for-each (λ (tm) (eval tm env))
            tm*))

(module+ test
  (test-case
   "unify x 1"
   (define env (make-hash))
   (run '((= x 1))
        env)
   (check-equal? ((lookup env 'x)) 1))
  (test-case
   "unify 1 x"
   (define env (make-hash))
   (run '((= 1 x))
        env)
   (check-equal? ((lookup env 'x)) 1))
  (test-case
   "unify (add1 1) (add1 x)"
   (define env (make-hash))
   (run '((= (add1 1) (add1 x)))
        env)
   (check-equal? ((lookup env 'x)) 1)))