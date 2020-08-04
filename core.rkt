#lang racket

(provide (all-defined-out))

(module+ test
  (require rackunit))

;;; Environment
(struct env (map parent?)
  #:mutable
  #:transparent)
(define (env-new #:parent [p #f])
  (env (make-hash) p))
(define (lookup env v)
  (hash-ref (env-map env) v
            (λ ()
              (if (env-parent? env)
                  (lookup (env-parent? env) v)
                  (make-parameter v)))))
(define (extend env v e)
  (hash-set! (env-map env) v e))
(define (extend/append env v e)
  (hash-set! (env-map env) v
             (cons e
                   (hash-ref (env-map env) v '()))))

(define (occurs v t)
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    (t (equal? v t))))

(define (extract-p? p)
  (if (parameter? p) (p) p))

;;; return #t for success
(define (unify t1 t2 env)
  (match* (t1 t2)
    [(_ t2) #:when (and (parameter? t2)
                        (symbol? (t2)))
            (if (or (eqv? t1 (t2)) (not (occurs (t2) t1)))
                (begin (t2 (extract-p? t1))
                       #t)
                (error (format "~a occurs in ~a" (t2) (extract-p? t1))))]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1 env)]
    [(tm #t)
     (eq? #t (eval tm env))]
    [(#t _)
     (unify t2 t1 env)]
    [(`(or ,p* ...) _)
     (unify t2 t1 env)]
    [(`(,a* ...) `(,b* ...))
     (andmap (λ (a b) (unify a b env))
             a* b*)]
    [(_ _) (eqv? t1 (extract-p? t2))]))

(define (eval tm env)
  (match tm
    [`(def/rule ,name ,form)
     (extend/append env name form)]
    [`(? ,form)
     (unify #t form env)]
    [`(rule ,name ,form)
     (ormap (λ (rule)
              (define new-env (env-new #:parent env))
              (unify (eval rule new-env)
                     (map (λ (tm)
                            (eval tm env))
                          form)
                     new-env))
            (lookup env name))]
    [`(= ,a ,b)
     (let ([a (eval a env)]
           [b (eval b env)])
       (unless (unify a b env)
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
(define (run tm* [env (env-new)])
  (for-each (λ (tm)
              (displayln (pretty (eval tm env))))
            tm*))

(module+ test
  (test-case
   "unify x z"
   (define env (env-new))
   (run '((= x 'z))
        env)
   (check-equal? ((lookup env 'x)) 'z))
  (test-case
   "unify z x"
   (define env (env-new))
   (run '((= 'z x))
        env)
   (check-equal? ((lookup env 'x)) 'z))
  (test-case
   "unify (s z) (s x)"
   (define env (env-new))
   (run '((= '(s z) ('s x)))
        env)
   (check-equal? ((lookup env 'x)) 'z))
  (test-case
   "unify +"
   (define env (env-new))
   (run
    '((= (+ 'z n n)
         (+ 'z '(s z) r)))
    env)
   (check-equal? ((lookup env 'r)) '(s z))))
