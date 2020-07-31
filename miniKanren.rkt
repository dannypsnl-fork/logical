#lang racket

(define (var name) (vector name))
(define (var? name) (vector? name))
(define empty-s '())
(define (ext-s x v s)
  (cond
    [(occurs? x v s) #f]
    [else (cons `(,x . ,v) s)]))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eqv? v x)]
      [(pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s))]
      [else #f])))
(define (walk v s)
  (let ([a (and (var? v) (assv v s))])
    (cond
      [(pair? a) (walk (cdr a) s)]
      [else v])))
(define (unify u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(eqv? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? v) (pair? u))
       (let ([s (unify (car u) (car v) s)])
         (and s
              (unify (cdr u) (cdr v) s)))]
      [else #f])))
(define (≡ u v)
  (λ (s)
    (let ([s (unify u v s)])
      (if s `(,s) '()))))
(define (succ s)
  `(,s))
(define (unsucc s)
  '())
(define (disj2 g1 g2)
  (λ (s)
    (append∞ (g1 s) (g2 s))))
(define (append∞ s∞ t∞)
  (cond
    [(null? s∞) t∞]
    [(pair? s∞)
     (cons (car s∞)
           (append∞ (cdr s∞) t∞))]
    [else (λ ()
            (append∞ t∞ (s∞)))]))
(define (never-o)
  (λ (s)
    (λ () ((never-o) s))))
(define (always-o)
  (λ (s)
    (λ ()
      (disj2 succ (always-o)) s)))
(define (take∞ n s∞)
  (cond
    [(and n (zero? n)) '()]
    [(null? s∞) '()]
    [(pair? s∞)
     (cons (car s∞)
           (take∞ (and n (sub1 n))
                  (cdr s∞)))]
    [else (take∞ n (s∞))]))
(define (conj2 g1 g2)
  (λ (s)
    (append-map∞ g2 (g1 s))))
(define (append-map∞ g s∞)
  (cond
    [(null? s∞) '()]
    [(pair? s∞)
     (append∞ (g (car s∞))
              (append-map∞ g (cdr s∞)))]
    [else (λ ()
            (append-map∞ g (s∞)))]))
(define (call/fresh name f)
  (f (var name)))
(define (reify-name n)
  (string->symbol
   (string-append "_"
                  (number->string n))))
(define (walk* v s)
  (let [(v (walk v s))]
    (cond
      [(var? v) v]
      [(pair? v)
       (cons (walk* (car v s))
             (walk* (cdr v s)))]
      [else v])))
(define (reify-s v r)
  (let ([v (walk v r)])
    (cond
      [(var? v)
       (let ([n (length r)])
         (let ([rn (reify-name n)])
           (cons `(,v . ,rn) r)))]
      [(pair? v)
       (let ([r (reify-s (car v) r)])
         (reify-s (cdr v) r))]
      [else r])))
(define (reify v)
  (λ (s)
    (let ([v (walk* v s)])
      (let ([r (reify-s v empty-s)])
        (walk* v r)))))
(define (run-goal n g)
  (take∞ n (g empty-s)))
(define (ifte g1 g2 g3)
  (λ (s)
    (let loop ((s∞ (g1 s)))
      (cond
        [(null? s∞) (g3 s)]
        [(pair? s∞)
         (append-map∞ g2 s∞)]
        [else (λ () (loop (s∞)))]))))
(define (once g)
  (λ (s)
    (let loop ((s∞ (g s)))
      (cond
        [(null? s∞) '()]
        [(pair? s∞)
         (cons (car s∞) '())]
        [else (λ () (loop (s∞)))]))))
