#lang typed/racket

(all-defined-out)

(module+ test
  (require typed/rackunit))

(struct Stmt () #:transparent)
; (= x 1) or (= 1 x) or (= 1 1)
(struct Stmt= Stmt
  ([e : Exp]
   [e2 : Exp])
  #:transparent)
; (: x integer)
(struct Stmt: Stmt
  ([e : Exp]
   [t : Typ])
  #:transparent)

(struct Typ () #:transparent)
(struct Exp () #:transparent)
(struct Val Exp () #:transparent)
(struct Val:int Val
  ([v : Integer])
  #:transparent)
(struct Val:free Val () #:transparent)
(struct Exp:var Exp
  ([name : String])
  #:transparent)
(struct Exp:+ Exp
  ([left : Exp]
   [right : Exp])
  #:transparent)

(define (new-var? [ctx : (Mutable-HashTable String Val)]
                  [name : String]
                  [v : Val])
  : Void
  (if (hash-has-key? ctx name)
      (void)
      (hash-set! ctx name v)))
(define (lookup/val [ctx : (Mutable-HashTable String Val)]
                    [name : String])
  : Val
  (hash-ref ctx name))
(define (Val-equal [v1 : Val]
                   [v2 : Val])
  : Boolean
  (match* (v1 v2)
    [((Val:int i) (Val:int j))
     (= i j)]
    [(_ _) #f]))

(define (unify [ctx : (Mutable-HashTable String Val)]
               [e1 : Exp]
               [e2 : Exp])
  : Boolean
  (match* (e1 e2)
    [((Exp:var name) e)
     (new-var? ctx name (Val:free))
     (Val-equal (eval ctx e1) (eval ctx e2))]
    [(e (Exp:var name))
     (new-var? ctx name (Val:free))
     (Val-equal (eval ctx e1) (eval ctx e2))]
    [(e1 e2) (Val-equal (eval ctx e1) (eval ctx e2))]))
(define (eval [ctx : (Mutable-HashTable String Val)]
              [e : Exp])
  : Val
  (match e
    [(Exp:+ l r) (Val:int (+ (eval-int ctx l) (eval-int ctx r)))]
    [(Exp:var name) (lookup/val ctx name)]
    [(Val:int i) (Val:int i)]))
(define (eval-int [ctx : (Mutable-HashTable String Val)]
                  [e : Exp])
  : Integer
  (match (eval ctx e)
    [(Val:int i) i]
    [_ (error 'type-mismatched "expected integer, but got: ~a" e)]))

(module+ test
  (test-case
   "unify x 1"
   (define ctx : (Mutable-HashTable String Val) (make-hash '()))
   (unify ctx (Val:int 1) (Exp:var "x"))
   (check-equal? (lookup/val ctx "x")
                 (Val:int 1)))

  )
