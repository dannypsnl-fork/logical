#lang typed/racket

(module+ test
  (require typed/rackunit))

(struct Typ () #:transparent)
(struct Val () #:transparent)
(struct Val:var Val
  ([name : String]
   [typ : Typ]
   [v : Val])
  #:transparent)
(struct Val:int Val
  ([v : Integer])
  #:transparent)
(define-type Exp
  (U Exp:binary Val))
(struct Exp:binary
  ([left : Exp]
   [right : Exp])
  #:transparent)

(module+ test
  (check-equal? (+ 2 2) 4))
