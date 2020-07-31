#lang racket

(require "core.rkt")

(run
 '((= (or (+ 'z n n)
          (+ ('s m) n
             ('s (+ m n))))
      (+ '(s z) 'z r))
   r))
