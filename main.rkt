#lang racket

(require "core.rkt")

(run
 '((def/rule + ('z n n))
   (def/rule + (('s m) n
                       ('s (rule + m n))))
   (? (rule + ('z 'z r2)))
   r2
   (? (rule + (('s 'z) 'z r)))
   r))
