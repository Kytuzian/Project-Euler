(load "lib.scm")

(define (answer limit) (sum (filter even (fib limit))))
