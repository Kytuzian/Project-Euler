(load "lib.scm")

(define (answer n)
  (- (square (sum (iota n 1)))
     (sum (map square (iota n 1)))))
