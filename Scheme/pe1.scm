(load "lib.scm")

(define (answer start end)
  (sum
    (filter
      (lambda (x) (or (divides 3 x) (divides 5 x)))
      (iota (- end 1) start))))

