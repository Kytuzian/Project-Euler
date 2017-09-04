(load "lib.scm")

(define (answer)
  (let ((nums (flatten (map (lambda (a) (map (lambda (b) (* a b)) (iota 899 100))) (iota 899 100)))))
    (apply max (map undigits (filter is-palindrome (map digits nums))))))
