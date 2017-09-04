(define (divides a b) (= (modulo b a) 0))
(define (even n) (divides 2 n))
(define (odd n) (not (even n)))

(define (sum xs) (fold-left + 0 xs))

(define (fib n) (fibc 0 1 n))

(define (fibc a b n)
  (if (< a n)
    (cons a (fibc b (+ a b) n))
    '()))

(define (factor n)
  (if (= n 1)
    '()
    (let ((d (next-factor n 2)))
      (cons d (factor (/ n d))))))

(define (next-factor n d)
  (if (divides d n)
    d
    (next-factor n (+ d 1))))

