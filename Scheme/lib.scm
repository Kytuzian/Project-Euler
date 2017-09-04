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

(define (reverse ls)
  (if (null? ls)
    '()
    (append (reverse (cdr ls)) (list (car ls)))))

(define (is-palindrome ls) (equal? ls (reverse ls)))

(define (digits n) (reverse (rev-digits n)))
(define (rev-digits n)
  (if (< n 10)
    (list n)
    (cons (modulo n 10) (rev-digits (quotient n 10)))))

(define (undigits ds)
  (fold-left (lambda (a b) (+ (* 10 a) b)) 0 ds))

(define (max-by f vals) (max-by-c (cdr vals) (car vals)))
(define (max-by-c xs v)
  (if (null? xs)
    v
    (if (> (f (car xs)) (f v))
      (max-by-c (cdr xs) (car xs))
      (max-by-c (cdr xs) v))))

(define (flatten ls)
  (fold-left append '() ls))

