(load "divisors.scm")

(define (make-rat num denom)
  (define g (gcd num denom))
  (cons (/ num g) (/ denom g))
)

(define num car)
(define denom cdr)

(define (add-rat x y)
  (make-rat 
    (+ (* (num x) (denom y)) (* (num y) (denom x)))
    (* (denom x) (denom y))
  )
)

(define (mul-rat x y)
  (make-rat (* (num x) (num y)) (* (denom x) (denom y)))
)

(define (sub-rat x y)
  (add-rat x (neg-rat y))
)

(define (neg-rat x) 
  (make-rat (- (num x)) (denom x))
)