(load "integration.scm")

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
    id
    (compose f (repeated f (- n 1)))
  )
)

(define dx 0.001)
(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3))
)

(define (iter-smooth f n) ((repeated smooth n) f))