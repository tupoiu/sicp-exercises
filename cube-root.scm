(define (avg x y) (/ (+ x y) 2))

(define (abs x) (if (< 0 x) x (- x)))

(define (good-enough? x v) 
    (< (abs (- x v)) 0.001))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (cbrt-iter guess x)
  (if (good-enough? (cube guess) x)
    guess
    (cbrt-iter (/ (+ (* 2 guess) (/ x (square guess))) 3) x)))

(define (cbrt x) (cbrt-iter 1.0 x))



(display (cbrt 1))