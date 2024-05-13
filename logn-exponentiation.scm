(define (square x) (* x x))
(define (even? x) (= 0 (remainder x 2)))

(define (expt-iter a b pow)
    (cond 
        ((= pow 0) a)
        ((even? pow) (expt-iter a (square b) (/ pow 2)))
        (else (expt-iter (* a b) b (- pow 1)))
    )
)

(define (expt b pow) (expt-iter 1 b pow))

(define (mult-iter acc b n)
    (cond
        ((= n 0) acc)
        ((even? n) (mult-iter acc (+ b b) (/ n 2)))
        (else (mult-iter (+ acc b) b (- n 1)))
    )
)

(define (mult a n) (mult-iter 0 a n))