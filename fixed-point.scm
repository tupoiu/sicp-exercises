
(define (average x y) (/ (+ x y) 2))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (fixed-point f init)
  (let ((next-pt (f init)))
    (if (close-enough? next-pt init)
      next-pt
      (begin
        (display next-pt)
        (newline)
        (fixed-point f (average init next-pt))
      )
    )
  )
)