(define (iter-improve improve-guess close-enough?)
  (define (iter guess)
    (let ((next (improve-guess guess)))
      (if (close-enough? guess next)
        next
        (iter next)
      )
    )
  )
  (lambda (init) (iter init))
)

(define dx 0.001)

(define (deriv f x)
  (/ (- (f (+ x dx)) (f x)) dx)
)

; Procedure to get a new guess from a function with Newton's method
; f is a function of one variable
; :: (a -> b) -> (a -> b)
(define (newton f) 
  (lambda (x)
    (- x (/ (f x) (deriv f x)))
  )
)

(define tolerance 0.001)
(define (within-tol? x y)
  (< (abs (- x y)) tolerance) ; Good enough
)

(define (sqrt x)
  (
    (iter-improve 
      (newton (lambda (y) (- (square y) x))) ; Next guess
      within-tol?
    )
    2
  )
)

(define (fixed-point f init)
  (
    (iter-improve
      f
      within-tol?
    )
    init
  )
)