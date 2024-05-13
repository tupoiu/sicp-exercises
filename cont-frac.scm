(define (cont-frac n d k)
  (define (helper i) 
    (cond 
        ((= i k) 0)
        (else (/ (n k) (+ (d k) (cont-frac n d (- k 1)))))  
    )
  )
  (helper 0)
)

(define (cont-frac2 n d k)
  (define (iter total i)
    (if (= i 0)
      total
      (iter (/ (n i) (+ (d i) total)) (- i 1))
    )
  )
  (iter 0 k)
)

(define (e-approx) 
  (define (d i)
    (cond 
      ((= 2 (mod i 3)) (expt 2 (/ (+ i 1) 3)))
      (else 1)
    )
  )
  (+ 2 (cont-frac2 (lambda (i) 1.0) d 100))
)