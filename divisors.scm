(define (square x) (* x x))

;; Sift up to sqrt n
(define (smallest-divisor n)
  (define (find-divisor-gt k)
    (cond 
      ((> (square k) n) n)
      ((divides? k n) k)
      (else (find-divisor-gt (+ 1 k)))))
  (find-divisor-gt 2)      
)

(define (gcd x d)
  (cond 
    ((= d 0) x)
    ((> d x) (gcd d x))
    (else (gcd d (remainder x d)))
  )
)

(define (divides? k n) (= 0 (remainder n k)))

(define (prime? n) (= (smallest-divisor n) n))

(define (find-primes-between lb ub)
    (cond
        ((> lb ub) (display "Done"))
        ((prime? lb) 
            ; (display lb)
            ; (display ", ")
            (find-primes-between (+ 1 lb) ub)
        )
        (else (find-primes-between (+ 1 lb) ub))
    )
)


(define (timed-prime-test n)
    (time (find-primes-between 1 n))
)