
(define (sum f a next b)
  (define (sum-iter a total)
    (if (> a b)
      total
      (sum-iter (next a) (+ total (f a)))
    )  
  )
  (sum-iter a 0)
)

(define (product f a next b)
  (if (> a b)
    1
    (* (f a) (product f (next a) next b))
  )
)

(load "divisors.scm")
(define (filtered-fold binop pred init f a next b)
  (define (iter a total)
    (if (> a b)
      total
      (iter (next a) 
        (if (pred a)
          (binop (f a) total) ;; Filter to only those for which pred holds
          total
        )
      )
    )
  )
  (iter a init)
)

(define (id x) x)
(define (inc x) (+ x 1))
(define (cube x) (* x x x))

;; Simpson's rule
(define (simp-integral f a b n)
  (define h (/ (- b a) n))
  (define (next-slice x) (+ x h))
  (define (f2 k) 
    (cond
      ((= k 0) (f a))
      ((= k n) (f b))
      ((divides? 2 k) (* 2 (f (+ a (* k h)))))
      (else (* 4 (f (+ a (* k h)))))  
    ))
  (* (/ h 3) (sum f2 0 inc n))
)