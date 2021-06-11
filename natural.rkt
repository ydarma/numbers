#lang racket
(define (non-negative-integer? x) ((and integer? (or positive? zero?)) x))
(provide
 successor eq? less? greater?
 (contract-out 
  [struct natural((val non-negative-integer?))]
  [struct divresult((quotient natural?)(remainder natural?))]
  [plus (-> natural? natural? natural?)] 
  [minus (-> natural? natural? natural?)] 
  [mult (-> natural? natural? natural?)] 
  [div (-> natural? natural? divresult?)]
  )
 )

(struct natural (val) #:transparent)

(struct divresult (quotient remainder) #:transparent)

(define (successor x)
  (natural (add1 (natural-val x)))
  )

(define (eq? x y)
  (equal? x y)
  )

(define (plus x y)
  (define (f x y i)
    (if (eq? y i) x
        (f (successor x) y (successor i))
        )
    )
  (f x y (natural 0))
  )

(define (less? x y)
  (define (f x y r)
    (cond 
      ((eq? y r) #f)
      ((eq? x r) #t)
      (else (f x y (successor r)))
      )
    )
  (f x y (natural 0))
  )

(define (greater? x y)
  (less? y x)
  )

(define (minus x y)
  (define (f x y r)
    (cond 
      ((eq? x r) (error "subtraction by greater natural"))
      ((eq? x y) r)
      (else (f x (successor y) (successor r)))
      )
    )
  (f x y (natural 0))
  )

(define (mult x y)
  (define (f x y s i)
    (if (eq? y i) s
        (f x y (plus s x) (successor i))
        )
    )
  (f x y (natural 0) (natural 0))
  )

(define (div x y)
  (define (f x y r s t)
    (cond ((eq? x t) (divresult s r))
          ((eq? y (successor r)) (f x y (natural 0) (successor s) (successor t)))
          (else (f x y (successor r) s (successor t)))
          )
    )
  (if (eq? y (natural 0)) (error "division by zero")
      (f x y (natural 0) (natural 0) (natural 0))
      )
  )