#lang racket
(provide 
 relative relative? abs sign
 successor eq? less? greater?
 plus minus mult div
 (struct-out divresult)
 )

(require (prefix-in n: "natural.rkt"))

(struct relative (sign abs) #:transparent)
(struct divresult (quotient remainder) #:transparent)

(define zero (relative '+ 0))
(define one (relative '+ 1))

(define (abs x)
  (relative-abs x)
  )

(define (sign x)
  (relative (relative-sign x) 1)
  )

(define (with-sign sign x)
  (relative sign x)
  )

(define (cast x)
  (with-sign '+ x)
  )

(define (pos? x)
  (equal? (relative-sign x) '+)
  )

(define (neg? x)
  (equal? (relative-sign x) '-)
  )

(define (same-sign x y)
  (equal? (relative-sign x) (relative-sign y))
  )

(define (mult-sign x y)
  (if (same-sign x y) '+ '-)
  )

(define (opposite x)
  (if
   (n:natural? x) (with-sign '- x)
   (relative (if (pos? x) '- '+) (relative-abs x))
   )
  )

(define (successor x)
  (plus x one)
  )

(define (mixop nimpl impl)
  (λ (x y)
    (cond
      ((and (n:natural? x) (n:natural? y)) (nimpl x y))
      ((n:natural? x) (impl (cast x) y))
      ((n:natural? y) (impl x (cast y)))
      (else (impl x y))
      )
    )
  )

(define (eq? x y)
  (define (impl x y)
    (if (same-sign x y) (eq? (abs x) (abs y)) #f)
    )
  ((mixop n:eq? impl) x y)
  )

(define (less? x y)
  (cond
    ((and (pos? x) (pos? y)) (n:less? (abs x) (abs y)))
    ((pos? x) #f)
    ((and (neg? x) (neg? y)) (n:less? (abs y) (abs x)))
    (else #t)
    )
  )

(define (greater? x y)
  (less? y x)
  )

(define (plus x y)
  (define (impl x y)
    (cond
      ((neg? x) (opposite (impl (opposite x) (opposite y))))
      ((pos? y) (cast (n:plus (abs x) (abs y))))
      ((n:greater? (abs x) (abs y))
       (cast (n:minus (abs x) (abs y)))
       )
      (else (with-sign '- (n:minus (abs y) (abs x))))
      )
    )
  ((mixop n:plus impl) x y)
  )

(define (minus x y)
  (define (impl x y)
    (plus x (opposite y))
    )
  ((mixop n:minus impl) x y)
  )

(define (mult x y)
  (define (impl x y)
    (with-sign (mult-sign x y) (n:mult (abs x) (abs y)))
    )
  ((mixop n:mult impl) x y)
  )

(define (div x y)
  (define (div-sign result qsign rsign)
    (divresult
     (with-sign qsign (n:divresult-quotient result))
     (with-sign rsign (n:divresult-remainder result))
     )
    )
  (define (impl x y)
    (div-sign
     (n:div (abs x) (abs y))
     (mult-sign x y)
     (relative-sign x)
     )
    )
  ((mixop n:div impl) x y)
  )
