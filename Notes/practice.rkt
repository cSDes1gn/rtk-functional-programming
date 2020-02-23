#lang racket

; Practice Exercises
; Exercise 1

(define (sum-coins p n d q)
    (+ p (* n 5) (* d 10) (* q 25)))

; Custom test case for practice
(and (if (= (sum-coins 1 0 0 0) 1) #t #f)
    (if (= (sum-coins 0 1 0 0) 5) #t #f)
    (if (= (sum-coins 0 0 0 1) 25) #t #f)
    (if (= (sum-coins 1 1 1 1) 41) #t #f))


; Exercise 2
(define (sa r h)
    (define pi 3.1415926)
    (define c (* (* 2 r) pi))
    (define (ac r)
        (* pi (* r r)))
    (+ (* 2 (ac r)) (* c h)))

(sa 2 3); result should be about 62.8
(sa 3 4) ; result should be about 131.9

; Exercise 3

(define (interest bal)
    (cond [(or (and ( > bal 0) (< bal 1000)) (= bal 1000)) (* 0.04 bal)]
        [(and (> bal 1000) (or (< bal 5000) (= bal 5000))) (* 0.045 bal)]
        [(> bal 5000) (* 0.05 bal)]
        [else (error "Input must be positive")]))

; Custom test case for practice
(and (if (= (interest 500) 20) #t #f)
    (if (= (interest 1000) 40) #t #f)
    (if (= (interest 2000) 90) #t #f)
    (if (= (interest 5000) 225) #t #f)
    (if (= (interest 10000) 500) #t #f))

; Exercise 4
(define (max x y z)
    (if (> x y)
    (if (> x z) x z)
    (if (> y z) y z)))

; Embedded λ procedures
(((λ (a) (λ (b) (* a b))) 4) 7)
; 1. Rtk resolves the inner λ which resolves in a no name procedure that takes in a single argument
(λ (b) (* a b))
; 2. That procedure becomes the body for the outter λ expression which resolves to a new expression
(λ (a) (λ (b) (* a b)))
; Note that this procedure when given 'a' resolves to λ (b) procedure with 'a' satisfied
; 3. This combination resolves to λ (b) with its 'a' parameter satisfied by 4
((λ (a) (λ (b) (* a b))) 4)
; 4. λ (b) now takes in 7 for its 'b' parameter and finishes resolving its body to 28
(((λ (a) (λ (b) (* a b))) 4) 7)