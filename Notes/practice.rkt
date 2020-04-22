#!/usr/bin/racket
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
; (λ (b) (* a b))
; 2. That procedure becomes the body for the outter λ expression which resolves to a new expression
(λ (a) (λ (b) (* a b)))
; Note that this procedure when given 'a' resolves to λ (b) procedure with 'a' satisfied
; 3. This combination resolves to λ (b) with its 'a' parameter satisfied by 4
((λ (a) (λ (b) (* a b))) 4)
; 4. λ (b) now takes in 7 for its 'b' parameter and finishes resolving its body to 28
(((λ (a) (λ (b) (* a b))) 4) 7)

; Practice Midterm Question 5:
; Define a recursive procedure named ​multiply-all​ that takes a list of numbers. 
; This procedure returns the product of the numbers in the list, by means of a ​recursive process​.
; > (multiply-all '(2 3 4))       ; returns 24
; > (multiply-all '(3))           ; returns 3
; > (multiply-all '())            ; returns 1

(define (multiply-all lst)
    (if (null? lst) 1
        (* (multiply-all (cdr lst)) (car lst))))

(multiply-all '(2 3 4))
(multiply-all '(3))
(multiply-all '())

; Define a function factorial that uses build-list and multiply-all to resolve factorial of value n
(define (factorial n)
    (multiply-all (build-list n (λ (x) (+ x 1)))))

(factorial 1)
(factorial 3)
(factorial 40)  ; Impressive how fast this can resolve

; Practice Midterm Question 6:
; Consider a function g : x → g(x). ​g​(​x)​ is the value of ​g​, evaluated at ​x​. The derivative of ​g​ is a
; function, ​Dg​. ​Dg​(​x)​ is the value of the derivative of ​g,​ evaluated at ​x.​ For example, the derivative
; of the function g : x → x3 is the function Dg : x → 3x2. The value of the derivative at 5, ​Dg(​ 5), 
; is 75.Define a procedure named ​deriv​ that takes a procedure, ​g​, as an argument. ​deriv​ returns a 
; procedure that takes one argument, ​x​. That procedure calculates and returns the derivative of ​g​, 
; evaluated at ​x​. To approximate the derivative of g : x → x3 at 5, we evaluate:
; > (define dx 0.00001)
; > (define (cube x) (* x x x))
; > ((deriv cube) 5)
; 75.00014999664018

(define (deriv f)
    (λ (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (cube x) (* x x x))
(define dx 0.00001)
((deriv cube) 5)