#lang racket

; Exercise 1:
(λ (x y z) (x y z)) ; generates a valid procedure while x is a procedure.
(λ () 10)           ; valid procedure with null arity
(λ (x) x)           ; Returns param
(λ (x y) x)         ; returns first argument

((λ (x y z) (x y z)) + 1 2) ; x is a procedure so this is valid
(+ ((λ () 10)) 10)          ; λ takes 0 parameters so its wrapped in an empty combination bracket
(((λ (x) x) *) 5 5)         ; λ outputs whatever value it was passed
(((λ (x y) x) / *) 5 5)     ; λ returns first of 2 passed values

; Exercise 2
((λ (x y) (+ x (* x y))) 1 2)   ; Evaluates to 3
((λ (x y) (+ x (
    (λ (z) (+ (* 3 z) (/ 1 z))) (* y y)))) 1 2) ; evaluates to 53/4

; Exercise 3
(define (square x) (* x x))
; > square      ;resolves to #<procedure:square> 
; > (square 5)  ;resolves to 25
(define sq (λ (x) (* x x)))
; > sq          ;resolves to #<procedure:sq> 
; > (sq 5)      ;resolves to 25
; Therefore these are functionally equivalent procedure definitions
(define (make-adder n) (λ (x) (+ x n)))
; > make-adder          ; resolves to #<procedure:make-adder>
; > (make-adder 3)      ; x undefined
; > ((make-adder 3) 7)  ; x undefined
; This procedure definition can't execute because λ requires 2 parameters but only a single is passed
(define plus3 (make-adder 3))
; > plus3               ; same error as above since x is undefined
; > (plus3 7)           ; same error as above since x is undefined

; Exercise 4: HARD: Construct a list builder which applies a function f to each element
(define (build-list n f)
    (define (iter lst index n)
        (if (= n 0) lst
            (iter (append lst (list (f index))) (+ index 1) (- n 1))))
    (iter null 0 n))

(define (build-naturals n)
    (build-list n (λ (x) 
        (cond [(= x 0) 0]
            [else x]))))

(define (build-rationals n)
    (build-list n (λ (x)
        (cond [(= x 0) 1]
            [else (/ 1 (+ x 1))]))))

(define (build-evens n)
    (build-list n (λ (x) (* 2 x))))

(build-naturals 5)
(build-rationals 8)
(build-evens 7)

; Exercise 5: Cubic higher order procedure definition
(define (cubic a b c)
    (λ (x)
        (+ (* x x x) (* (* x x) a) (* x b) c)))

((cubic 1 2 3) 4)

; Exercise 6: Procedures passed as an argument
; Recall that λ specifies an input parameter x which the input function f uses
(define (twice f)
    (λ (x) (f (f x))))

;(define (square x) (* x x))
((twice square) 5)          ; 5^2^2
(define (inc x) (+ x 1))
((twice inc) 5)             ; (5+1)+1