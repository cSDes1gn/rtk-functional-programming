#!/usr/bin/racket
#lang racket

; Notes for midterm 1
; -------------------
; Compound procedures:
; (define (<name> <formal parameters>) <body>)
; Nameless 'lambda' procedures can be used as an operator in a combination
((λ (x) (* x x )) 3)

; Conditional expressions:
; (cond
;    [<p1> <e1>]
;    [<p2> <e2>]
;    ...
;    [<pn> <en>]
;    [else <en+1>])

; (if <predicate> <consequence> <alternative>)
; > (if (positive? -5) (error "doesn't get here") 2)
;   2
;> (if (positive? 5) 1 (error "doesn't get here"))
;   1
;> (if 'we-have-no-bananas "yes" "no")
;   "yes"

; (and <e1> <e2> ... <en>)
; (or <e1> <e2> ... <en>)
; (not <e1> <e2> ... <en>)

(define (abs x)
    (cond
        [(> x 0) x]
        [(< x 0) (- x)]
        [(= x 0) 0]))

(define (abs0 x)
    (cond
        [(> x 0) x]
        [(< x 0) (- x)]
        [else 0]))

(define (abs1 x)
    (if (< x 0) (- x) (x)))

(define (interval5 x)
    (and (> x 5) (< x 10)))

; Free vs Bound Variables using Lexical Scoping 
; Implementation using Bound variable x
;    (define (sqrt x)
;        (define (good-enough? guess x)
;            (< (abs (- (square guess) x)) 0.001))
;        (define (improve guess x)
;            (average guess (/ x guess)))
;        (define (sqrt-iter guess x)
;            (if (good-enough? guess x)
;                guess
;                (sqrt-iter (improve guess x) x)))
;        (sqrt-iter 1.0 x))

; Implementation using Free variable x (No need to redefine x in local procedures)
; The scope is formed in terms of x so there is no need to pass it.
;  (define (sqrt1 x)
;    (define (good-enough? guess)
;        (< (abs (- (square guess) x)) 0.001))
;    (define (improve guess)
;        (average guess (/ x guess)))
;    (define (sqrt-iter guess)
;        (if (good-enough? guess)
;           guess
;            (sqrt-iter (improve guess))))
;    (sqrt-iter 1.0))

; Processes describes how the execution evolves
; Procedures are syntactical

; Linear Recursive Process:
; Builds frame trace by delays operation until the a condition is met
; recursive procedure power generates a recursive process

; (power 2 4)
; ==> (* 2 (power 2 3))
; ==> (* 2 (* 2 (power 2 2)))
; ==> (* 2 (* 2 (* 2 (power 2 1))))
; ==> (* 2 (* 2 (* 2 (* 2 (power 2 0)))))
; ==> (* 2 (* 2 (* 2 (* 2 1))))
; ==> (* 2 (* 2 (* 2 2)))
; ==> (* 2 (* 2 4))
; ==> (* 2 8)
; ==> 16

(define (power x n)
    (if (= n 0)
        1
        (* x (power x (- n 1)))))

; Linear Iterative Process:
; Only needs to keep track of running counts by immediate computation. No frame trace built.
; recursive procedure power-iter generates an iterative process

; (power 2 3)
; (power-iter 1 0 2 3)
; (power-iter 2 1 2 3)
; (power-iter 4 2 2 3)
; (power-iter 8 3 2 3)
; (power-iter 16 4 2 3)
; 16
(define (power x n)
    (power-iter 1 0 x n))

(define (power-iter product counter x max-count)
  (if (>= counter max-count)
      product
      (power-iter (* x product) (+ counter 1) x max-count))) ; computes values before call to next procedure

; Lists are Singly linked lists
; Constructed by pairs with an element and a pointer to the next pair
; Pairs are constructed by cons()
; NOTE: applying cdr to the last pair in a list returns the empty list: null, empty, '()
; You cons up a result list and cdr down a list
(define (append list1 list2)
  (if (empty? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


; Procedures are a first class data type (such as an int in C) we can have higher order procedures
; (procedures that manipulate procedures)

; This procedure takes a procedure fn as an argument to compute a sum of a function between 2 
; interger intervals
(define (sum fn a b)
    (if (> a b) 0 
        (+ (fn a) (sum fn (+ a 1) b))))

; if procedure identity is passed to sum it would result in sum computing the integers between a and b
(define (identity x) x)
(define (sum-integers a b)
     (sum identity a b))

; if procedure square is passed to sum it would result in sum computing the sum of squares of each
; integer between a and b
(define (square x) (* x x))
  (define (sum-squares a b)
  (sum square a b))

; λ procedures are defined without a name so they dont resolve to a name instead they resolve to a
; procedure

; in this way λ procedures can be used as an operator in a combination
((λ (x) (* (sin x) (sin x))) (/ pi 4))
; and again using sum we pass λ as a procedure instead of defining a name and passing that name
(sum (λ (x)(*(sin x) (sin x))) a b)

; This procedure returns a root pair (remember data structure) of a quadratic equation defined by its
; polynomial coefficients
(define (roots a b c)
((lambda (d)
    (cons(/ (+(- b)d)(* 2a)) (/ (-(- b)d)(* 2a))))  ; This part is the body of λ
    (sqrt (- (* b b) (* 4 a c)))))                  ; This part is defined as (d)

; 'let' expression allows for better readability than λ procedures, and its scope is in its body
; let   <var > have the value <exp>
;       <var > have the value <exp >
;       ...
;       <var > have the value <exp >
;       in <body>

(define (f x)
(+ (let ((x 3))
       (+ x (* x 10)))  ; <body> of let defined defined name scope
    x))

; (f 5) evaluates to 38 since x defined by let is restricted to let's body (+ x (* x 10))

(define  (f x)
    (let ((x 3)
        (y (+ x 2)))
    (* x y)))

; (f 2) evaluates to 12 since y assignment is still outside of newly defined x's scope and uses x 
; value passed in f's procedure arguments. In the body of let, x and y's assigned values are used.

; Procedures can also be returned from a procedure
(define (make-adder n) (λ (x) (+ x n)))
((make-adder 3) 7)  ; (make-adder n) is now an operator for primitive 7

; Racket Evaluation Review
; ====================================================================================================
; Basic combinations
; ---------------------------------------
; > 21                | 21 
; > a                 | undefined error
; > “a”               | "a"
; > (1 + 2)           | 1 not a procedure
; > (+ 1 2)           | 3
; > (define byte 8)   | N/a
; > byte              | 8
; > (define KB 1024)  | N/a
; > ( + byte byte)    | 16
; > (* byte KB)       | 8192

; Nested Expressions
; ----------------------------------------------------------------------------------------------------
; > (* (+ 3 2) (- 5 10))                                        | -25
; > (define x 10)                                               | N/a
; > (define tripleX (* 3 x))                                    | N/a
; > (+ (- (- 20 (/ tripleX x)) (* x (- (/ x tripleX) 3))) 7)    | 152/3 (frac for  irrational numbers)

; λ procedures
; -------------------------------------------------------------------------------------
; > (lambda (x) (/ x 1024))         | #<procedure>
; > ((lambda (x) (/ x 1024)) 4096)  | 4
; > (lambda () 1)                   | #<procedure>
; > ((lambda () 1))                 | #<procedure> >> 1 (it takes null as a parameter!)
; > ((lambda () 1) 5)               | 1 not a procedure
; > (lambda (y z) (+ z y))          | #<procedure>
; > ((lambda (y z) (+ z y)) 5 4)    | 9
; > ((lambda (y z) (+ z y)) x 7)    | x undefined

; Naming (NOTE: 'arrity' is the number of arguments/parameters a function takes)
; -------------------------------------------------------------------------------------
; (define x 1)
; (define y -1)
; (define adder (lambda (a b) (+ a b)))
; (define identity (lambda (x) x))
; (define one (lambda () 1))
; (define inc (lambda (x) (adder (one) x)))

; > x                                       | 1
; > adder                                   | #<procedure:adder> >> #<procedure:name>
; > (adder)                                 | arrity mismatch
; > (adder 1)                               | arrity mismatch
; > (adder 1 2)                             | 3
; > (identity)                              | arrity mismatch
; > one                                     | #<procedure:one>
; > (one)                                   | 1
; > (inc (adder (identity (one)) (one)))    | 3

; Syntax
; -------------------------------------------------------------------------------------
(define adder (λ (a b) (+ a b)))
; Using regular procedure definition
(define (adder a b) (+ a b))

(define one (λ () 1))
; Using regular procedure definition
(define (one) 1)

(define (plus3 x) (+ x 3))
; functionally equivalent using basic λ expression
(define plus3 (λ (x) (+ x 3)))

; Lists
; -------------------------------------------------------------------------------------
; > ‘(1 2 3 4)                      | ‘(1 2 3 4)
; > (quote 1 2 3)                   | ‘(1 2 3 4) >> bad syntax (quote (1 2 3))
; > (list 1 2 3 4)                  | ‘(1 2 3 4)
; > (define odd (list 1 3 5 7))     | N/a
; > odd                             | '(1 3 5 7)
; > (odd)                           | list not a procedure/operator
; > (list (1 2) 3 4)                | 1 not a procedure
; > (list (+ 1 2) 3 4)              | '(3 3 4)
; > (list ‘+ 1 2)                   | '(#<procedure:+> 1 2)