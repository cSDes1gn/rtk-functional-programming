#lang racket

; Exercise 1: Recursive Summation of a list of numbers
(define (sum numbers)
        (if (null? numbers) 0
            (+ (sum (cdr numbers)) (car numbers))))
(define (avg numbers)
        (/ (sum numbers) (length numbers)))
    
; Exercise 2: Recursive occurence
(define (occurrences numbers n)
        (cond [(null? numbers) 0]
            [(= (car numbers) n) (+ 1 (occurrences (cdr numbers) n))]
            [else (occurrences (cdr numbers) n)]))

; Exercise 3: List to integer conversion iterative approach**
(define (convert lst)
    (define (iter lst val pl)
            (if (null? lst) val
                (iter (cdr lst) (+ val (* (car lst) pl)) (* 10 pl))))
    (iter lst 0 1))

; Exercise 4: Farenheit to Celsuis conversion using iterative approach
(define (convertFC lst)
    (define (createlst newlst lst)
        (if (null? lst) newlst
            (createlst (append newlst (list ((λ (x) (* (/ 5 9) (- x 32))) (car lst)))) (cdr lst))))
    (createlst null lst))
 
; Exercise 5: Elimination threshold using iterative approach
(define (eliminate-threshold lst thresh)
    (define (iter newlst lst)
        (if (null? lst) newlst
            (cond [(or (< (car lst) thresh) (= (car lst) thresh))
                    (iter (append newlst (list (car lst))) (cdr lst))]
                [else (iter newlst (cdr lst))])))
    (iter null lst))


; SYSC 3101 Winter 2020 Lab 2 - Some Test Cases
(display "Testing sum-numbers")
(newline)
(display "Expected: 0, actual: ")
(sum empty)
(display "Expected: 21, actual: ")
(sum (list 1 2 3 4 5 6))
(newline)

(display "Testing average")
(newline)
(display "Expected: 3.5, actual: ")
(avg (list 1 2 3 4 5 6))
(newline)

(display "Testing occurrences")
(newline)
(display "Expected: 3, actual: ")
(occurrences '(1 3 5 2 7 5 8 9 5) 5)
(display "Expected: 0, actual: ")
(occurrences '(1 3 5 2 7 5 8 9 5) 6)
(display "Expected: 0, actual: ")
(occurrences empty 1)
(newline)

(display "Testing convert")
(newline)
(display "Expected: 0, actual: ")
(convert empty)
(display "Expected: 3, actual: ")
(convert (list 3))
(display "Expected: 543, actual: ")
(convert (list 3 4 5))
(newline)

(display "Testing convertFC")
(newline)
(display "Expected: '(), actual: ")
(convertFC empty)
(display "Expected: '(0 100 37.0), actual: ")
(convertFC (list 32 212 98.6))
(newline)

(display "Testing eliminate-threshold")
(newline)
(display "Expected: '(1 2 3 4 4 3 2 1), actual: ")
(eliminate-threshold (list 1 2 3 4 5 6 5 4 3 2 1 20) 4)
(display "Expected: '(), actual: ")
(eliminate-threshold (list 1 2 3 4 5 6 5 4 3 2 1 20) 0)
(display "Expected: '(1 2 3 4 5 6 5 4 3 2 1 20), actual: ")
(eliminate-threshold (list 1 2 3 4 5 6 5 4 3 2 1 20) 25)
(newline)