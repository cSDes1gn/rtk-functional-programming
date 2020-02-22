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