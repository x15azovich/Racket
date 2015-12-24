#lang racket
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Exercise-254-is-index) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; Exercise 281. 
; 
; Develop is-index?, a specification for the index function.
; Use is-index? to formulate a check-satisfied test for index.


; X [List-of X] -> [Maybe N]
; determine the (0-based) index of the first occurrence of x in l, 
; #false otherwise
(check-expect (index "a" '()) #false)
(check-expect (index "a" '("b" "c" "a")) 2)

(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; X [Maybe N] [List-of X] -> Boolean
; is x in within the bounds of the length of the list
; if not #false
(check-expect ((is-index? "a" '())            #false) #true)
(check-expect ((is-index? "a" '("b" "c" "a")) 2)      #true)
(check-expect ((is-index? "a" '("a" "b" "c")) 0)      #true)
(check-expect ((is-index? "a" '("b")) 1)              #false)
(check-expect ((is-index? "a" '("a" "b" "c")) 3)      #false)

(define (is-index? x lst)
  (lambda (n)
    (if (or  (and (member? x lst)
                  (>= n 0)
                  (<  n (length lst)))
             (and (not (member? x lst))
                  (equal? n #false)))
        #true
        #false)))