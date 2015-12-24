

#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(define r(rectangle 10 10 "outline" "black"))
(define (rectangleBeside)(beside/align "baseline"
                   (rectangle 10 10 "outline" "Black")
                   (rectangle 10 10 "outline" "Black")
                   (rectangle 10 10 "outline" "Black")
                   (rectangle 10 10 "outline" "Black")
                   (rectangle 10 10 "outline" "Black")
                   (rectangle 10 10 "outline" "Black")
                   (rectangle 10 10 "outline" "Black")
                   (rectangle 10 10 "outline" "Black")))



(define (rectangleAbove)(above
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)
  (rectangleBeside)))
(define (hall)(place-image (rectangleAbove) 40 90 (empty-scene 80 180)))
; physical constants 
(define  HEIGHT 180)
(define  WIDTH 80)
(define XSHOTS (/ WIDTH 2))

; graphical constants 
(define BACKGROUND (hall))
(define SHOT (circle 3 "solid" "red"))
 (define number 10)
(define-struct posn [x y])

 (define (XYSHOT)
   (make-posn (random WIDTH) (random HEIGHT)))
; A ShotWorld is List-of-numbers. 
; interpretation the collection of shots fired and moving straight up
 
; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock 1]
    [stop-when done]
    [to-draw to-image]))


(define (tock w)
  (cond
    [(> (length w) number) (cons (first w) '())]
    [else (cons (XYSHOT) w )]))
(define (done w)
  (cond
    [(equal? (length w) number) (output w) #T]
    [else #F]))
(define (output w)
  (cond
    [(empty? w) (break)  ]
     [(print (first w)) (output (rest w))]))
(define (print posn)
  (fprintf (current-output-port)
           "(~a, ~s)"
           (posn-x posn)
           (posn-y posn)))
(define (break)
  (fprintf (current-output-port)
           "\n"))


(define(shootBalloons n)
  (set! number n) (main '()))

; ShotWorld -> Image 
; adds each y on w at (MID,y) to the background image 
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT (posn-x (first w)) (posn-y (first w)) (to-image (rest w)))]))