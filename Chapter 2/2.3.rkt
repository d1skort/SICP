#lang sicp


(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point
   (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
   (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-rectangle left-top-point width height)
  (cons left-top-point (cons width height)))

(define (left-top-point rectangle)
  (car rectangle))

(define (height rectangle)
  (cdr (cdr rectangle)))

(define (width rectangle)
  (car (cdr rectangle)))

(define (perimeter rectangle)
  (* 2 (height rectangle) (width rectangle)))

(define (square rectangle)
  (* (height rectangle) (width rectangle)))