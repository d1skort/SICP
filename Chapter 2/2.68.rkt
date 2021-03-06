#lang sicp


(define (accumulate proc initial seq)
  (if (null? seq)
      initial
      (proc (car seq)
            (accumulate proc initial (cdr seq)))))


(define (append x y)
  (accumulate cons y x))


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))


(define (leaf? object)
  (eq? (car object) 'leaf))


(define (symbol-leaf x) (cadr x))


(define (weight-leaf x) (caddr x))


(define (left-branch tree) (car tree))


(define (right-branch tree) (cadr tree))


(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))


(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (left-branch tree)) (cons 0 (encode-symbol symbol (left-branch tree))))
        (else (cons 1 (encode-symbol symbol (right-branch tree))))))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))