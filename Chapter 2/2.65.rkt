#lang sicp


(define (entry tree) (car tree))


(define (left-branch tree) (cadr tree))


(define (right-branch tree) (caddr tree))


(define (make-tree entry left right)
  (list entry left right))


(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))


(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (union-set set1 set2)
  (define (union-set-lists s1 s2)
    (cond ((null? s1) s2)
          ((null? s2) s1)
          (else
           (let ((x1 (car s1))
                 (x2 (car s2)))
             (cond ((< x1 x2)
                    (cons x1
                          (union-set-lists (cdr s1) s2)))
                   ((< x2 x1)
                    (cons x2
                          (union-set-lists s1 (cdr s2))))
                   (else
                    (cons x1
                          (union-set-lists (cdr s1) (cdr s2)))))))))
  (list->tree (union-set-lists (tree->list set1)
                               (tree->list set2))))


(define (intersection-set set1 set2)
  (define (intersection-set-lists s1 s2)
    (if (or (null? s1) (null? s2))
        '()
        (let ((x1 (car s1))
              (x2 (car s2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-set-lists (cdr s1)
                                               (cdr s2))))
                ((< x1 x2)
                 (intersection-set-lists (cdr s1) s2))
                ((< x2 x1)
                 (intersection-set-lists s1 (cdr s2)))))))
  (list->tree (intersection-set-lists (tree->list set1)
                                      (tree->list set2))))