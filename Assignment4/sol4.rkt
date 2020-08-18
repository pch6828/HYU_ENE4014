#lang racket
(provide (all-defined-out))

;1. check_bst
(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (make_list BST)
  (if (null? BST)
      null
      (append (make_list (car (cdr BST))) (cons (car BST) (make_list (car (cdr (cdr BST))))))))

(define (is_sorted xs)
  (if (null? xs)
      #t
      (if (null? (cdr xs))
          #t
          (and (< (car xs) (car (cdr xs))) (is_sorted (cdr xs))))))

(define (check_bst BST)
  (is_sorted (make_list BST)))

;2. apply
(define (apply f BST)
  (if (null? BST)
      null
      (list (f (car BST)) (apply f (car (cdr BST))) (apply f (car (cdr (cdr BST)))))))

;3. equals
(define (is_same_list xs ys)
  (if (null? xs)
      (null? ys)
      (if (null? ys)
          #f
          (and (= (car xs) (car ys)) (is_same_list (cdr xs) (cdr ys))))))

(define (equals BST1 BST2)
  (is_same_list (make_list BST1) (make_list BST2)))