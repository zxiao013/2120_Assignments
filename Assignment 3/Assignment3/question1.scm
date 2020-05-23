#lang racket
(define List '(0 -2 3 -4 1))
(define (changeList List)
  
 (map (lambda (x) (if (> -1 x)
                         (/ 1 (abs x))
                         x)) (map (lambda (x) (if (< 1 x)
                         (* x 10)
                          x))  (filter (lambda (x) (< 1 (abs x))) List))))
    

   
(changeList List)