#lang racket

(define (sum elemList)
  (if
    (null? elemList)
    0
    (+ (car elemList) (sum (cdr elemList)))
  )
)


(define (neuralNode2 A S)
  (lambda (X)
     (if (equal? S "sigmoid")
       (/ 1 (+ 1 (exp (- 0 (+ (car A) (sum (map (lambda (x y) (* x y)) (cdr A) X ) ) )))))
       (0)
       )
   )
)
(define (neuralNode A S) (neuralNode2 A S))

;((neuralNode '(0.1 0.3 0.4) "sigmoid") '(0.5 0.5))

(define (makelist n)
  (if (= n 0)
     (list 0)                       ; base case. Just return (0)
     (cons n (makelist (- n 1)))))

(define (neuralLayer Y X)
    (map (lambda (a) ((neuralNode a "sigmoid") X)) Y ))
     


;(neuralLayer '((0.1 0.3 0.4)(0.5 0.8 0.3)(0.7 0.6 0.6)) '(0.5 0.5))


(define R '(0.5 0.3 0.7 0.1))
(define (neuralNet X R)
  ((neuralNode R "sigmoid") (neuralLayer '((0.1 0.3 0.4)(0.5 0.8 0.3)(0.7 0.6 0.6)) X))
  )
;(neuralNet '(0 1) R)

(define v (vector '1 '2 '3 '4 '5 '6 '7 '8 '9 '10 '11 '12 '13 '14 '15 '16))
(define pi (* 4 (atan 1.0)))

(define (applyNet2 N K)
  (if (< N K)
  (list)
  (cons (neuralNet (list (sin (/ (* (* 2 pi) (- K 1)) N)) (cos (/ (* (* 2 pi) (- K 1)) N)) ) R) (applyNet2 N (+ 1 K)))
         ))


(define (applyNet N) (applyNet2 N 1))
(applyNet 16)