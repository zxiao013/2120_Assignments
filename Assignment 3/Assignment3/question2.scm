#lang racket
(define L '( 0 1 5 3 3 3 2 1 1 1 1))
(define (sameNum2 L maxNumber maxLength number length)
  (cond ((null? L) '())
        ((null? (cdr L))
         (if (eq? (car L) maxNumber)
             (makelist maxNumber maxLength)
             (if (and (eq? (car L) number)(eq? maxLength (+ 1 length)))
                      (makelist number maxLength)
                      (makelist maxNumber maxLength))))
        ((eq? (car L) (car (cdr L)))
         (if (>= (+ 1 length) maxLength)
             (sameNum2 (cdr L) (car L) (+ 1 maxLength) (car L) (+ 1 length))
             (sameNum2 (cdr L) maxNumber maxLength (car L) (+ 1 length))))
        (else
         (if (and (eq? (car L) number)(eq? maxLength length))
             (sameNum2 (cdr L) maxNumber (+ 1 maxLength) (car(cdr L)) 0)
             (if (and (eq? (car L) number)(eq? maxLength (+ 1 length)))
                  (sameNum2 (cdr L) number maxLength (car(cdr L)) 0)
                  (sameNum2 (cdr L) maxNumber maxLength (car(cdr L)) 0))))))


     (define (makelist m n)
       (if (= n 0)
           (list)                       
           (cons m (makelist m (- n 1)))))

(define (sameNum L) (sameNum2  L (car L) 0 (car L) 0))    
(sameNum L)
