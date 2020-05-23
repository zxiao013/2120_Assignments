#lang racket


(define (calculateFirst choice L)
  (cond
    ((equal? (car (car (cdr (car (cdr choice)))))  "peru") (cons (+ 3 (car L)) (cdr L)))  
     ((equal? (car (car (cdr (car (cdr choice)))))  "greece") (list (car L) (+ 3 (car (cdr L)))  (car (cdr(cdr L)))))
     ((equal? (car (car (cdr (car (cdr choice)))))  "vietnam") (list (car L) (car (cdr L)) (+ 3 (car (cdr(cdr L))))))
     (else L)
     
    )
    )
  

;;;(calculateFirst '("marie" '("vietnam" "peru" "greece")) '(0 0 0))

(define (calculateSecond choice L)
  (cond
    ((equal? (car (cdr (car (cdr (car (cdr choice))))))  "peru") (cons (+ 2 (car L)) (cdr L)))  
     ((equal? (car (cdr (car (cdr (car (cdr choice))))))  "greece") (list (car L) (+ 2 (car (cdr L)))  (car (cdr(cdr L)))))
     ((equal? (car (cdr (car (cdr (car (cdr choice))))))  "vietnam") (list (car L) (car (cdr L)) (+ 2 (car (cdr(cdr L))))))
     (else L)
    )
    )
;;;(calculateSecond '("marie" '("vietnam" "peru" "greece")) '(0 0 3))

(define (calculateThird choice L)
  (cond
    ((equal? (car (cdr (cdr (car (cdr (car (cdr choice)))))))  "peru") (cons (+ 1 (car L)) (cdr L)))
     ((equal? (car (cdr (cdr (car (cdr (car (cdr choice)))))))  "greece") (list (car L) (+ 1 (car (cdr L)))  (car (cdr(cdr L)))))
     ((equal? (car (cdr (cdr (car (cdr (car (cdr choice)))))))  "vietnam") (list (car L) (car (cdr L)) (+ 1 (car (cdr(cdr L))))))
    )
    )

;;;(calculateThird '("marie" '("vietnam" "peru" "greece")) '(0 0 3))


(define (calculate2 x R)
  (if (null? x)
      R 
      (calculate2 (cdr x) (calculateThird (cadar x) ( calculateSecond (cadar x) ( calculateFirst (cadar x) R ))))
      )
  )

;(calculate2 choices '(0 0 0))


;(define (calculate choice) (calculate2(choice  '(0 0 0))))


(define (getbiggest2 T M CI MI)
  (if (< M (car T))
           (getbiggest (cdr T) (car T) (+ 1 CI) (+ 1 CI))
           (getbiggest (cdr T) M (+ 1 CI) CI)
       )
  )

(define ( getbiggest T M CI MI)
  (if (null? T)
       (cond
        ((= MI 0) (cons "peru" M))
        ((= MI 1) (cons "greece" M))
        ((= MI 2) (cons "vietnam" M))
      )
       (if (< M (car T))
           (getbiggest (cdr T) (car T) (+ 1 CI) CI)
           (getbiggest (cdr T) M (+ 1 CI) MI)
       )
  
      )
  )
(define choices '('("marie" '("peru" "greece" "vietnam"))
                        '("jean" '("greece" "peru" "vietnam"))
                        '("sasha" '("vietnam" "peru" "greece"))
                        '("helena" '("peru" "vietnam" "greece"))
                        '("emma" '("greece" "peru" "vietnam"))))
;(getbiggest '(12 10 8) 0 0 0)
(define (destination choice)
  ( getbiggest (calculate2 choice '(0 0 0)) 0 0 0)  
  )
(destination choices)
