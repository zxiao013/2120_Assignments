#lang racket


(define (read_file_fun filename) (let ((p (open-input-file filename)))
  (let f ((x (read-line p)))   ; reading from file
 (if (eof-object? x)  ; check for eof
(begin
(close-input-port p)
  '())
(cons x (f (read-line p)))))))


(define (append-list L)
(if (null?  L)
    L
(cons (regexp-split #rx"[-\".,\n]+" (car L)) (append-list (cdr L)))))

(define (remove-last lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (remove-last (cdr lst)))))

(define (final-list L)
  (if (null? (cdr L))
      L
      (cons (remove-last (car L))(final-list (cdr L)))))


(define (better? x y l) (memq y (cdr (memq x l))))

(define (length-list L)
  (if (null? L)
0
      (+ 1 (length-list (cdr L)))))

(define (stable-matching Eprefs Sprefs)
  (define E (map car Eprefs))
  (define employments (make-hasheq))
  (define preferences (make-hasheq))
  (define (hire! e s)
    (hash-set! employments e s)
    (hash-set! employments s e))
  (for ([e Eprefs]) (hash-set! preferences (car e) (cdr e)))
  (for ([s Sprefs]) (hash-set! preferences (car s) (cdr s)))
  (let loop ()
    (define e+s
      (for/or ([e E])
        (and (not (hash-ref employments e #f))   
             (let ([p (hash-ref preferences e)])
               (and (pair? p)
                    (let ([s (car p)])
                      (hash-set! preferences e (cdr p))
                      (cons e s)))))))
    (when e+s
      (define e (car e+s))
      (define s (cdr e+s))
      (define e* (hash-ref employments s #f))   
      (cond [(not e*) (hire! e s)]             
            [(better? e e* (hash-ref preferences s)) 
             (hire! e s)
             (hash-set! employments e* #f)])     
      (loop)))
  employments)
 
(define (find-unstable Eprefs Sprefs matches)
  (for*/or ([e (map car Sprefs)] [s (map car Sprefs)])
    (define s* (hash-ref matches e))
    (define e* (hash-ref matches s))
    (and (not (eq? e e*))
         (better? s s* (cdr (assq e Eprefs)))
         (better? e e* (cdr (assq s Sprefs)))
         (cons e s))))

(define proc-out-file
  (lambda (filename proc)
(let ((p (open-output-file filename)))
  (let ((v (proc p)))
(close-output-port p)
v))))

(define EMPLOYER (final-list (append-list (read_file_fun "coop_e_3x3.txt"))))
(define STUDENT (final-list (append-list (read_file_fun "coop_s_3x3.txt"))))

(define (matches EMPLOYER STUDENT) (stable-matching EMPLOYER STUDENT))
(define (stableMatching F1 F2) (define EMPLOYER (final-list (append-list (read_file_fun F1))))
(define STUDENT (final-list (append-list (read_file_fun F2)))) (define S1 (string-append  (string-append (string-append "matches_scheme_"
                                                                            (string-append (number->string (length-list EMPLOYER)) "x"))
                                                                            (number->string (length-list EMPLOYER)) ) ".csv"))
  (proc-out-file S1
  (lambda (p) (for ([e (map car EMPLOYER)]) (write (cons e (hash-ref (matches EMPLOYER STUDENT) e)) p) (newline p)))))
#|


(proc-out-file "list.out"
  (lambda (p)
(let ((list-to-be-printed '(1 2 3 4)))
(let f ((l list-to-be-printed))
(if (not (null? l))
    (begin
(write (car l) p)
(newline p)
(f (cdr l))) 0)))))

(let ([E (map car (take (shuffle EMPLOYER) 2))])
  (define (swap! x y)
    (define t (hash-ref matches x))
    (hash-set! matches x (hash-ref matches y))
    (hash-set! matches y t))
  (swap! (car E) (cadr E))
  (swap! (hash-ref matches (car E)) (hash-ref matches (cadr E))))
|#