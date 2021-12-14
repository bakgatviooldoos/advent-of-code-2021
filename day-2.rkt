#lang racket

(define commands
  (with-input-from-file "day-2-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
        (define split (string-split l " "))
        (cons (string->symbol (first split))
              (string->number (second split)))))))

(define (follow-method-1 commands)
  (foldl (lambda (command state)
           (define distance (first state))
           (define depth    (second state))
           
           (match command
             [`(forward . ,x) (list (+ distance x)
                                    depth)]
             [`(up . ,x)      (list distance
                                    (- depth x))]
             [`(down . ,x)    (list distance
                                    (+ depth x))]))
         '(0 0)
         commands))

(define (follow-method-2 commands)
  (foldl (lambda (command state)
           (define distance (first state))
           (define depth    (second state))
           (define aim      (third state))
           
           (match command
             [`(forward . ,x) (list (+ distance x)
                                    (+ depth (* aim x))
                                    aim)]
             [`(up . ,x)      (list distance
                                    depth
                                    (- aim x))]
             [`(down . ,x)    (list distance
                                    depth
                                    (+ aim x))]))
         '(0 0 0)
         commands))

(define state-1 (follow-method-1 commands))
(displayln (format "product of horizontal position and depth with naive control usage:\n~a"
 (* (first state-1) (second state-1))))
(newline)

(define state-2 (follow-method-2 commands))
(displayln (format "product of horizontal position and depth with proper control usage:\n~a"
 (* (first state-2) (second state-2))))
