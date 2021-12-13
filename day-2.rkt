#lang racket

(define commands
  (with-input-from-file "day-2-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
        (let ([s (string-split l " ")])
          (cons (string->symbol (first s))
                (string->number (second s))))))))

(define (follow-method-1 commands)
  (foldl (lambda (command state)
           (define distance (first state))
           (define depth    (second state))
           
           (match command
             [`(forward . ,X) (list (+ distance X)
                                    depth)]
             [`(up . ,X)      (list distance
                                    (- depth X))]
             [`(down . ,X)    (list distance
                                    (+ depth X))]))
         '(0 0)
         commands))

(define (follow-method-2 commands)
  (foldl (lambda (command state)
           (define distance (first state))
           (define depth    (second state))
           (define aim      (third state))
           
           (match command
             [`(forward . ,X) (list (+ distance X)
                                    (+ depth (* aim X))
                                    aim)]
             [`(up . ,X)      (list distance
                                    depth
                                    (- aim X))]
             [`(down . ,X)    (list distance
                                    depth
                                    (+ aim X))]))
         '(0 0 0)
         commands))


(define state-1 (follow-method-1 commands))
(displayln (format "product of horizontal position and depth with naive control usage:\n~a"
 (* (first state-1) (second state-1))))

(define state-2 (follow-method-2 commands))
(displayln (format "product of horizontal position and depth with proper control usage:\n~a"
 (* (first state-2) (second state-2))))
