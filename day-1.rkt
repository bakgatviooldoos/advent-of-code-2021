#lang racket

(define sonar-depths
  (with-input-from-file "day-1-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
        (string->number l)))))

(define (count-window-increases run depths [last-sum +inf.0])
  (cond [(> run (length depths)) 0]
        [else
         (define curr-sum (apply + (take depths run)))
         (+ (if (< last-sum curr-sum) 1 0)
            (count-window-increases run (cdr depths) curr-sum))]))

(displayln (format "number of depth increases considering 1 measurement at a time:\n~a"
 (count-window-increases 1 sonar-depths)))
(newline)

(displayln (format "number of depth increases considering 3 measurements at a time:\n~a"
 (count-window-increases 3 sonar-depths)))
