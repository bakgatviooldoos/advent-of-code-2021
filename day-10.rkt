#lang racket

(define subsystem-lines
  (with-input-from-file "day-10-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
          (filter non-empty-string? (string-split l ""))))))

(define (close? bracket)
  (or (equal? ")" bracket)
      (equal? "]" bracket)
      (equal? "}" bracket)
      (equal? ">" bracket)))
  
(define (parse-line line)
  (define bracket->number
    (hash "(" +1 ")" -1
          "[" +2 "]" -2
          "{" +3 "}" -3
          "<" +4 ">" -4))
  
  (let loop ([line line]
             [open (list)])
    (cond [(empty? line) open]
          [(close? (car line))
           (if (zero? (+ (hash-ref bracket->number (car line))
                         (hash-ref bracket->number (car open))))
               (loop (cdr line) (cdr open))
               line)]
          [else
           (loop (cdr line) (cons (car line) open))])))

(displayln (format "syntax checker score and auto-complete score of lines:\n~a"
 (let loop ([stx-check-points 0]
            [auto-cmpl-scores (list)]
            [lines            subsystem-lines])
   (define open->points
     (hash "(" 1
           "[" 2
           "{" 3
           "<" 4))

   (define close->points
     (hash ")"     3
           "]"    57
           "}"  1197
           ">" 25137))
   
   (cond [(empty? lines)
          (define middle-score
            (list-ref (sort auto-cmpl-scores <)
                      (/ (sub1 (length auto-cmpl-scores)) 2)))
          (cons stx-check-points middle-score)]
         [else
          (define parsed
            (parse-line (car lines)))
         
          (cond [(empty? parsed)
                 (loop stx-check-points auto-cmpl-scores (cdr lines))]
                [(close? (car parsed))
                 (loop (+ stx-check-points
                          (hash-ref close->points (car parsed)))
                       auto-cmpl-scores
                       (cdr lines))]
                [else
                 (define score
                   (let loop ([score 0]
                              [open  parsed])
                     (if (empty? open)
                         score
                         (loop (+ (* 5 score)
                                  (hash-ref open->points (car open)))
                               (cdr open)))))
                
                 (loop stx-check-points
                       (cons score auto-cmpl-scores)
                       (cdr lines))])]))))

