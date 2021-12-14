#lang racket

(define vent-lines
  (with-input-from-file "day-5-1.txt"
    (lambda ()
      (define (produce-line x1 y1 x2 y2)
        (if (and (equal? x1 x2) (equal? y1 y2))
            (list (cons x1 y1))
            (cons (cons x1 y1)
                  (produce-line (+ (sgn (- x2 x1)) x1) (+ (sgn (- y2 y1)) y1)
                                x2                     y2))))
      
      (for/list ([l (in-lines)])
        (apply produce-line
               (apply append
                      (map (lambda (start-end)
                             (map string->number (string-split start-end ",")))
                           (string-split l " -> "))))))))

(define (covered-points lines)
  (group-by identity (apply append lines)))

(define (vertical? line)
  (equal? (car (first line)) (car (second line))))

(define (horizontal? line)
  (equal? (cdr (first line)) (cdr (second line))))

(displayln (format "number of points where 2 or more (horizontal or vertical) vent lines cross:\n~a"
 (count (lambda (covered) (< 1 (length covered)))
        (covered-points (filter (lambda (line)
                                  (or (vertical? line) (horizontal? line)))
                                vent-lines)))))
(newline)

(displayln (format "number of points where 2 or more vent lines cross:\n~a"
 (count (lambda (covered) (< 1 (length covered)))
        (covered-points vent-lines))))
