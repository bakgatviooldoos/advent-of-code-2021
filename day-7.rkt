#lang racket

(define crab-submarine-distances
  (with-input-from-file "day-7-1.txt"
    (lambda ()
      (map string->number (string-split (read-line) ",")))))

(define (fuel-consumption new-x distances #:measure-with distance-func)
  (apply + (map (lambda (x)
                  (distance-func new-x x))
                distances)))

(define (optimal-point distances #:measure-with distance-func)
  (define optimal-x
    (argmin (lambda (x)
              (fuel-consumption x distances #:measure-with distance-func))
            (range (apply min distances) (add1 (apply max distances)))))
  (cons optimal-x
        (fuel-consumption optimal-x distances #:measure-with distance-func)))

(define norm-distance-function
  (lambda (x1 x2)
    (abs (- x1 x2))))

(define crab-distance-function
  (lambda (x1 x2)
    (define n (abs (- x1 x2)))
    (/ (* n (+ n 1)) 2)))

(displayln (format "optimal point and fuel consumption with naive fuel usage:\n~a"
 (optimal-point
  crab-submarine-distances
  #:measure-with norm-distance-function)))

(displayln (format "optimal point and fuel consumption with crabwise fuel usage:\n~a"
 (optimal-point
  crab-submarine-distances
  #:measure-with crab-distance-function)))
