#lang racket

(define-values (fold-lines manual-paper)
  (with-input-from-file "day-13-1.txt"
    (lambda ()
      (define points
        (sort 
         (let loop ()
           (define line (read-line))
           (cond [(equal? "" line) (list)]
                 [else
                  (define position (string-split line ","))
                  (cons (cons (string->number (first position))
                              (string->number (second position)))
                        (loop))]))
         (lambda (x y)
           (if (= (cdr x) (cdr y))
               (< (car x) (car y))
               (< (cdr x) (cdr y))))))

      (define folds
        (let loop ()
          (define line (read-line))
          (cond [(eof-object? line) (list)]
                [else
                 (define fold (string-split (substring line 11) "="))
                 (cons (cons (string->symbol (first fold))
                             (string->number (second fold)))
                       (loop))])))

      (define max-x (car (argmax car points)))
      (define max-y (cdr (argmax cdr points)))

      (values
       folds
       (let loop ([index  0]
                  [points points])
         (cond [(equal? (add1 max-y) index) (list)]
               [else
                (define-values (points-in-row rest)
                  (splitf-at points (lambda (point)
                                      (equal? index (cdr point)))))
                (define points-at-x (map car points-in-row))
                (cons
                 (build-list (add1 max-x) (lambda (x)
                                            (if (member x points-at-x) "#" " ")))
                 (loop (add1 index) rest))]))))))

(define (combine a b)
  (if (empty? a)
      b
      (cons (if (equal? " " (car a)) (car b) (car a))
            (combine (cdr a) (cdr b)))))

(define (fold-on line paper)
  (match line
    [`(x . ,n)
     (for/list ([row paper])
       (define-values (lhs rhs)
         (split-at row n))

       (reverse (combine (cdr rhs) (reverse lhs))))]
    [`(y . ,n)
     (define-values (top bot)
       (split-at paper n))

     (reverse
      (let loop ([bot (cdr bot)]
                 [top (reverse top)])
        (if (empty? bot)
            top
            (cons (combine (car top) (car bot))
                  (loop (cdr bot) (cdr top))))))]))

(define (fold-on-all lines paper)
  (if (empty? lines)
      paper
      (fold-on-all (cdr lines) (fold-on (car lines) paper))))

(displayln (format "number of dots visible after first fold:\n~a"
 (apply +
        (map (lambda (row)
               (count (lambda (point) (equal? "#" point)) row))
             (fold-on (first fold-lines) manual-paper)))))
(newline)

(displayln (format "activation code after completing folds:\n~a"
 (string-join (map string-join
                   (fold-on-all fold-lines manual-paper))
              "\n")))
