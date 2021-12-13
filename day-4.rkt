#lang racket

(define bingo-input
  (with-input-from-file "day-4-1.txt"
    (lambda ()
      (for/list ([l (in-lines)]
                 #:when (non-empty-string? l))
        l))))

(define bingo-numbers
  (map string->number (string-split (first bingo-input) ",")))

(define bingo-boards
  (let loop ([boards (cdr bingo-input)])
    (if (empty? boards)
        (list)
        (cons (apply append
                     (map (lambda (row)
                            (map string->number
                                 (filter non-empty-string? (string-split row " "))))
                          (take boards 5)))
              (loop (drop boards 5))))))

(define (mark-boards number boards)
  (for/list ([board boards])
    (map (lambda (x)
           (if (equal? number x) #f x))
         board)))

(define rows
  (for/list ([col-index (in-range 5)])
    (for/list ([row-index (in-range 5)])
      (+ (* 5 col-index) row-index))))

(define cols
  (for/list ([col-index (in-range 5)])
    (for/list ([row-index (in-range 5)])
      (+ col-index (* 5 row-index)))))

(define (board-wins? board)
  (or
   (ormap (lambda (row)
            (andmap (lambda (index)
                      (false? (list-ref board index)))
                    row))
          rows)
   (ormap (lambda (col)
            (andmap (lambda (index)
                      (false? (list-ref board index)))
                    col))
          cols)))

(define (score bingo-state)
  (define (bingo-score winning-number board)
    (* winning-number
       (foldl + 0 (filter-not false? board))))
  (define winners (first bingo-state))
  (define numbers (second bingo-state))
  (define boards  (third bingo-state))
  (map (lambda (winner)
         (bingo-score (car numbers) winner))
       winners))

(define (play-bingo boards numbers)
  (define-values (winners busy)
    (partition board-wins? (mark-boards (car numbers) boards)))

  (cond [(empty? winners)
         (play-bingo busy (cdr numbers))]
        [else
         (list winners numbers busy)]))

(define (play-bingo-all boards numbers)
  (define bingo-state  (play-bingo boards numbers))
  (define numbers-left (second bingo-state))
  (define boards-left  (third bingo-state))

  (if (empty? boards-left)
      bingo-state
      (play-bingo-all boards-left (cdr numbers-left))))

(score (play-bingo bingo-boards bingo-numbers))
(score (play-bingo-all bingo-boards bingo-numbers))


  
