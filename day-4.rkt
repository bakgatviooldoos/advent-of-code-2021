#lang racket

(define-values (bingo-numbers bingo-boards)
  (with-input-from-file "day-4-1.txt"
    (lambda ()
      (define bingo-input
        (for/list ([l (in-lines)]
                   #:when (non-empty-string? l))
          l))

      (define numbers
        (map string->number (string-split (first bingo-input) ",")))

      (define boards
        (let loop ([boards (cdr bingo-input)])
          (if (empty? boards)
              (list)
              (cons (apply append
                           (map (lambda (row)
                                  (map string->number
                                       (filter non-empty-string? (string-split row " "))))
                                (take boards 5)))
                    (loop (drop boards 5))))))
      
      (values numbers boards))))

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
  (define (bingo? dimension)
    (andmap (lambda (index)
              (not (list-ref board index)))
            dimension))
  (or (ormap bingo? rows) (ormap bingo? cols)))

(define (score bingo-state)
  (define (bingo-score winning-number board)
    (* winning-number
       (apply + (filter identity board))))
  
  (define winners (first bingo-state))
  (define numbers (second bingo-state))
  (map (lambda (winner)
         (bingo-score (car numbers) winner))
       winners))

(define (play-bingo boards numbers)
  (define-values (winners busy)
    (partition board-wins? (mark-boards (car numbers) boards)))

  (cond [(not (empty? winners))
         (list winners numbers busy)]
        [else
         (play-bingo busy (cdr numbers))]))

(define (play-bingo-all boards numbers)
  (define bingo-state  (play-bingo boards numbers))
  (define numbers-left (second bingo-state))
  (define boards-left  (third bingo-state))
  
  (if (empty? boards-left)
      bingo-state
      (play-bingo-all boards-left (cdr numbers-left))))

(displayln (format "final score of first winning board:\n~a"
 (car (score (play-bingo bingo-boards bingo-numbers)))))
(newline)

(displayln (format "final score of final winning board:\n~a"
 (car (score (play-bingo-all bingo-boards bingo-numbers)))))
