#lang racket

(require racket/generator)

(define-values (player-1 player-2)
  (with-input-from-file "day-21-1.txt"
    (lambda ()
      (values (string->number
               (substring (read-line) (string-length "Player 1 starting position: ")))
              (string->number
               (substring (read-line) (string-length "Player 2 starting position: ")))))))

(define (play-round pawn score rolled)
  (define next-pawn
    (add1 (modulo (+ pawn rolled -1) 10)))
  (values next-pawn (+ score next-pawn)))

(define (100-sided-die)
  (sequence->repeated-generator (range 1 101)))

(define (play-deterministic-dice pawn-1 pawn-2)
  (define roll (100-sided-die))
  
  (let loop
    ([pawn-1 pawn-1] [pawn-2 pawn-2] [score-1 0] [score-2 0] [turns 0])
    (cond [(<= 1000 score-1)
           (* score-2 (* 3 turns))]
          [(<= 1000 score-2)
           (* score-1 (* 3 turns))]
          [(even? turns)
           (define-values (next-pawn next-score)
             (play-round pawn-1 score-1 (+ (roll) (roll) (roll))))
           (loop next-pawn pawn-2 next-score score-2 (add1 turns))]
          [else
           (define-values (next-pawn next-score)
             (play-round pawn-2 score-2 (+ (roll) (roll) (roll))))
           (loop pawn-1 next-pawn score-1 next-score (add1 turns))])))

(define (play-quantum-dice pawn-1 pawn-2)
  (define rolls
    (map (lambda (group)
           (cons (first group) (length group)))
         (group-by identity
                   (for*/list ([i '(1 2 3)]
                               [j '(1 2 3)]
                               [k '(1 2 3)])
                     (+ i j k)))))
  (let loop
    ([pawn-1 pawn-1] [pawn-2 pawn-2] [score-1 0] [score-2 0] [turns 0])
    (cond [(<= 21 score-1) 1+0i]
          [(<= 21 score-2) 0+1i]
          [(even? turns)
           (for/sum ([result rolls])
             (define-values (next-pawn next-score)
               (play-round pawn-1 score-1 (car result)))
             (* (cdr result) (loop next-pawn pawn-2 next-score score-2 (add1 turns))))]
          [else
           (for/sum ([result rolls])
             (define-values (next-pawn next-score)
               (play-round pawn-2 score-2 (car result)))
             (* (cdr result) (loop pawn-1 next-pawn score-1 next-score (add1 turns))))])))

(displayln (format (string-append
                    "product of the losing player's score and the number of rolls\n"
                    "when playing deterministic dice:\n~a")
 (play-deterministic-dice player-1 player-2)))
(newline)

(displayln (format (string-append
                    "number of universes in which either player wins the most when\n"
                    "playing quantum dice:\n~a")
 (let* ([results      (play-quantum-dice player-1 player-2)]
        [player-1-won (real-part results)]
        [player-2-won (imag-part results)])
   (max player-1-won player-2-won))))
