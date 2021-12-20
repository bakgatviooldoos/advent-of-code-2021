#lang racket

(define snailfish-numbers
  (with-input-from-file "day-18-1.txt"
    (lambda ()
      (define (sf-string->number sf-string)
        (define tokens
          (string-split (string-replace (string-replace (string-replace sf-string "[" "[ ")
                                                        "]" " ]")
                                        "," " ")
                        " "))

        (let loop ([tokens  tokens]
                   [numbers (list)])
          (cond [(empty? tokens)
                 numbers]
                [(equal? "[" (first tokens))
                 (loop (rest tokens) numbers)]
                [(equal? "]" (first tokens))
                 (loop (rest tokens)
                       (cons (cons (second numbers)
                                   (first numbers))
                             (drop numbers 2)))]
                [else
                 (loop (rest tokens) (cons (string->number (first tokens))
                                           numbers))])))
      
      (for/list ([l (in-lines)])
        (first (sf-string->number l))))))

(struct zipper [cursor context]
  #:transparent)

(struct cursor [branch content]
  #:transparent)

(define (zip-up expr)
  (zipper (cursor (list) expr) expr))

(define LEFT car)
(define DOWN cdr)

(define (left zip)
  (define focus (zipper-cursor zip))
  (if (not (pair? (cursor-content focus)))
      #f
      (zipper (cursor (cons LEFT (cursor-branch focus))
                      (LEFT (cursor-content focus)))
              (zipper-context zip))))

(define (down zip)
  (define focus (zipper-cursor zip))
  (if (not (pair? (cursor-content focus)))
      #f
      (zipper (cursor (cons DOWN (cursor-branch focus))
                      (DOWN (cursor-content focus)))
              (zipper-context zip))))

(define (back zip)
  (define (do-walk branch context)
    (if (empty? branch)
        context
        ((first branch) (do-walk (rest branch) context))))
  
  (define focus (zipper-cursor zip))
  (if (empty? (cursor-branch focus))
      #f
      (zipper (cursor (rest (cursor-branch focus))
                      (do-walk (rest (cursor-branch focus))
                               (zipper-context zip)))
              (zipper-context zip))))

(define (swap zip value)
  (define (do-swap branch context value)
    (let loop ([branch  (reverse branch)]
               [context context])
      (cond [(empty? branch)
             value]
            [(equal? LEFT (first branch))
             (cons (loop (rest branch)
                         ((first branch) context))
                   (DOWN context))]
            [(equal? DOWN (first branch))
             (cons (LEFT context)
                   (loop (rest branch)
                         ((first branch) context)))])))
  
  (define focus (zipper-cursor zip))
  (zipper (cursor (cursor-branch focus) value)
          (do-swap (cursor-branch focus)
                   (zipper-context zip)
                   value)))

(define (wrap zip branch value)
  (define focus (zipper-cursor zip))
  (zipper (cursor (append (cursor-branch focus) (list branch))
                  (cursor-content focus))
          (if (equal? LEFT branch)
              (cons (zipper-context zip) value)
              (cons value (zipper-context zip)))))

(define (back-past zip branch)
  (define focus (zipper-cursor zip))
  (cond [(empty? (cursor-branch focus))
         (and (equal? 'all branch) zip)]
        [(equal? branch (first (cursor-branch focus)))
         (back zip)]
        [else
         (back-past (back zip) branch)]))

(define (focus-on zip branches match?)
  (if (match? (zipper-cursor zip))
      zip
      (ormap (lambda (branch)
               (define maybe-branch (branch zip))
               (and maybe-branch
                    (focus-on maybe-branch branches match?)))
             branches)))

(define (explodes? zip)
  (define (can-explode? focus)
    (define content (cursor-content focus))
    (and (pair? content)
         (number? (LEFT content))
         (number? (DOWN content))
         (< 3 (length (cursor-branch focus)))))

  (define (literal? focus)
    (number? (cursor-content focus)))

  (define (next-literal-left zip)
    (define go-back
      (back-past zip DOWN))
    (and go-back
         (focus-on (left go-back)
                   (list down)
                   literal?)))

  (define (next-literal-down zip)
    (define go-back
      (back-past zip LEFT))
    (and go-back
         (focus-on (down go-back)
                   (list left)
                   literal?)))
  
  (define maybe-explode
    (focus-on zip (list left down) can-explode?))
  
  (cond [(not maybe-explode) #f]
        [else
         (define swap-zero (swap maybe-explode 0))
         
         (define add-left
           (let* ([maybe-left (next-literal-left swap-zero)])
             (cond [(not maybe-left) swap-zero]
                   [else
                    (define new-value
                      (+ (LEFT (cursor-content (zipper-cursor maybe-explode)))
                         (cursor-content (zipper-cursor maybe-left))))
                    (next-literal-down (swap maybe-left new-value))])))
         
         (define add-down
           (let* ([maybe-down (next-literal-down add-left)])
             (cond [(not maybe-down) add-left]
                   [else
                    (define new-value
                      (+ (DOWN (cursor-content (zipper-cursor maybe-explode)))
                         (cursor-content (zipper-cursor maybe-down))))
                    (next-literal-left (swap maybe-down new-value))])))
         add-down]))

(define (splits? zip)
  (define (can-split? focus)
    (define content (cursor-content focus))
    (and (number? content) (< 9 content)))

  (define maybe-split
    (focus-on zip (list left down) can-split?))
  
  (cond [(not maybe-split) #f]
        [else
         (define number
           (cursor-content (zipper-cursor maybe-split)))
         (swap maybe-split
               (cons (floor (/ number 2)) (ceiling (/ number 2))))]))

(define (snailfish-add zip number)
  (back (wrap zip LEFT number)))

(define (snailfish-reduce zip)
  (define reduced
    (or (explodes? zip) (splits? zip)))
  (if (not reduced)
      (back-past zip 'all)
      (snailfish-reduce (back-past reduced 'all))))

(define (snailfish-magnitude zip)
  (define context (zipper-context zip))
  (let loop ([tree context])
    (+ (* 3 (if (number? (LEFT tree)) (LEFT tree) (loop (LEFT tree))))
       (* 2 (if (number? (DOWN tree)) (DOWN tree) (loop (DOWN tree)))))))

(define sum-of-all-snailfish-numbers
  (let loop ([numbers   (rest snailfish-numbers)]
             [sf-number (zip-up (first snailfish-numbers))])
    (if (empty? numbers)
        sf-number
        (loop (rest numbers)
              (snailfish-reduce (snailfish-add sf-number (first numbers)))))))

(define sum-with-greatest-magnitude
  (let ([combinations
         (for*/list ([i (in-range (length snailfish-numbers))]
                     [j (in-range (length snailfish-numbers))]
                     #:unless (equal? i j))
           (cons i j))])
    (argmax snailfish-magnitude
            (map (lambda (ab)
                   (snailfish-reduce
                    (snailfish-add (zip-up (list-ref snailfish-numbers (car ab)))
                                   (list-ref snailfish-numbers (cdr ab)))))
                 combinations))))

(displayln (format "sum of all the snailfish numbers in the homework assignment:\n~a"
 (snailfish-magnitude sum-of-all-snailfish-numbers)))
(newline)

(displayln (format (string-append
                    "sum of two snailfish numbers in the assignment with greatest\n"
                    "magnitude:\n~a")
 (snailfish-magnitude sum-with-greatest-magnitude)))
