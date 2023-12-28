#lang racket/base
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/string)

(struct expr [var op value result] #:transparent)

(define (make-result-expr result)
  (expr 'none 'none 'none result))

(define (part-cat part cat)
  (hash-ref part cat))

;;; Read and parse workflows and parts

(define (read-input input-file)
  (let-values ([(workflow-lines part-lines)
               (splitf-at (port->lines (open-input-file input-file) #:line-mode 'linefeed)
                          non-empty-string?)])
    (list (parse-workflows workflow-lines) (parse-parts (drop part-lines 1)))))

(define (parse-workflows lines)
  (for/hash ([line lines])
    (match-let* ([(list label expr-str) (string-split (string-replace line #rx"[{|}]" " ") #:trim? #t)]
                 [expr-lst (string-split expr-str "," #:trim? #t)]
                 [exprs (map parse-expr expr-lst)])
      (values label exprs))))

(define (parse-expr expr-str) 
  (if (string-contains? expr-str ":")
    (match-let ([(list pred result) (string-split expr-str ":")])
      (expr (string-ref pred 0) 
            (string-ref pred 1)
            (string->number (substring pred 2))
            result))
    (make-result-expr expr-str)))

(define (parse-parts lines)
  (for/list ([line lines])
    (apply hash
           (flatten
             (map (lambda (lst) (list (string-ref (first lst) 0) (string->number (second lst))))
                  (regexp-match* #rx"([a-z])=([0-9]+)" line #:match-select cdr))))))

;;; Evaluate expressions and workflows

(define (op->fn op)
  (match op
    [#\< <]
    [#\> >]))

;;; Part 1

(define (eval-workflow flow part all-flows)
  (match (eval-expr (first flow) part)
    [#f (eval-workflow (rest flow) part all-flows)]
    ["A" #t]
    ["R" #f]
    [new-flow (eval-workflow (hash-ref all-flows new-flow) part all-flows)]))

(define (eval-expr e part)
  (cond
    [(eq? (expr-var e) 'none) (expr-result e)]
    [else
     (if ((op->fn (expr-op e)) (part-cat part (expr-var e)) (expr-value e))
         (expr-result e)
         #f)]))

(define (sum-accepted flows parts)
  (define in-flow (hash-ref flows "in"))
  (for/fold ([accepted-sum 0])
            ([part parts])
    (if (eval-workflow in-flow part flows)
        (+ accepted-sum (part-value part))
        accepted-sum)))

(define (part-value part)
    (for/sum ([val (hash-values part)]) val))
                        
;;; Part 2

(define all-parts (hash #\a (cons 1 4000) #\m (cons 1 4000) #\s (cons 1 4000) #\x (cons 1 4000)))

(define (count-accepted-combinations flows)
  (count-combinations (list (cons all-parts "in")) flows 0))

(define (count-combinations flow-q flows combinations)
  (cond
    [(empty? flow-q) combinations]
    [else
     (match-let ([(cons part flow-id) (first flow-q)])
       (cond
         [(string=? flow-id "A")
          (count-combinations (rest flow-q) flows (+ combinations
                                                     (count-part-combinations part)))]
         [(string=? flow-id "R")
          (count-combinations (rest flow-q) flows combinations)]
         [else
          (count-combinations (append (rest flow-q) (eval-workflow-range (hash-ref flows flow-id) part))
                              flows
                              combinations)]))]))
     
(define (eval-workflow-range flow part [new-flows '()])
  (cond
    [(or (empty? flow) (eq? part 'none)) new-flows]
    [else
     (match-let ([(list matched unmatched) (eval-expr-range (first flow) part)])
       (eval-workflow-range (rest flow) unmatched (cons matched new-flows)))]))
    
  
(define (eval-expr-range e part)
  (cond
    [(eq? (expr-var e) 'none)
     (list (cons part (expr-result e)) 'none)]
    [else
     (match-let* ([part-range (part-cat part (expr-var e))]
                  [split-value (expr-value e)]
                  [op (expr-op e)]
                  [(list pass-range fail-range) (split-range part-range op split-value)])
       (list (cons (update-part part (expr-var e) pass-range) (expr-result e))
             (update-part part (expr-var e) fail-range)))]))
    
(define (update-part part var range)
  (if (eq? range 'none)
      'none
      (hash-set part var range)))

(define (split-range r op split)
  (match-let ([(cons lo hi) r])
    (match op
      [#\<
       (cond
         [(< hi split) (list r 'none)]
         [(<= split lo) (list 'none r)]
         [else (list (cons lo (sub1 split)) (cons split hi))])]
      [#\>
       (cond
         [(<= hi split) (list 'none r)]
         [(< split lo) (list r 'none)]
         [else (list (cons (add1 split) hi) (cons lo split))])])))

(define (count-part-combinations part)
  (for/product ([r (hash-values part)])
    (add1 (- (cdr r) (car r)))))
      
;;; Entry point

(module+ main
  (match-let ([(list flows parts) (read-input "input.txt")])
    (printf "AoC 2023 Day 19 - Aplenty~n")
    (printf "Part 1: ~a~n" (sum-accepted flows parts))
    (printf "Part 2: ~a~n" (count-accepted-combinations flows))))

;;; Tests

(module+ test
  (require rackunit)

  (check-equal? (split-range (cons 1000 1500) #\< 2000) (list (cons 1000 1500) 'none))
  (check-equal? (split-range (cons 1000 3000) #\< 2000) (list (cons 1000 1999) (cons 2000 3000)))
  (check-equal? (split-range (cons 2500 3000) #\< 2000) (list 'none (cons 2500 3000)))
  (check-equal? (split-range (cons 1000 2000) #\< 2000) (list (cons 1000 1999) (cons 2000 2000)))
  (check-equal? (split-range (cons 2000 3000) #\< 2000) (list 'none (cons 2000 3000)))

  (check-equal? (split-range (cons 1000 1500) #\> 2000) (list 'none (cons 1000 1500)))
  (check-equal? (split-range (cons 1000 3000) #\> 2000) (list (cons 2001 3000) (cons 1000 2000)))
  (check-equal? (split-range (cons 2500 3000) #\> 2000) (list (cons 2500 3000) 'none))
  (check-equal? (split-range (cons 1000 2000) #\> 2000) (list 'none (cons 1000 2000)))
  (check-equal? (split-range (cons 2000 3000) #\> 2000) (list (cons 2001 3000) (cons 2000 2000)))

  (match-let ([(list flows parts) (read-input "input1.txt")])
    (check-equal? (sum-accepted flows parts) 19114)
    (check-equal? (count-accepted-combinations flows) 167409079868000)))

