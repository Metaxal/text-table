#lang racket/base

(require rackunit
         "../utils.rkt")

(check-true ((pattern-list-of number?) '()))
(check-true ((pattern-list-of number?) '1))
(check-true ((pattern-list-of number?) '(1)))
(check-true ((pattern-list-of number?) '(1 2 3)))
(check-true ((pattern-list-of number?) '(1 ...)))
(check-true ((pattern-list-of number?) '(1 2 ...)))
(check-true ((pattern-list-of number?) '(1 2 ... ...)))
(check-true ((pattern-list-of number?) '(1 2 3 ... ... ... 4)))

(check-false ((pattern-list-of number?) 'a)) ; not a number
(check-false ((pattern-list-of number?) '(a))) ; not a number
(check-false ((pattern-list-of number?) '(...))) ; too many dots
(check-false ((pattern-list-of number?) '(1 2 ... ... ...))) ; too many dots
(check-false ((pattern-list-of number?) '(1 2 ... ... 3 ...))) ; can't have more than 1 ...group

(check-equal? (pattern-list->list 'x 3)
              '(x x x))
(check-equal? (pattern-list->list '(x) 3)
              '(x x x))
(check-equal? (pattern-list->list '(a ...) 3)
              '(a a a))
(check-equal? (pattern-list->list '(a b c) 5)
              '(a b c c c))
(check-equal? (pattern-list->list '(a b c ... ...) 10)
              '(a b c b c b c b c b))
(check-equal? (pattern-list->list '(a b c ... ... d e) 10)
              '(a b c b c b c b d e))
(check-equal? (pattern-list->list '(a b c ... ... d e) 3)
              '(a d e))
(check-exn exn:fail? (λ () (pattern-list->list '(a b c ... ... d e) 2)))
(check-exn exn:fail? (λ () (pattern-list->list '() 2)))
(check-exn exn:fail? (λ () (pattern-list->list '(a b c) 2)))
(check-equal? (pattern-list->list '() 0)
              '())
  
(check-equal? (group-by-lengths '(a b c d e f)
                                '(1 2 3))
              '((a) (b c) (d e f)))
(check-equal? (group-by-lengths '(a b c d e f)
                                '(0 0 1 0 2 3))
              '(() () (a) () (b c) (d e f)))
(check-exn exn:fail? (λ () (group-by-lengths '(a b c d e f)
                                             '(1 2 2))))
(check-exn exn:fail? (λ () (group-by-lengths '(a b c d e f)
                                             '(1 2 4))))

(check-equal? (apply/2d-list-as-list (λ (l) (map add1 l))
                                     '((1) (2 3) (4 5 6) (7)))
              '((2) (3 4) (5 6 7) (8)))

(check-equal? (string-repeat "abc" 7)
              "abcabca")
(check-equal? (string-repeat "abc" 0)
              "")
(check-equal? (string-repeat "abc" 2)
              "ab")
