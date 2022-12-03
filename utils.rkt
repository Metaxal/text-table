#lang racket/base

(require racket/format
         racket/string
         racket/list
         racket/match
         version/utils)

(provide (all-defined-out))

(define-syntax-rule (define/for/fold ([x a] ...) (y ...) body ...)
  (define-values (x ...)
    (for/fold ([x a] ...) (y ...)
      body ...)))

;==============;
;=== String ===;
;==============;

(define (string-repeat str len)
  (cond [(= 0 len) ""]
        [else
         (define str-len (string-length str))
         (when (= 0 str-len)
           (raise-argument-error 'repeat-string "non-empty string" str))
         (define-values (q r) (quotient/remainder len str-len))
         (string-append* (append (make-list q str)
                                 (list (substring str 0 r))))]))
; Differs from ~a:
#;(repeat-string "abc" 5)
#;"abcab"
#;(~a "" #:pad-string "abc" #:min-width 5)
#;"bcabc"

(define (~r* #:sign [sign #f]
             #:base [base 10]
             #:precision [precision 6]
             #:notation [notation 'positional]
             #:format-exponent [format-exponent #f]
             #:min-width [min-width 1]
             #:pad-string [pad-string " "]
             #:groups [groups '(3)]
             #:group-sep [group-sep ""]
             #:decimal-sep [decimal-sep "."])
  (if (version<? (version) "8.5.0.5")
    (λ (x)
      (if (rational? x)
        (~r x
            #:sign sign
            #:base base
            #:precision precision
            #:notation notation
            #:format-exponent format-exponent
            #:min-width min-width
            #:pad-string pad-string)
        (~a x #:min-width min-width #:pad-string pad-string)))
    (λ (x)
      (if (rational? x)
        (~r x
            #:sign sign
            #:base base
            #:precision precision
            #:notation notation
            #:format-exponent format-exponent
            #:min-width min-width
            #:pad-string pad-string
            #:groups groups
            #:group-sep group-sep
            #:decimal-sep decimal-sep)
        (~a x #:min-width min-width #:pad-string pad-string)))))

;============;
;=== List ===;
;============;

(define (transpose xs)
  (when (or (not (list? xs)) (empty? xs))
    (raise-argument-error 'transpose "non-empty list?" xs))
  (apply map list xs))

(define ((pattern-list-of pred?) l)
  (if (not (list? l))
    (pred? l)
    (let loop ([l l] [n-pre-dots 0] [n-dots 0])
      (cond [(null? l) #true]
            [(eq? (car l) '...)
             (and n-dots
                  (< n-dots n-pre-dots)
                  (loop (cdr l) n-pre-dots (+ n-dots 1)))]
            [(pred? (car l))
             (loop (cdr l)
                   (+ 1 n-pre-dots) ; unconditionally, that's fine
                   (and n-dots (not (> n-dots 0)) n-dots))] ; can't use dots anymore when #f
            [else #f]))))

;; l : (pattern-list-of any/c)
;; n-elts: exact-nonnegative-integer?
;; -> list?
(define (pattern-list->list l n-elts #:truncate-ok? [truncate-ok? #f])
  (match l
    ['()
     (unless (= n-elts 0)
       (error "List is empty but n-elts > 0" l n-elts))
     '()]
    [(list front1 (and front (not '...)) ... '... (and '... dots) ... tail ...)
     (define n-dots (+ (length dots) 1))
     (define-values (head rep) (split-at-right (cons front1 front) n-dots))
     (define-values (n-rep rem) (quotient/remainder (- n-elts (length head) (length tail))
                                                    n-dots))
     (cond [(and (>= n-rep 0) (>= rem 0))
            (append head (append* (make-list n-rep rep)) (take rep rem) tail)]
           [truncate-ok?
            (take (append head tail) n-elts)]
           [else
            (error "Minimum length of list l exceeds n-elt" l n-elts)])]
    [(? list?)
     ; Repeat the last element.
     (pattern-list->list (append l (list (last l) '...)) n-elts #:truncate-ok? truncate-ok?)]
    [else (make-list n-elts l)]))

(define (group-by-lengths l lens)
  (unless (= (apply + lens) (length l))
    (error "List length and sum lengths don't match" (length l) (apply + lens)))
  (let loop ([l l] [lens lens] [res '()])
    (cond [(and (empty? l) (empty? lens))
           (reverse res)]
          [else
           (define-values (subl new-l) (split-at l (first lens)))
           (loop new-l (rest lens) (cons subl res))])))

;; proc : (listof T) . any -> (listof U)
;; (proc col args) must return a list of the same length as col.
;; Applies proc on the flattened ll, but re-structures the result
;; to have the same 2d shape of l.
;; Useful to align a column where cells are lists of strings
;; as a flat list of strings.
(define (apply/2d-list-as-list proc ll . args)
  ; Flatten, but keep info about cell lengths.
  (define lens (map length ll))
  (define flat-l (append* ll))
  (define flat-res (apply proc flat-l args))
  ; Unflatten.
  (group-by-lengths flat-res lens))
