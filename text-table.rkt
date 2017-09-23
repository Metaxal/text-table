#lang racket

(provide
 (contract-out
  (table->string
   (->* ((listof list/c))
        (#:->string (any/c . -> . string?)
         #:border-style (apply or/c (dict-keys table-borders-dict))
         #:framed? boolean?
         #:row-sep? boolean?
         #:align (or/c 'left 'center 'right))
        string?))))

#|
Features:
- takes as input a list of strings or objects and converts them with ~a by default
- auto-adapt to width
- center, left-align, right-align
- merge cells?
- choose
- simple to use for basic usage
- highly customizable

|#

;; See
;; https://en.wikipedia.org/wiki/Box-drawing_character
;; http://www.utf8-chartable.de/unicode-utf8-table.pl?start=9472&unicodeinhtml=dec
(define table-borders-dict
  '((space         . (#\space  (" " " " " ") (" " " " " ") (" " " " " ") (" " " " " ")))
    (space-single  . (#\─      ("│" " " "│") ("┌" "─" "┐") ("├" "─" "┤") ("└" "─" "┘")))
    (single        . (#\─      ("│" "│" "│") ("┌" "┬" "┐") ("├" "┼" "┤") ("└" "┴" "┘")))
    (rounded       . (#\─      ("│" "│" "│") ("╭" "┬" "╮") ("├" "┼" "┤") ("╰" "┴" "╯")))
    (double        . (#\═      ("║" "║" "║") ("╔" "╦" "╗") ("╠" "╬" "╣") ("╚" "╩" "╝")))))

;; for/fold is very often used with define-values
;; todo: add a finalizer that applies to the value before being set to the variable.
;; useful for lists that often are reversed.
(define-syntax-rule (define/for/fold ([x a] ...) (y ...) body ...)
  (define-values (x ...)
    (for/fold ([x a] ...) (y ...)
      body ...)))

(define (table->string ll
                       #:->string [->string ~a]
                       #:border-style [border-style 'single]
                       #:framed? [framed? #t]
                       #:row-sep? [row-sep? #t]
                       #:align [align 'left]) ; like for ~a
  (let* ([lens (map length ll)]
         [len1 (first lens)])
    (unless (andmap (λ(len)(= len len1)) (rest lens))
      (error "All rows must have the same length")))
  
  (define/for/fold ([cell-sizes (make-list (length (first ll)) 0)]
                    [ll-str '()])
                   ([row (in-list ll)])
    (define/for/fold ([new-cell-sizes '()]
                      [row-str '()])
                     ([cell (in-list row)]
                      [size (in-list cell-sizes)])
      (define str (->string cell))
      (values (cons (max (string-length str) size) new-cell-sizes)
              (cons str row-str)))
    (values (reverse new-cell-sizes)
            (cons (reverse row-str) ll-str)))

  (define style (dict-ref table-borders-dict border-style))
  (define-values (row-sep col-seps first-row-corners mid-row-corners last-row-corners)
    (apply values style))

  (define (make-row-line row-corners)
    (string-join
     (for/list ([n (in-list cell-sizes)])
       (make-string n row-sep))
     (second row-corners)
     #:before-first (if framed? (first row-corners) "")
     #:after-last (if framed? (third row-corners) "")))

  (define align-list
    (if (symbol? align)
        (make-list (length cell-sizes) align)
        align))
  (define rows-str
    (map (λ(row-str)
           (string-join (map (λ(str size al)
                               (~a str
                                   #:min-width size
                                   #:align al))
                             row-str
                             cell-sizes
                             align-list)
                        (second col-seps)
                        #:before-first (if framed? (first col-seps) "")
                        #:after-last (if framed? (third col-seps) "")))
         (reverse ll-str)))

  (string-join
   (if row-sep?
       (add-between rows-str (make-row-line mid-row-corners))
       rows-str)
   #:before-first
   (if framed?
       (string-append (make-row-line first-row-corners) "\n")
       "")
   #:after-last
   (if framed?
       (string-append "\n" (make-row-line last-row-corners))
       "")
   "\n"))


(module+ main

  (define table
    '((a b c d e f gggg h)
      (123 456 77 54 1  5646547987 41 1)
      (111 22 3333 44 5 6 7 8888)))
  (define aligns
    '(left center center center center center center right))
  
  (for* ([border-style (in-list (dict-keys table-borders-dict))]
         [framed? (in-list '(#t #f))]
         [row-sep? (in-list '(#t #f))])
    (newline)
    (newline)
    ; Print values
    (displayln
     (table->string
      (list (list 'border-style border-style)
            (list 'framed? framed?)
            (list 'row-sep? row-sep?))))
    ; Example
    (displayln
     (table->string
      table
      #:align aligns #;'right
      #:border-style border-style
      #:row-sep? row-sep?
      #:framed? framed?))))

