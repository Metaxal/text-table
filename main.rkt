#lang racket/base
(require racket/format
         racket/list
         racket/dict
         racket/string
         racket/contract)

(provide
 border-style/c
 (contract-out
  (table->string
   (->* ((listof list?))
        (#:->string (any/c . -> . string?)
         #:border-style border-style/c
         #:framed? boolean?
         #:row-sep? boolean?
         #:align (or/c (listof (or/c 'left 'center 'right))
                       (or/c 'left 'center 'right)))
        string?))
  (simple-table->string
   (->* ((listof list?))
        (#:->string (any/c . -> . string?)
         #:border-style border-style/c
         #:framed? boolean?
         #:row-sep? boolean?
         #:align (or/c (listof (or/c 'left 'center 'right))
                       (or/c 'left 'center 'right)))
        string?)))
 print-table
 print-simple-table)

;; See
;; https://en.wikipedia.org/wiki/Box-drawing_character
;; http://www.utf8-chartable.de/unicode-utf8-table.pl?start=9472&unicodeinhtml=dec
(define table-borders-dict
  '((space         . (#\space  (" " " " " ") (" " " " " ") (" " " " " ") (" " " " " ")))
    (space-single  . (#\─      ("│" " " "│") ("┌" "─" "┐") ("├" "─" "┤") ("└" "─" "┘")))
    (single        . (#\─      ("│" "│" "│") ("┌" "┬" "┐") ("├" "┼" "┤") ("└" "┴" "┘")))
    (rounded       . (#\─      ("│" "│" "│") ("╭" "┬" "╮") ("├" "┼" "┤") ("╰" "┴" "╯")))
    (double        . (#\═      ("║" "║" "║") ("╔" "╦" "╗") ("╠" "╬" "╣") ("╚" "╩" "╝")))
    (heavy         . (#\━      ("┃" "┃" "┃") ("┏" "┳" "┓") ("┣" "╋" "┫") ("┗" "┻" "┛")))))

(define border-styles
  (cons 'latex (dict-keys table-borders-dict)))

(define border-style/c
  (apply or/c
         ; custom style
         (list/c char?
                 (list/c string? string? string?)
                 (list/c string? string? string?)
                 (list/c string? string? string?)
                 (list/c string? string? string?))
         ; default styles
         border-styles))

(define (make-latex-border-style align)
  (define (align-ref al)
    (case al [(left) "l"] [(right) "r"] [(center) "c"]))
  (define als (string-append
               (string-append* "\\begin{tabular}{"
                               (map align-ref align))
               "}"))
  `(#\space  ("" " & " " \\\\")
             (,als "" "")
             ("\\hline" "" "")
             ("\\end{tabular}" "" "")))

(define-syntax-rule (define/for/fold ([x a] ...) (y ...) body ...)
  (define-values (x ...)
    (for/fold ([x a] ...) (y ...)
      body ...)))

(define (transpose xs) (apply map list xs))

(define-syntax-rule (print-table args ...)
  (displayln (table->string args ...)))

(define-syntax-rule (print-simple-table args ...)
  (displayln (simple-table->string args ...)))

(define (simple-table->string ll
                              #:->string [->string ~a]
                              #:border-style [border-style 'space]
                              #:framed? [framed? #f]
                              #:row-sep? [row-sep? #f]
                              #:align [align 'left]) ; like for ~a
  (table->string ll
                 #:->string ->string
                 #:border-style border-style
                 #:framed? framed?
                 #:row-sep? row-sep?
                 #:align align))

(define (table->string ll
                       #:->string [->string ~a]
                       #:border-style [border-style 'single]
                       #:framed? [framed? #t]
                       #:row-sep? [row-sep? #t]
                       #:align [align 'left]) ; like for ~a
  (unless (and (list? ll) (not (empty? ll)) (andmap list? ll))
    (raise-argument-error 'table->string
                          "nonempty list of lists of the same lengths"
                          0 ll))
  (define lens (map length ll))
  (define the-len (first lens))
  (unless (andmap (λ (len) (= len the-len)) (rest lens))
    (error "All rows must have the same length"))
  
  (define/for/fold ([cell-sizes (make-list the-len 0)]
                    [ll-str '()])
                   ([row (in-list ll)])
    (define/for/fold ([new-cell-sizes '()]
                      [subrow-size 0]
                      [row-str '()])
                     ([cell (in-list row)]
                      [size (in-list cell-sizes)])
      (define as-str (->string cell))
      (define str-splitted (string-split (if (non-empty-string? as-str)
                                             as-str
                                             " ")
                                         "\n"))
      (values
       (cons (apply max size (map string-length str-splitted)) new-cell-sizes)
       (max subrow-size (length str-splitted))
       (cons str-splitted row-str)))

    (values
     (reverse new-cell-sizes)
     (cons (transpose
            (for/list ([col-group (in-list (reverse row-str))])
              (append col-group
                      (make-list (- subrow-size (length col-group)) ""))))
           ll-str)))

  ;; Adjust align to the cells
  (when (list? align)
    (define nal (length align))
    (define nc (length cell-sizes))
    (cond [(= nal 0) (error "align cannot be an empty list")]
          [(< nc nal) (set! align (take align nc))] ; trim
          [(> nc nal) (set! align (append align (make-list (- nc nal) (last align))))])) ; extend

  (define align-list
    (if (symbol? align)
        (make-list (length cell-sizes) align)
        align))

  (define style
    (if (list? border-style)
      border-style
      (case border-style
        [(latex) (make-latex-border-style align-list)]
        [else (dict-ref table-borders-dict border-style)])))
  (define-values (row-sep col-seps first-row-corners mid-row-corners last-row-corners)
    (apply values style))

  (define (make-row-line row-corners)
    (string-join
     (for/list ([n (in-list cell-sizes)])
       (make-string n row-sep))
     (second row-corners)
     #:before-first (if framed? (first row-corners) "")
     #:after-last (if framed? (third row-corners) "")))

  (define rows-str
    (for/list ([row-strs (in-list (reverse ll-str))])
      (string-join
       (for/list ([row-str (in-list row-strs)])
         (string-join
          (for/list ([str (in-list row-str)]
                     [size (in-list cell-sizes)]
                     [al (in-list align-list)])
            (~a str #:min-width size #:align al))
                        (second col-seps)
                        #:before-first (if framed? (first col-seps) "")
                        #:after-last (if framed? (third col-seps) "")))
       "\n")))

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

(module+ test
  (require rackunit)
  (check-exn exn:fail? (λ () (table->string '())))
  (check-exn exn:fail? (λ () (table->string '(a))))
  (check-exn exn:fail? (λ () (table->string '([a b] [c]))))
  (check-not-exn (λ () (table->string '([a b] [c d]))))
  )

;; Usage example. To see the output, run:
;; racket -l text-table
(module+ main

  (define table
    '((a b c d e f gggg h)
      (123 456 77 54 1  5646547987 41 1)
      (111 22 3333 44 5 6 7 8888)))

  (define aligns
    '(left center center center center center center right)) ; one alignment per column

  (for* ([align (in-list (list 'left 'center 'right aligns))])
    (newline)
    (newline)
    ; Print values
    (displayln
     (table->string
      (list (list '#:align align))))
    ; Example
    (displayln
     (table->string
      table
      #:align align)))
  
  (for* ([border-style (in-list border-styles)]
         [framed? (in-list '(#t #f))]
         [row-sep? (in-list '(#t #f))])
    (newline)
    (newline)
    ; Print values
    (displayln
     (table->string
      (list (list '#:border-style border-style)
            (list '#:framed? framed?)
            (list '#:row-sep? row-sep?))))
    ; Example
    (displayln
     (table->string
      table
      #:align aligns
      #:border-style border-style
      #:row-sep? row-sep?
      #:framed? framed?)))

  (newline)
  (newline)
  (displayln "Multiline")
  (newline)
  (displayln
   (table->string
    `(["hello\nworld" "1\n2\n3" "3" ""]
      ["" "" "" ""]
      ["a\nbb\nccc\ndddd" "1" "22\n22" ""]))))
