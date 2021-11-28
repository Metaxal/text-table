#lang racket/base
(require racket/format
         racket/list
         racket/dict
         racket/string
         racket/match
         racket/contract)

(provide
 border-style/c
 border-style1/c
 border-style2/c
 border-style-frame/c
 string-length=/c
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

;; "Window" style frames.
;; Easier to specify, and more flexible since col seps may be different for top, middle and bottom.
(define table-frames
  '((space
     "    "
     "    "
     "    "
     "    ")
    (single
     "┌─┬┐"
     "│ ││"
     "├─┼┤"
     "└─┴┘")
    (space-single
     "┌──┐"
     "│  │"
     "├──┤"
     "└──┘")
    (rounded
     "╭─┬╮"
     "│ ││"
     "├─┼┤"
     "╰─┴╯")
    (double
     "╔═╦╗"
     "║ ║║"
     "╠═╬╣"
     "╚═╩╝")
    (heavy
     "┏━┳┓"
     "┃ ┃┃"
     "┣━╋┫"
     "┗━┻┛")))

(define ((string-length=/c n) x)
  (and (string? x)
       (= n (string-length x))))

(define border-style-frame/c
  (list/c (string-length=/c 4)
          (string-length=/c 4)
          (string-length=/c 4)
          (string-length=/c 4)))

(define (frame->border2 frame)
  (map (λ (s) (map string (string->list s))) frame))

;; See
;; https://en.wikipedia.org/wiki/Box-drawing_character
;; http://www.utf8-chartable.de/unicode-utf8-table.pl?start=9472&unicodeinhtml=dec
;; old border styles
(define table-borders
  (for/list ([(name frame) (in-dict table-frames)])
    (cons name (frame->border2 frame)))
  #;
  '((space         . (#\space  (" " " " " ") (" " " " " ") (" " " " " ") (" " " " " ")))
    (space-single  . (#\─      ("│" " " "│") ("┌" "─" "┐") ("├" "─" "┤") ("└" "─" "┘")))
    (single        . (#\─      ("│" "│" "│") ("┌" "┬" "┐") ("├" "┼" "┤") ("└" "┴" "┘")))
    (rounded       . (#\─      ("│" "│" "│") ("╭" "┬" "╮") ("├" "┼" "┤") ("╰" "┴" "╯")))
    (double        . (#\═      ("║" "║" "║") ("╔" "╦" "╗") ("╠" "╬" "╣") ("╚" "╩" "╝")))
    (heavy         . (#\━      ("┃" "┃" "┃") ("┏" "┳" "┓") ("┣" "╋" "┫") ("┗" "┻" "┛")))))

(define border-style1/c
  (list/c char?
          (list/c string? string? string?)
          (list/c string? string? string?)
          (list/c string? string? string?)
          (list/c string? string? string?)))

(define border-style2/c
  (list/c (list/c string? string? string? string?)
          (list/c string? string? string? string?)
          (list/c string? string? string? string?)
          (list/c string? string? string? string?)))

(define (border1->border2 border)
  (match border
    [(list sep-char (list rowl rowm rowr) (list tl tm tr) (list ml mm mr) (list bl bm br))
     (define sep (string sep-char))
     ; default pad-char is " "
     (list (list tl sep tm tr)
           (list rowl " " rowm rowr)
           (list ml sep mm mr)
           (list bl sep bm br))]))

(define border-styles
  (cons 'latex (dict-keys table-frames #;table-borders-dict)))

(define border-style/c
  (apply or/c
         ; custom (old) style, kept for backward compatibility
         border-style1/c
         ; new style, with one row separator per row type
         border-style2/c
         ; custom "window" style
         border-style-frame/c
         ; default styles
         border-styles))

(define (make-latex-border-style align)
  (define (align-ref al)
    (case al [(left) "l"] [(right) "r"] [(center) "c"]))
  (define als (string-append
               (string-append* "\\begin{tabular}{"
                               (map align-ref align))
               "}"))
  `((,als "" "" "")
    ("" " " " & " " \\\\")
    ("\\hline" "" "" "")
    ("\\end{tabular}" "" "" "")))

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
    (cond [(eq? border-style 'latex)
           (make-latex-border-style align-list)]
          [(symbol? border-style)
           (dict-ref table-borders border-style)]
          [(border-style2/c border-style)
           border-style]
          [(border-style1/c border-style) ; old style
           (border1->border2 border-style)]
          [(border-style-frame/c border-style)
           (frame->border2 border-style)]
          [else
           (error "Unrecognized style" border-style)]))
  
  (define-values (top-row-corners col-seps mid-row-corners bottom-row-corners)
    (apply values style))

  (define (make-row-line strs row-corners)
    (define (@ n) (list-ref row-corners n))
    (define row-sep (@ 1))
    (string-join strs
                 (@ 2)
                 #:before-first (if framed? (@ 0) "")
                 #:after-last   (if framed? (@ 3) "")))

  (define (make-border-line row-corners)
    (define row-sep (list-ref row-corners 1))
    (define len (string-length row-sep))
    (make-row-line
     (if (= len 0)
       (make-list (length cell-sizes) "")
       (for/list ([n (in-list cell-sizes)])
         (define-values (q r) (quotient/remainder n len))
         (string-append (string-append* (make-list q row-sep))
                        (substring row-sep 0 r))))
     row-corners))

  (define pad-string (list-ref col-seps 1))
  
  (define rows-str
    (for/list ([row-strs (in-list (reverse ll-str))])
      (string-join
       (for/list ([row-str (in-list row-strs)])
         (make-row-line
          (for/list ([str (in-list row-str)]
                     [size (in-list cell-sizes)]
                     [al (in-list align-list)])
            (~a str #:min-width size #:align al #:pad-string pad-string))
          col-seps))
       "\n")))

  (string-join
   (if row-sep?
       (add-between rows-str (make-border-line mid-row-corners))
       rows-str)
   #:before-first
   (if framed?
       (string-append (make-border-line top-row-corners) "\n")
       "")
   #:after-last
   (if framed?
       (string-append "\n" (make-border-line bottom-row-corners))
       "")
   "\n"))

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
