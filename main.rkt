#lang racket/base
(require racket/format
         racket/list
         racket/dict
         racket/string
         racket/match
         racket/contract
         "utils.rkt")

(provide
 string-length=/c
 (rename-out [border-styles named-border-styles])
 border-style/c
 border-style1/c
 border-style2/c
 border-style-frame/c
 (contract-out
  (table->string        table->string/c)
  (simple-table->string table->string/c))
 print-table
 print-simple-table)

(define ((string-length=/c n) x)
  (and (string? x)
       (= n (string-length x))))

;==============;
;=== Frames ===;
;==============;

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
  (cons
   '(empty  ("" " " "" "") ("" " " "" "") ("" " " "" "") ("" " " "" ""))
   (for/list ([(name frame) (in-dict table-frames)])
     (cons name (frame->border2 frame))))
  #; ; equivalent to
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
  (cons 'latex (dict-keys table-borders)))

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

(define (make-latex-border-style align framed? col-sep?s)
  (define (align-ref al sep?)
    (string-append (if sep? "|" "")
                   (case al [(left) "l"] [(right) "r"] [(center) "c"])))
  (define als (string-append
               "\\begin{tabular}{"
               (if framed? "|" "")
               (string-append*
                (align-ref (first align) #f)
                (map align-ref (rest align) col-sep?s))
               (if framed? "|}\n\\hline" "}")))
  `((,als "" "" "")
    ("" " " " & " " \\\\")
    ("\\hline" "" "" "")
    (,(if framed? "\\hline\n\\end{tabular}" "\\end{tabular}") "" "" "")))

;==================;
;=== Alignments ===;
;==================;

;; col: (listof string?)
;; align: (or/c 'left 'center 'right)
(define (align-column col align pad-string)
  (define width (apply max (map string-length col)))
  (map (λ (str)
         (~a str #:min-width width #:align align #:pad-string pad-string))
       col))

;; mrow: 2d-list?
;; align: (or/c 'top 'center 'bottom)
(define (align-row mrow align pad-string)
  (define height (apply max (map length mrow)))
  (map (λ (mcell)
         (define n (- height (length mcell)))
         (define str-len (string-length (first mcell)))
         (define pad (string-repeat pad-string str-len))
         (case align
           [(top) (append mcell (make-list n pad))]
           [(bottom) (append (make-list n pad) mcell)]
           [(center)
            (define h (length mcell))
            (define ntop (quotient h 2))
            (append (make-list ntop pad) mcell (make-list (- height h ntop) pad))]
           [else (error "Unknown align-row align:" align)]))
       mrow))

(define numeric-rx #px"^\\s*([-+]?)\\s*(\\d*)(\\.?)(\\d*)(e?)([-+]?)(\\d*)\\s*$")

(define (align-column-numeric col align pad-string)
  (define cols
    (transpose
     (map
      (λ (str)
        (define m (regexp-match numeric-rx str))
        (if m
          (cons #f (rest m))
          (cons str (make-list 7 ""))))
      col)))
  (define rows
    (transpose
     (cons (first cols)
           (map (λ (col align pad) (align-column col align pad))
                (rest cols)
                '(right right left left left left right)
                (list pad-string pad-string "." "0" "e" "+" "0")))))
  (align-column
   (for/list ([row (in-list rows)])
     (or (first row)
         (string-append* (rest row))))
   align
   pad-string))

;=====================;
;=== table->string ===;
;=====================;

(define table->string/c
  (->* ((listof list?))
       (#:->string      (pattern-list-of (any/c . -> . string?))
        #:border-style  border-style/c
        #:framed?       boolean?
        #:row-sep?      (pattern-list-of boolean?)
        #:col-sep?      (pattern-list-of boolean?)
        #:align         (pattern-list-of (or/c 'left 'center 'right))
        #:row-align     (pattern-list-of (or/c 'top 'center 'bottom)))
       string?))

(define-syntax-rule (print-table args ...)
  (displayln (table->string args ...)))

(define-syntax-rule (print-simple-table args ...)
  (displayln (simple-table->string args ...)))


;; If only I could use `define2`… :-/
(define (simple-table->string ll
                              #:border-style [border-style 'space]
                              #:framed?      [framed?      #false]
                              #:row-sep?     [row-sep?     #false]
                              #:col-sep?     [col-sep?     #false]
                              #:->string     [->string     ~a]
                              #:align        [align        'left]
                              #:row-align    [row-align    'top])
  (table->string ll
                 #:border-style border-style
                 #:framed?      framed?
                 #:->string     ->string
                 #:row-sep?     row-sep?
                 #:align        align
                 #:row-align    row-align))

(define (table->string ll
                       #:border-style [border-style 'single]
                       #:framed?      [framed?      #true]
                       #:row-sep?     [row-sep?     #true]
                       #:col-sep?     [col-sep?     #true]
                       #:->string     [->string     ~a]
                       #:align        [align        'left]
                       #:row-align    [row-align    'top])
  ;::::::::::::::::::;
  ;:: Check inputs ::;
  ;::::::::::::::::::;

  (unless (and (list? ll) (not (empty? ll)) (andmap list? ll))
    (raise-argument-error 'table->string
                          "nonempty list of lists of the same lengths"
                          0 ll))
  (define lens (map length ll))
  (define n-rows (length ll))
  (define n-columns (first lens))
  (unless (andmap (λ (len) (= len n-columns)) (rest lens))
    (error "All rows must have the same length"))

  ;::::::::::::::::::::::::::;
  ;:: Expand pattern lists ::;
  ;::::::::::::::::::::::::::;

  (define ->string-list  (pattern-list->list ->string  n-columns))
  (define align-list     (pattern-list->list align     n-columns))
  (define row-align-list (pattern-list->list row-align n-rows))
  (define col-sep?s      (pattern-list->list col-sep?  (- n-columns 1)))
  (define row-sep?s      (pattern-list->list row-sep?  (- n-rows 1)))

  ;:::::::::::::::::::;
  ;:: Prepare style ::;
  ;:::::::::::::::::::;

  (define style
    (cond [(eq? border-style 'latex)
           (define new-style (make-latex-border-style align-list framed? col-sep?s))
           ; force borders
           (set! framed? #t)
           (set! col-sep?s (make-list (- n-columns 1) #t))
           new-style]
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
  (define pad-string (list-ref col-seps 1))

  ;:::::::::::::::::::::::::;
  ;:: Transform the table ::;
  ;:::::::::::::::::::::::::;

  ;; ll: 2d-list of any/c

  ;; 0. Each cell initially contains a string, possibly with newlines, turn
  ;     them into lists of strings without newline.
  ;; TODO: We can't consider that a list in a cell is a multiline,
  ;; but we could have a `cell` struct that can contains multiple elements
  ;; to be displayed on several lines
  (define ll1
    (map (λ (row) (map (λ (cell ->string)
                         (define res (string-split (if (string? cell) cell (->string cell))
                                                   "\n"))
                         (if (empty? res) '("") res))
                       row
                       ->string-list))
         ll))

  #;(writeln ll1)

  ;; ll1: 3d-list of string
  ;; (cells are list of strings)
  
  ;; 1. transpose table,
  ;;    align-column, so that all lines in a cell of all cells of the column have the same width
  ;;    transpose table back
  (define ll2
    (transpose
     (map (λ (mcol align) (apply/2d-list-as-list align-column mcol align pad-string))
          (transpose ll1)
          align-list)))

  #;(writeln ll2)

  ;; 2. align-row, to create the missing lines in the cell, so all cells in the same
  ;;    row have the same number of lines (same height)
  (define ll3 (map (λ (mrow align) (align-row mrow align pad-string))
                   ll2
                   row-align-list))

  #;(writeln ll3)

  (define cell-widths (map (λ (mcell) (string-length (first mcell)))
                          (first ll3)))
  
  (define (make-row-line strs row-corners)
    (define (@ n) (list-ref row-corners n))
    (define row-sep (@ 2))
    ; Special case for latex
    (define no-sep (@ 1))
    (string-append
     (if framed? (@ 0) "")
     (first strs)
     (string-append*
      (append-map (λ (str sep?) (if sep? (list row-sep str) (list no-sep str)))
                  (rest strs)
                  col-sep?s))
     (if framed? (@ 3) ""))
    #;
    (string-join strs
                 (@ 2)
                 #:before-first (if framed? (@ 0) "")
                 #:after-last   (if framed? (@ 3) "")))
  
  ;; 3. For each mrow, transpose the mrow, then string-join the lines of a rows
  ;;    to obtain a simple list of strings, one per line, but without the frame rows.
  (define ll4
    (map (λ (mrow)
           (string-join
            (map (λ (strs) (make-row-line strs col-seps))
                 (transpose mrow))
            "\n"))
         ll3))

  #;(writeln ll4)

  (define (make-sep-line row-corners)
    (define row-sep (list-ref row-corners 1))
    (define row-sep-len (string-length row-sep))
    (make-row-line
     (if (= row-sep-len 0)
       (make-list n-columns "")
       (for/list ([len (in-list cell-widths)])
         (string-repeat row-sep len)))
     row-corners
     #;(string-repeat make-string (string-length pad-string) row-sep)))

  (define mid-sep-line (make-sep-line mid-row-corners))

  ;; 4. Finally, append all the lines together, adding the frame lines if applicable.
  (string-join
   #:before-first
   (if framed?
     (string-append (make-sep-line top-row-corners) "\n")
     "")
   (if row-sep?
     (cons (first ll4)
           (append-map (λ (row sep?) (if sep? (list mid-sep-line row) (list row)))
                       (rest ll4)
                       row-sep?s))
     ll4)
   "\n"
   #:after-last
   (if framed?
     (string-append "\n" (make-sep-line bottom-row-corners))
     "")))

;============;
;=== Main ===;
;============;

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


(module+ drracket

  (for ([col (list
              (map ~a '(1 100 1000))
              (map ~a '(1 100 1e3))
              (map ~a '(1 100 1000 -12))
              (map ~a '(1 100 1000 1.12))
              (map ~a '(1 100 1000 3e25))
              (map ~a '(1 100 1000 3e25 2.12e31))
              '("hello" "1.2e34" "+inf.0" "12e34"
                        "12345" "12.34" "1.234e-3" "2.322e+03" "-nan.0" "-13.3"))])
    (displayln (string-join (align-column-numeric col 'center "_") "\n"))
    (newline))

  (define mcol
    '(("hello") ("1.2e34") ("+inf.0" "12e34")
                ("12345" "12.34" "1.234e-3" "2.322e+03") ("-nan.0") ("-13.3")))

  (apply/2d-list-as-list align-column-numeric mcol 'right "_")
  (flatten (apply/2d-list-as-list align-column-numeric mcol 'right "_")))
