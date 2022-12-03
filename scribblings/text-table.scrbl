#lang scribble/manual
@(require racket/sandbox
          scribble/example
          text-table
          (for-label text-table
                     text-table/utils
                     racket/contract
                     racket/base
                     racket/format
                     racket/string))

@title{Text Table}
@author{Laurent Orseau}

@(define my-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base '(require text-table
                                            text-table/utils
                                            racket/format
                                            racket/list))))

@defmodule[text-table]{
 A simple package to display utf-8 textual tables.}

License: APACHE2+MIT

To install:

@verbatim{raco pkg install text-table}

See the example in the main submodule of the @filepath{main.rkt} file.
You can observe the results by running:

@verbatim{racket -l text-table}

@examples[
 #:eval my-eval
(code:comment "Minimalistic example:")
 (print-table
  '((a b c d e f gggg h)
    (123 456 77 54 1  5646547987 41 1)
    (111 22 3333 44 5 6 7 8888)))

 (code:comment "With more bells and whistles")
 (print-table
  '((a b c d e f gggg h)
    (123 456 77 54 1  5646547987 41 1)
    (111 22 3333 44 5 6 7 8888))
  #:border-style 'double
  #:framed? #f
  #:row-sep? #t
  #:align '(left center right))

 (code:comment "Custom border style using border-style-frame/c")
 (print-table '((abc abc abc)
                (abcdef ab abcdef)
                (a abcdef abc))
              #:border-style
              '("╭─┬╮"
                "│.││"
                "├─┼┤"
                "╰─┴╯")
              #:align '(center)
              #:framed? #t
              #:row-sep? #t)

 (code:comment "Custom border style using border-style2/c")
  (print-table '((abc abc abc)
                (abcdef ab abcdef)
                (a abcdef abc))
              #:border-style
              '(("<table>" "" "" "")
                ("<tr><td> " " " " </td><td> "  " </td></tr>")
                ("" "" "" "")
                ("</table>" "" "" ""))
              #:framed? #t
              #:row-sep? #f)
  
  (code:comment "LaTeX style")
  (print-table '((abc abc abc)
                (abcdef ab abcdef)
                (a abcdef abc))
              #:border-style 'latex)

 (code:comment "Aligning numbers (incorrectly then well)")
  (print-table
   #:row-sep? '(#t #f ...)
   #:col-sep? '(#t #f ...)
   #:align '(left right ... center)
   #:->string
   (list
    ~a (code:comment "Name")
    ~a (~r*) (~r* #:precision '(= 2)) (~r* #:notation 'exponential) (code:comment "Speed")
    ~a) (code:comment "Unit")
    (code:comment "The table:")
    (map (λ (l) (pattern-list->list l 6))
        `((Name Speed ... Unit)
          (Alice 10 ... "km/h")
          (Bob ,(sqrt 2) ... "m/s")
          (Charlie +inf.0 +nan.0 ... ... n/a)
          (light ,(* 299792458 (expt 10 3)) ... "mm/s"))))

 (code:comment "Empty style and doubly repeating alignments")
 (print-simple-table
   #:border-style 'empty
   #:align '(right left ... ...)
  (list (make-list 10 '*)
        (make-list 10 '**)
        (make-list 10 '***)
        (make-list 10 '****)
        (make-list 10 '*****)
        (make-list 10 "|")))

 (code:comment "Multiple separators")
 (print-table (for/list ((i 6)) (for/list ((j 10)) (* (+ i 1) (+ j 1))))
              #:row-sep? '(#t #f ... ...)
              #:col-sep? '(#t #f ... ...))
 ]

@section{Tables}

@defproc[(table->string
          [table (listof list?)]
          [#:->string to-string (pattern-list-of (procedure-arity-includes/c 1)) ~a]
          [#:border-style border-style border-style/c 'single]
          [#:framed? framed? boolean? #t]
          [#:row-sep? row-sep? (pattern-list-of boolean?) #t]
          [#:col-sep? col-sep? (pattern-list-of boolean?) #t]
          [#:align align
           (pattern-list-of (or/c 'left 'center 'right))
           'left]
          [#:row-align row-align
           (pattern-list-of (or/c 'top 'center 'bottom))
           'top])
         string?]{
 Accepts a table specified as a list of lists, and returns a string
 representing the table. The lists must all be of the same lengths.

 The @racket[to-string] procedure is used to convert cell values to
 strings, or a pattern-list of such procedures.

 The @racket[border-style] specifies the style of lines to be used
 in drawing the table.

 When @racket[framed?] is @racket[#true], a frame is drawn around the
 outside of the table.

 The @racket[row-sep?] and @racket[col-sep?] arguments
 specify whether separators are added between rows or columns.

 The @racket[align] specification indicates how the contents of the
 cells are to be aligned within their cells. A single-symbol specification
 applies to all cells, or a list of symbols of the same length as the
 rows can be applied in order to specify the alignment of each column
 independently. When @racket[align] is a list, it is trimmed to the length
 of the columns if it is too long, or the last element of the list is used
 for the remaining columns if it is too short.

 The @racket[row-align] specification indicates how the contents of the cells
 are aligned in a row, when cells are strings with multiple lines.

 The @racket[to-string], @racket[align] and @racket[row-align], @racket[row-sep?] and
 @racket[col-sep?] arguments accept pattern lists.
}

@defproc[(simple-table->string
          [table (listof list?)]
          [#:->string to-string (pattern-list-of (procedure-arity-includes/c 1)) ~a]
          [#:border-style border-style border-style/c 'single]
          [#:framed? framed? boolean? #f]
          [#:row-sep? row-sep? (pattern-list-of boolean?) #f]
          [#:col-sep? col-sep? (pattern-list-of boolean?) #f]
          [#:align align
           (pattern-list-of (or/c 'left 'center 'right))
           'left]
          [#:row-align row-align
           (pattern-list-of (or/c 'top 'center 'bottom))
           'top])
         string?]{
  Like @racket[table->string], but with different default arguments to output a minimalistic table.
}
@examples[#:eval my-eval
          (displayln
           (simple-table->string
            #:align '(left right)
            '((a b c d e f gggg h)
              (123 456 77 54 1  5646547987 41 1)
              (111 22 3333 44 5 6 7 8888))))]

@defform[(print-table args ...)]{
Shorthand form for @racket[(displayln (table->string args ...))].
Takes the same arguments as @racket[table->string].
}

@defform[(print-simple-table args ...)]{
Shorthand form for @racket[(displayln (simple-table->string args ...))].
Takes the same arguments as @racket[simple-table->string].
}

@; I tried this, but doesn't format well. #,@ doesn't work (because of @-reader?)
@; #,named-border-styles
@defthing[border-style/c contract?
          #:value
          (or/c
           'empty 'latex 'space 'space-single 'single 'rounded 'double 'heavy
           border-style1/c
           border-style2/c
           border-style-frame/c
           )]{
Border style contract.
The list element is for custom border styles.
See the example at the top of this document.
}

@defthing[border-style1/c contract?
          #:value
          (list/c char? (code:comment "row sep")
                  (list/c string? string? string?) (code:comment "text line")
                  (list/c string? string? string?) (code:comment "top line")
                  (list/c string? string? string?) (code:comment "middle line")
                  (list/c string? string? string?) (code:comment "bottom line"))]{
The old border style. Obsolete but kept for backward compatibility.
See @racket[border-style2/c] instead.
Note that, compared to @racket[border-style2/c],
the first an second lists are in reverse order,
the row separator is the same for all lines,
and the space filler is always @racket[" "].
}

@defthing[border-style2/c contract?
          #:value
          (list/c (list/c string? string? string? string?) (code:comment "top line")
                  (list/c string? string? string? string?) (code:comment "text line")
                  (list/c string? string? string? string?) (code:comment "middle line")
                  (list/c string? string? string? string?) (code:comment "bottom line"))]{
 Each string specifies one of the elements of the frame of the table.
 The strings can be of arbitrary length
 @examples[#:eval my-eval
           #:label #f
           (print-table
            '((_ _ ____ _)
              (_ _ _ _)
              (__ "_\n__" _ _))
            #:border-style
            '(("╭" "^" "┬" "╮")
              ("{" "." "│" "}")
              ("├" "─" "+" "┤")
              ("╰" "v" "┴" "╯")))]
 The element @racket["."] is a space filler.
 Note that each element can be a multi-character string rather than a single char.
 See also @racket[border-style-frame/c].
}

@defthing[border-style-frame/c contract?
          #:value
          (list/c (string-length=/c 5) (code:comment "top line")
                  (string-length=/c 5) (code:comment "text line")
                  (string-length=/c 5) (code:comment "middle line")
                  (string-length=/c 5) (code:comment "bottom line"))]{
 A simplification of @racket[border-style2/c] where each element of the frame is a single
 character, so they can all be specified in a single string per line.
 @examples[#:eval my-eval
           #:label #f
           (print-table '((abc abc abc)
                (abcdef ab abcdef)
                (a abcdef abc))
              #:border-style
              '("╭─┬╮"
                "│.││"
                "├─┼┤"
                "╰─┴╯")
              #:align '(center))]
Note that the @racket["."] is the space filler.}


@defproc[((string-length=/c [n integer?]) [x any/c]) boolean?]{
Returns @racket[#true] if @racket[x] is a string of length @racket[n],
 @racket[#false] otherwise.}

@section{Utilities}

@defmodule[text-table/utils]{Utilities used to build text tables.}

@subsection{Lists}

@defproc[((pattern-list-of [pred? (procedure-arity-includes/c 1)]) [x any/c]) boolean?]{
 Returns @racket[#true] if either @racket[x] is not a list and @racket[(pred? x)] is @racket[#true],
 or @racket[x] is a list @racket[(head ... dots ... tail ...)]
 satisfying  @racket[(andmap pred? (head ... tail ...))]
 and @racket[(dots ...)] is a list of @racket['...] not longer than @racket[(head ...)].
}

@defproc[(pattern-list->list [pat (pattern-list-of any/c)]
                             [#:truncate-ok? truncate-ok? any/c #f]
                             [result-length exact-nonnegative-integer?])
         list?]{
@examples[#:eval my-eval
          (pattern-list->list 'a 3)
          (pattern-list->list '(a) 3)
          (pattern-list->list '(a b) 5)
          (pattern-list->list '(a b ...) 5)
          (pattern-list->list '(a b c ... ...) 10)
          (pattern-list->list '(a b c d ... ... ... e f) 10)
          (eval:error (pattern-list->list '(a b c d ... ... ... e f) 2))
          (pattern-list->list '(a b c d ... ... ... e f) 2 #:truncate-ok? #t)]
}

@defproc[(transpose [l (listof list?)]) (listof list?)]{
 Returns a new list where the columns and rows of @racket[l] are swapped.
@examples[#:eval my-eval
           (transpose '((a b c) (1 2 3)))]
}


@defproc[(group-by-lengths [l list?] [lengths (listof exact-nonnegative-integer?)])
         (listof list?)]{
 Returns a list with the same elements as @racket[l] but grouped in sublists
 of lengths given by @racket[lengths].
@examples[#:eval my-eval
           (group-by-lengths '(a b c d e f g)
                             '(1 0 2 3 0 1))]
}

@;@defproc[(apply/2d-list-as-list [proc procedure?] [ll (listof list?)] [args any/c] ...)
@;         (listof (list))]{TODO}

@subsection{Strings}

@defproc[(string-repeat [str string?] [len exact-nonnegative-integer?]) string?]{
Returns a string of length @racket[len] by repeating @racket[str].
 @examples[#:eval my-eval
           (string-repeat "abc" 5)
           (string-repeat "abc" 2)]}

@defproc[(~r*  [#:sign sign
                       (or/c #f '+ '++ 'parens
                             (let ([ind (or/c string? (list/c string? string?))])
                               (list/c ind ind ind)))
                       #f]
               [#:base base
                       (or/c (integer-in 2 36) (list/c 'up (integer-in 2 36)))
                       10]
               [#:precision precision
                            (or/c exact-nonnegative-integer?
                                  (list/c '= exact-nonnegative-integer?)) 
                            6]
               [#:notation notation
                           (or/c 'positional 'exponential
                                 (-> rational? (or/c 'positional 'exponential)))
                           'positional]
               [#:format-exponent format-exponent
                (or/c #f string? (-> exact-integer? string?))
                #f]
               [#:min-width min-width exact-positive-integer? 1]
               [#:pad-string pad-string non-empty-string? " "]
               [#:groups groups (non-empty-listof exact-positive-integer?) '(3)]
               [#:group-sep group-sep string? ""]
               [#:decimal-sep decimal-sep string? "."])
         (any/c . -> . string?)]{
Like @racket[~r] but curried, and also accepts non-rationals,
 which are printed with @racket[~a] instead.
 @examples[#:eval my-eval
           (print-table
            #:->string (list ~a (code:comment "1")
                             (~r*) (code:comment "2")
                             (~r* #:notation 'exponential) (code:comment "3")
                             (~r* #:precision '(= 2)) (code:comment "4 (good)")
                             (~r* #:notation 'exponential #:precision '(= 2)) (code:comment "5 (good)")
                             (~r* #:min-width 10 #:pad-string ".")) (code:comment "6")
            #:align '(right ... )
            #:row-sep? '(#f #t #f ...)
            (cons
             '("1" "2" "3" "4 (good)" "5 (good)" "6") 
             (transpose
             (make-list 6 `(header 1111.11 22.222 33.33e5 4.44e12 ,(sqrt 2))))))]
}
@;'("1" "2" "3" "4👍\u200B" "5👍\u200B" "6")
@; cheat because 👍 is double-width. \u200B is a zero-width space

