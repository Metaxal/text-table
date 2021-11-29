#lang scribble/manual
@(require racket/sandbox
          scribble/example
          (for-label text-table
                     racket/contract
                     racket/base
                     racket/format
                     racket/string))

@title{text-table}
@author{Laurent Orseau}

@(define my-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base '(require text-table))))

@defmodule[text-table]{
 A simple package to display utf-8 textual tables.}

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
 ]

@defproc[(table->string
          [table (listof list?)]
          [#:->string to-string procedure? ~a]
          [#:border-style border-style border-style/c 'single]
          [#:framed? framed? boolean? #t]
          [#:row-sep? row-sep? boolean? #t]
          [#:align align
           (or/c (listof (or/c 'left 'center 'right))
                 (or/c 'left 'center 'right))
           'left])
         string?]{
 Accepts a table specified as a list of lists, and returns a string
 representing the table. The lists must all be of the same length.

 The @racket[to-string] procedure is used to convert cell values to
 strings.

 The @racket[border-style] specifies the style of lines to be used
 in drawing the table.

 When @racket[framed?] is @racket[#true], a frame is drawn around the
 outside of the table.

 When @racket[row-sep?] is false, no separators are drawn between the
 rows.

 The @racket[align] specification indicates how the contents of the
 cells are to be aligned within their cells. A single-symbol specification
 applies to all cells, or a list of symbols of the same length as the
 rows can be applied in order to specify the alignment of each column
 independently. When @racket[align] is a list, it is trimmed to the length
 of the columns if it is too long, or the last element of the list is used
 for the remaining columns if it is too short.
}

@defproc[(simple-table->string
          [table (listof list?)]
          [#:->string to-string procedure? ~a]
          [#:border-style border-style border-style/c 'space]
          [#:framed? framed? boolean? #f]
          [#:row-sep? row-sep? boolean? #f]
          [#:align align
           (or/c (listof (or/c 'left 'center 'right))
                 (or/c 'left 'center 'right))
           'left])
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

@defthing[border-style/c contract?
          #:value
          (or/c
           'latex 'space 'space-single 'single 'rounded 'double 'heavy
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
Returns @racket[#true] if @racket[x] is a string of length @racket[n].}



