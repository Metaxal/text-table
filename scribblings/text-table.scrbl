#lang scribble/manual
@require[@for-label[text-table
                    racket/base]]

@title{text-table}
@author{orseau}

@defmodule[text-table]{
 A simple package to display utf-8 textual tables.}

To install:

@verbatim{raco pkg install text-table}

See the example in the main submodule of the @filepath{main.rkt} file.
You can observe the results by running:


@verbatim{racket -l text-table}


Two examples:

@codeblock|{
#lang racket
(require text-table)

;; Minimalistic example:
(displayln
 (table->string
  '((a b c d e f gggg h)
    (123 456 77 54 1  5646547987 41 1)
    (111 22 3333 44 5 6 7 8888))))

;; With more bells and whistles
(displayln
 (table->string
  '((a b c d e f gggg h)
    (123 456 77 54 1  5646547987 41 1)
    (111 22 3333 44 5 6 7 8888))
  #:border-style 'double
  #:framed? #f
  #:row-sep? #t
  #:align '(left center center center center center center right)))
}|

This outputs:

@verbatim{
┌───┬───┬────┬──┬─┬──────────┬────┬────┐
│a  │b  │c   │d │e│f         │gggg│h   │
├───┼───┼────┼──┼─┼──────────┼────┼────┤
│123│456│77  │54│1│5646547987│41  │1   │
├───┼───┼────┼──┼─┼──────────┼────┼────┤
│111│22 │3333│44│5│6         │7   │8888│
└───┴───┴────┴──┴─┴──────────┴────┴────┘
a  ║ b ║ c  ║d ║e║    f     ║gggg║   h
═══╬═══╬════╬══╬═╬══════════╬════╬════
123║456║ 77 ║54║1║5646547987║ 41 ║   1
═══╬═══╬════╬══╬═╬══════════╬════╬════
111║22 ║3333║44║5║    6     ║ 7  ║8888
}


@defproc[(table->string
          [table (listof list?)]
          [#:->string to-string procedure? ~a]
          [#:border-style border-style
           (or/c 'single 'space 'space-single 'rounded 'double 'latex)
           'single]
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
 independently.}

@defform[(print-table args ...)]{
Shorthand form for @racket[(displayln (table->string args ...))].
Takes the same arguments as @racket[table->string].
}