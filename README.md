text-table
==========
A simple package to display utf-8 textual tables.

To install:
```
raco pkg install text-table
```

See the example in the main submodule of the `main.rkt` file.
You can observe the results by running:
```
racket -l text-table
```

Two examples:
```
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
```
This outputs:
```
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
```





