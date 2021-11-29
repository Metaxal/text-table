text-table
==========
A simple package to display utf-8 textual tables.
Check out the [docs](https://docs.racket-lang.org/text-table/index.html).

To install:
```
raco pkg install text-table
```

See the example in the main submodule of the `main.rkt` file.
You can observe the results by running:
```
racket -l text-table
```

A minimalistic example:
```scheme
#lang racket
(require text-table)

(print-simple-table
 '((a b c d e f gggg h)
   (12  "a\nbcde" 77 54 1  5646547987 41 1)
   (111 222 3333 44 5 6 7 8888)))
```
Output:
```
a   b    c    d  e f          gggg h   
12  a    77   54 1 5646547987 41   1   
    bcde                               
111 222  3333 44 5 6          7    8888
```
A less minimalistic example:
```scheme
(print-table
 '((a b c d e f gggg h)
   (12  "a\nbcde" 77 54 1  5646547987 41 1)
   (111 222 3333 44 5 6 7 8888)))
```
```
┌───┬────┬────┬──┬─┬──────────┬────┬────┐
│a  │b   │c   │d │e│f         │gggg│h   │
├───┼────┼────┼──┼─┼──────────┼────┼────┤
│12 │a   │77  │54│1│5646547987│41  │1   │
│   │bcde│    │  │ │          │    │    │
├───┼────┼────┼──┼─┼──────────┼────┼────┤
│111│222 │3333│44│5│6         │7   │8888│
└───┴────┴────┴──┴─┴──────────┴────┴────┘
```
An example with some more bells and whistles:
```scheme
(print-table
 '((a b c d e f gggg h)
   (12  "a\nbcde" 77 54 1  5646547987 41 1)
   (111 222 3333 44 5 6 7 8888))
 #:border-style 'double
 #:framed? #f
 #:row-sep? #t
 #:align '(left center center center center center center right))
```
```
a  ║ b  ║ c  ║d ║e║    f     ║gggg║   h
═══╬════╬════╬══╬═╬══════════╬════╬════
12 ║ a  ║ 77 ║54║1║5646547987║ 41 ║   1
   ║bcde║    ║  ║ ║          ║    ║    
═══╬════╬════╬══╬═╬══════════╬════╬════
111║222 ║3333║44║5║    6     ║ 7  ║8888
```





