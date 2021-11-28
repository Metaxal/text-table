#lang racket
(require text-table
         rackunit)

(check-exn exn:fail? (λ () (table->string '())))

(check-exn exn:fail? (λ () (table->string '(a))))

(check-exn exn:fail? (λ () (table->string '([a b] [c]))))

(check-equal? (table->string '([a "b\nbbb"] [c 3]))
              (string-join '("┌─┬───┐"
                             "│a│b  │"
                             "│ │bbb│"
                             "├─┼───┤"
                             "│c│3  │"
                             "└─┴───┘")
                           "\n"))

(check-equal? (table->string '((a b c d e f gggg h)
                               (123 456 77 54 "a\nbbbb\nc" 5646547987 41 1)
                               (111 22 3333 44 5 6 7 8888))
                             #:border-style 'latex
                             #:framed? #t
                             #:row-sep? #t
                             #:align '(left center right center)) ; fewer alings than columns
              "\
\\begin{tabular}{lcrccccc}
a   &  b  &    c & d  &  e   &     f      & gggg &  h   \\\\
\\hline
123 & 456 &   77 & 54 &  a   & 5646547987 &  41  &  1   \\\\
    &     &      &    & bbbb &            &      &      \\\\
    &     &      &    &  c   &            &      &      \\\\
\\hline
111 & 22  & 3333 & 44 &  5   &     6      &  7   & 8888 \\\\
\\end{tabular}")

(check-equal?
 (table->string '((a b c d e f gggg h)
                  (123 456 77 54 "a\nbbb\nc"  123456 41 1)
                  (111 22 3333 44 5 6 7 8888))
                #:border-style
                '("<-+>"
                  "(.│)"
                  "[-+]"
                  "{-+}")
                #:framed? #t
                #:row-sep? #t
                #:align '(left center right center))
 "\
<---+---+----+--+---+------+----+---->
(a..│.b.│...c│d.│.e.│..f...│gggg│.h..)
[---+---+----+--+---+------+----+----]
(123│456│..77│54│.a.│123456│.41.│.1..)
(...│...│....│..│bbb│......│....│....)
(...│...│....│..│.c.│......│....│....)
[---+---+----+--+---+------+----+----]
(111│22.│3333│44│.5.│..6...│.7..│8888)
{---+---+----+--+---+------+----+----}")
  

