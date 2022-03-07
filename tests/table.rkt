#lang racket
(require text-table
         rackunit)

; to write/try test easily:
(define-syntax-rule (tbcheck expr)
    (let ([res expr])
      (displayln res)
      `(check-equal? expr ,res)))
; Ex:
#;(tbcheck (simple-table->string '((aaa bbb ccc)
                                   (1 2 3))))

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
; Check border-style1/c still works
(check-equal? (table->string '([a "b\nbbb"] [c 3])
                             #:border-style
                             '(#\─      ("│" "│" "│") ("┌" "┬" "┐") ("├" "┼" "┤") ("└" "┴" "┘"))
                             )
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

;; border-style-frame/c
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
  
(check-equal?
 (simple-table->string '((aaa bbb ccc)))
 "aaa bbb ccc")

(check-equal? (simple-table->string '((aaa bbb ccc) (1 2 3)))
              "aaa bbb ccc\n1   2   3  ")
(check-equal?
 (simple-table->string '((aaa bbb ccc "") (1 2 3 "")))
 "aaa bbb ccc \n1   2   3   ")

(check-equal? (simple-table->string '((""))) "")

(check-equal? (table->string '(("" "") ("" ""))) "┌┬┐\n│││\n├┼┤\n│││\n└┴┘")
(check-equal? (table->string '((b "") ("" a))) "┌─┬─┐\n│b│ │\n├─┼─┤\n│ │a│\n└─┴─┘")

;;; Check alignment

(check-equal?
 (table->string '((aaa bbb cccc dddd) (1 22 3 33)) #:align 'right)
 "┌───┬───┬────┬────┐\n│aaa│bbb│cccc│dddd│\n├───┼───┼────┼────┤\n│  1│ 22│   3│  33│\n└───┴───┴────┴────┘")
(check-equal?
 (table->string '((aaa bbb cccc dddd) (1 22 3 33)) #:align 'left)
 "┌───┬───┬────┬────┐\n│aaa│bbb│cccc│dddd│\n├───┼───┼────┼────┤\n│1  │22 │3   │33  │\n└───┴───┴────┴────┘")

(check-equal?
 (table->string '((aaa bbb cccc dddd) (1 22 3 33)) #:align 'center)
 "┌───┬───┬────┬────┐\n│aaa│bbb│cccc│dddd│\n├───┼───┼────┼────┤\n│ 1 │22 │ 3  │ 33 │\n└───┴───┴────┴────┘")

(check-equal?
 (table->string '((aaa bbb cccc dddd) (1 22 3 33)) #:align '(left right))
 "┌───┬───┬────┬────┐\n│aaa│bbb│cccc│dddd│\n├───┼───┼────┼────┤\n│1  │ 22│   3│  33│\n└───┴───┴────┴────┘")

(check-equal?
 (table->string '((aaa bbb cccc dddd) (1 22 3 33)) #:align '(left ... right))
 "┌───┬───┬────┬────┐\n│aaa│bbb│cccc│dddd│\n├───┼───┼────┼────┤\n│1  │22 │3   │  33│\n└───┴───┴────┴────┘")

(check-equal?
 (table->string '((aaa bbbb cccc dddd) (1 22 3 33)) #:align '(left center ... right))
 "┌───┬────┬────┬────┐\n│aaa│bbbb│cccc│dddd│\n├───┼────┼────┼────┤\n│1  │ 22 │ 3  │  33│\n└───┴────┴────┴────┘")

(check-equal?
 (table->string
  '((aaa bbbb cccc dddd eeee) (1 22 3 33 4))
  #:align
  '(left center ... ... right))
 "┌───┬────┬────┬────┬────┐\n│aaa│bbbb│cccc│dddd│eeee│\n├───┼────┼────┼────┼────┤\n│1  │ 22 │3   │ 33 │   4│\n└───┴────┴────┴────┴────┘")
