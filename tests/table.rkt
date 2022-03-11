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

(check-equal?
 (table->string
  '((a b c d e f gggg h)
    (123 456 77 54 "a\nbbbb\nc" 5646547987 41 1)
    (111 22 3333 44 5 6 7 8888))
  #:border-style
  'latex
  #:framed?
  #t
  #:row-sep?
  '(#t ...)
  #:align
  '(left center right center))
 "\\begin{tabular}{|l|c|r|c|c|c|c|c|}\n\\hline\na   &  b  &    c & d  &  e   &     f      & gggg &  h   \\\\\n\\hline\n123 & 456 &   77 & 54 &  a   & 5646547987 &  41  &  1   \\\\\n    &     &      &    & bbbb &            &      &      \\\\\n    &     &      &    &  c   &            &      &      \\\\\n\\hline\n111 & 22  & 3333 & 44 &  5   &     6      &  7   & 8888 \\\\\n\\hline\n\\end{tabular}")

(check-equal?
 (table->string
  (for/list ((i 5)) (for/list ((j 6)) (* (+ i 1) (+ j 1))))
  #:align
  'right
  #:border-style
  'latex)
 "\\begin{tabular}{|r|r|r|r|r|r|}\n\\hline\n1 &  2 &  3 &  4 &  5 &  6 \\\\\n\\hline\n2 &  4 &  6 &  8 & 10 & 12 \\\\\n\\hline\n3 &  6 &  9 & 12 & 15 & 18 \\\\\n\\hline\n4 &  8 & 12 & 16 & 20 & 24 \\\\\n\\hline\n5 & 10 & 15 & 20 & 25 & 30 \\\\\n\\hline\n\\end{tabular}")

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
(check-equal?
 (simple-table->string #:border-style 'empty '((a bb c d) (1 2 33 44)))
 "abbc d \n12 3344")

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

(check-equal?
 (table->string (for/list ((i 6)) (for/list ((j 5)) (* i j))) #:row-sep? '(#t #f ... ...))
 "┌─┬─┬──┬──┬──┐\n│0│0│0 │0 │0 │\n├─┼─┼──┼──┼──┤\n│0│1│2 │3 │4 │\n│0│2│4 │6 │8 │\n├─┼─┼──┼──┼──┤\n│0│3│6 │9 │12│\n│0│4│8 │12│16│\n├─┼─┼──┼──┼──┤\n│0│5│10│15│20│\n└─┴─┴──┴──┴──┘")

(check-equal?
 (table->string
  (for/list ((i 5)) (for/list ((j 6)) (* (+ i 1) (+ j 1))))
  #:align
  'right
  #:framed?
  #f
  #:row-sep?
  '(#t #f)
  #:col-sep?
  '(#t #f)
  #:border-style
  'single)
 "1│ 2  3  4  5  6\n─┼──────────────\n2│ 4  6  8 10 12\n3│ 6  9 12 15 18\n4│ 8 12 16 20 24\n5│10 15 20 25 30")

(check-equal?
 (table->string
  (for/list ((i 5)) (for/list ((j 6)) (* (+ i 1) (+ j 1))))
  #:align
  'right
  #:framed?
  #f
  #:row-sep?
  '(#t #f)
  #:col-sep?
  '(#t #f)
  #:border-style
  'latex)
 "\\begin{tabular}{r|rrrrr}\n1 &  2 &  3 &  4 &  5 &  6 \\\\\n\\hline\n2 &  4 &  6 &  8 & 10 & 12 \\\\\n3 &  6 &  9 & 12 & 15 & 18 \\\\\n4 &  8 & 12 & 16 & 20 & 24 \\\\\n5 & 10 & 15 & 20 & 25 & 30 \\\\\n\\end{tabular}")


(check-equal? (table->string #:->string (λ _ "") '((1))) "┌┐\n││\n└┘")
(check-equal?
 (table->string #:->string (list (λ (x) "a") (λ (x) "b")) '((1 2)))
 "┌─┬─┐\n│a│b│\n└─┴─┘")

(check-equal?
 (table->string #:->string (list (λ (x) "a") (λ (x) "b") '... (λ (x) "c")) '((1 2 3 4)))
 "┌─┬─┬─┬─┐\n│a│b│b│c│\n└─┴─┴─┴─┘")
