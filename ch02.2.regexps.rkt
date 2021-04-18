#lang racket

(require (only-in srfi/1 lset=))

(require syntax/parse/define)
(define-simple-macro (qc  body ...) (module+ test body ...))
(define-simple-macro (qce body ...) (qc (check-equal? body ...)))

(qc (require rackunit))

(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

(qce (r:seq "a" "b" "c") "\\(abc\\)")

(define chars-needing-quoting '(#\. #\[ #\^ #\$ #\*))

(define (r:quote string)
  (r:seq (list->string
          (append-map (λ (char)
                        (if (memv char chars-needing-quoting)
                            (list #\\ char)
                            (list char)))
                      (string->list string)))))

(qce (r:quote "abc") "\\(abc\\)")
(qce (r:quote "^abc$") "\\(\\^abc\\$\\)")

(define (r:alt . exprs)
  (if (pair? exprs)
      (apply r:seq (cons (car exprs)
                         (append-map (λ (expr)
                                       (list "\\|" expr))
                                     (cdr exprs))))
      (r:seq)))

(qce (r:alt "a" "b" "c") "\\(a\\|b\\|c\\)")

(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond [(not max) (list expr "*")]
                       [(= max min) '()]
                       (else (make-list (- max min) (r:alt expr "")))))))

(qce (r:repeat 0 1 (r:seq "abc"))           "\\(\\(\\(abc\\)\\|\\)\\)")
(qce (r:repeat 1 2 (r:seq "abc"))  "\\(\\(abc\\)\\(\\(abc\\)\\|\\)\\)") ; horrific
(qce (r:repeat 0 #f (r:seq "abc")) "\\(\\(abc\\)*\\)")
(qce (r:repeat 1 1 (r:seq "abc"))  "\\(\\(abc\\)\\)")

(define (quote-bracketed-contents cs)
  (define chars-needing-quoting-in-brackets '(#\] #\^ #\-))
  (define (optional c) (if (memv c cs) (list c) '()))
  (append (optional #\])
          (filter-not (λ (c) (memv c chars-needing-quoting-in-brackets))
                  cs)
          (optional #\^)
          (optional #\-)))

(qce '(#\a #\b #\c) (quote-bracketed-contents (string->list "abc")))
(qce '(#\a #\c #\-) (quote-bracketed-contents (string->list "a-c")))

(define (r:char-from string)
  (case (string-length string)
    [(0) (r:seq)]
    [(1) (r:quote string)]
    [else (bracket string (λ (cs)
                            (if (lset= eqv? '(#\- #\^) cs)
                                '(#\- #\^)
                                (quote-bracketed-contents cs))))]))

(qce "\\(\\)" (r:char-from ""))
(qce "\\(a\\)" (r:char-from "a"))
(qce "[ab]" (r:char-from "ab"))
(qce "[abc]" (r:char-from "abc"))
(qce "[ac-]" (r:char-from "a-c"))

(define (r:char-not-from string)
  (bracket string (λ (cs) (cons #\^ (quote-bracketed-contents cs)))))

(qce "[^abc]" (r:char-not-from "abc"))

(define (bracket string fn)
  (list->string (append '(#\[)
                        (fn (string->list string))
                        '(#\]))))

(qce (bracket "abc" (curry map char-upcase)) "[ABC]")

;;; Exercise 2.6: implement r:* and r:+

(define (r:* expr) (r:repeat 0 #f expr))
(define (r:+ expr) (r:repeat 1 #f expr))

(qce (r:* (r:seq "abc")) "\\(\\(abc\\)*\\)")
(qce (r:+ (r:seq "abc")) "\\(\\(abc\\)\\(abc\\)*\\)") ; FIX: don't like

;;; DONE

(qc (displayln 'done))
