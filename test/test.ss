;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
;; SPDX-License-Identifier: MIT
;;
;; Project:  vscode-scheme-repl
;; File:     test.ss
;; Date:     15.May.2023
;;
;; ==============================================================================

(machine-type)

;; Code formatting of documentation
;; (with-output-to-string thunk)

(+ 5 6)

;; Problem: documentation parts missing. Because the second part of the
;; documentation has it's own link name.
;; (define-ftype (ftype-name ftype ) ...)

(define (f x)
  (* 8 x))

(f 5)

(define-syntax incr!
  (syntax-rules ()
    ((incr! x)
     (set! x (+ x 1)))))

(define y 5)

(incr! y)

y

(define (hugo x)
  (+ x x))

'(1 2 3 4)

(f 9)

(environment-symbols (interaction-environment))
(apropos-list 'real?)

(define url-list-of-forms "https://cisco.github.io/ChezScheme/csug9.5/summary.html")
