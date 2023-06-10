;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
;; SPDX-License-Identifier: MIT
;;
;; Project:  vscode-scheme-repl
;; File:     test.ss
;; Date:     15.May.2023
;;
;; ==============================================================================
#!chezscheme

(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

;; Code formatting of documentation
;;(with-output-to-string thunk)

#vu8(255 0 255)

`#10vfx(1 2 3)

(+ 5 6)

`#{g0 bcsfg5eq4e9b3h9o-a}

;; Problem: documentation parts missing. Because the second part of the
;; documentation has it's own link name.
;; (define-ftype (ftype-name ftype ) ...)

(define (f x)
  (* 8 x))

(f 6)

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

(f 6)

(environment-symbols (interaction-environment))
(get-identifiers "get-i")

(define url-list-of-forms "https://cisco.github.io/ChezScheme/csug9.5/summary.html")
