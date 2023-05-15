;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
;; SPDX-License-Identifier: MIT
;;
;; Project:  vscode-scheme-repl
;; File:     test.ss
;; Date:     15.May.2023
;;
;; ==============================================================================


(machine-type)

(+ 5 6)

(define (f x)
  (* 8 x))

(f 5)

(f 9)

(environment-symbols (interaction-environment))
(apropos-list "real?")

(define url-list-of-forms "https://cisco.github.io/ChezScheme/csug9.5/summary.html")
