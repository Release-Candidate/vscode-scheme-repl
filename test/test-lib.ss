;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
;; SPDX-License-Identifier: MIT
;;
;; Project:  vscode-scheme-repl
;; File:     test_lib.ss
;; Date:     09.Jun.2023
;;
;; ==============================================================================
(library (test test-lib)
  (export get-identifiers)
  (import (chezscheme))
  (define (get-identifiers s)

    (filter
      (lambda (x)
        (cond
          [(symbol? x) (equal? s (substring (symbol->string x) 0 (string-length s)))]
          [else #f]))
      (apropos-list s (interaction-environment))))
)
