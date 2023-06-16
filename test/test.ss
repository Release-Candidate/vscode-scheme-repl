;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
;; SPDX-License-Identifier: MIT
;;
;; Project:  vscode-scheme-repl
;; File:     test.ss
;; Date:     15.May.2023
;;
;; ==============================================================================
#!chezscheme

;;;;; Hi My name is
;;;; Bla
;;; Hallo
;; fasdf
; sdf
(import (test test-lib))

( quote 5)

(quasiquote (1.0 2.0 3.5))

(machine-type)

#\a
#\alarm
#\123
#\xff454

#trUE
#FalSE

#o0156364
#X#e-ff
#E#xff
#e#b-10110e100
#2R#e10110
#e#20rG43358

#B1101/101
#o0156364/725
#X#e-ff/f65
#E#xff/10
#e#b-10110/1010f100
#2R#e10110
#e#20rG43358
-125/125

#2r110.1010f100
#b110.f100
#b#i110
#i#2r.1010f100
#i#o0156364.
#O#i0156364.433547e453
#O#i0156364
#8R.433547e453
#i#d125.1555e15
125.
#10r.1555e15
#i2
#d#i2
#i#20rG43358
#20r#iG43358
#20rG433.58
#20rG433.
#20r.58

(waiter-prompt-string "λ>")

;; Code formatting of documentation
;;(with-output-to-string thunk)

#vu8(255 0 255)

`#10vfx(1 2 3)

(+ 5 6)

`#{g0 bcsfg5eq4e9b3h9o-a}
'#:g4566

'#&1568

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

'(1, 2, 3 ,@ 4)
'(unquote-splicing (1 2 3))

(environment-symbols (interaction-environment))
(get-identifiers "get-i")

(define url-list-of-forms "https://cisco.github.io/ChezScheme/csug9.5/summary.html")

#!eof
