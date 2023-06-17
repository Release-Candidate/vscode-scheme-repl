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


+inf.0
-inF.0
+NAN.0
-nan.0
+nan.0i
1-nan.0i
3+inf.0i
3-INF.0i
#o0156364
#X#e-ff
#E#xff
#b-10110e100
#b#e-10110e100
#2R#e10110
#e#20rG43358
#B1101/101
#o0156364/725
#X#e-ff/f65
#E#xff/10
#e#b-10110/1010f100
#2R#e10110
#e#20rG43358/6456A
-125/125
#o0156364.
#E#xff.
#e#b-10110.e100
#2R#e10110.
#e#20rG43358.
#e1.
#B1101/101
#o0156364/725
#X#e-ff/f65
#E#xff/10
#e#b-10110/1010f100
#2R#e10110
#e#20rG43358/6456A
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
#X-ff.
#i#20rG43358
#20r#iG43358
#20rG433.58
#20rG433.
#20r.58
#B#i1101/101
#o#I0156364/725
#X#i-ff/f65
#I#xff/10
#i#b-10110/1010f100
#2R#i10110
#i-125/125
#i#20rG43358/6456A

3+i

#;

(2 (1 hugo sfd #{gdf 456}) "adsds")

(waiter-prompt-string "λ>")
'[1 ("asd") "as\"d"  asdasd () ]

;; Code formatting of documentation
;;(with-output-to-string thunk)

#vu8(255 0 255)

#5vu8(1 2 3 4)

'#5("1" #(1 2 3 5) "a" (1 2) ;sdfdsf
  ; fdfdsf
 5)

#10vfx(1 2 3)
#vfx(5 6 78 7)

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
