/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     fileParseErrors.ts
 * Date:     10.Jun.2023
 *
 * =============================================================================
 * Outputs of source file parse errors, with the expected results.
 */

import * as h from "../../src/helpers";

/* eslint-disable no-magic-numbers */

export const excNotBound: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

(with-output-to-string thunk)

>
`,
    stderr: `Exception: variable thunk is not bound`,
};

export const excNotBoundText = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
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
(with-output-to-string thunk)

#vu8(255 0 255)

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

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

(define url-list-of-forms "https://cisco.github.io/ChezScheme/csug9.5/summary.html")`;

export const excNotBoundExp = h.rangeFromPositions([17, 23], [17, 28]);

export const excMissingParen: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

> `,
    stderr: `Exception in read: unexpected end-of-file reading list at line 15, char 1 of test/test.ss`,
};

export const excMissingParenText = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
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

(waiter-prompt-string "λ>"

;; Code formatting of documentation
;;(with-output-to-string thunk)

#vu8(255 0 255)

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

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
`;

export const excMissingParenExp = h.rangeFromPositions([14, 0], [14, 0]);

export const excWrongLib: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

#vu8(255 0 255)

>
`,
    stderr: `Exception in read: #<n>vfx(...) fxvector syntax is not allowed in #!r6rs mode at line 24, char 2 of test/test.ss`,
};

export const excWrongLibText = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
;; SPDX-License-Identifier: MIT
;;
;; Project:  vscode-scheme-repl
;; File:     test.ss
;; Date:     15.May.2023
;;
;; ==============================================================================
;;#!chezscheme
#!r6rs


(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

;; Code formatting of documentation
;;(with-output-to-string thunk)

#vu8(255 0 255)

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

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
`;

export const excWrongLibExp = h.rangeFromPositions([23, 1], [23, 1]);

export const excWrongNumArg: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

#vu8(255 0 255)

\`#vfx(1 2 3 3 3 3 3 3 3 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

(define (f x) (* 8 x))

(f)

>
`,
    stderr: `Exception: incorrect number of arguments 0 to #<procedure f at test.ss:635>`,
};

export const excWrongNumArgText = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
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

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

;; Problem: documentation parts missing. Because the second part of the
;; documentation has it's own link name.
;; (define-ftype (ftype-name ftype ) ...)

(define (f x)
  (* 8 x))

(f )

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
`;

export const excWrongNumArgExp = h.rangeFromPositions([34, 0], [34, 4]);

export const excWrongNumArg2: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

#vu8(255 0 255)

\`#vfx(1 2 3 3 3 3 3 3 3 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

(define (f x) (* 8 x))

(f 6)

(define-syntax incr!
  (syntax-rules () [(incr! x) (set! x (+ x 1))]))

(define y 5)

(incr! y)

y

(define (hugo x) (+ x x))

'(1 2 3 4)

(f 6 9)

> >

`,
    stderr: `Exception: incorrect number of arguments 2 to #<procedure f at test.ss:635>`,
};

export const excWrongNumArg2Text = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
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

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

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

(f 6
  9)

(environment-symbols (interaction-environment))
(get-identifiers "get-i")

(define url-list-of-forms "https://cisco.github.io/ChezScheme/csug9.5/summary.html")
`;

export const excWrongNumArg2Exp = h.rangeFromPositions([53, 0], [54, 4]);

export const excInvalidSyntax: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

#vu8(255 0 255)

\`#vfx(1 2 3 3 3 3 3 3 3 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

(define-ftype (ftype-name ftype) ...)

>
`,
    stderr: `Exception: invalid syntax (define-ftype (ftype-name ftype) ...) at line 30, char 1 of test/test.ss`,
};

export const excInvalidSyntaxText = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
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

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

;; Problem: documentation parts missing. Because the second part of the
;; documentation has it's own link name.
(define-ftype (ftype-name ftype ) ...)

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
`;

export const excInvalidSyntaxExp = h.rangeFromPositions([29, 0], [29, 0]);

export const excApplyNonProc: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

#vu8(255 0 255)

\`#vfx(1 2 3 3 3 3 3 3 3 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

(define (f x) (* 8 x))

(f 6)

(define-syntax incr!
  (syntax-rules () [(incr! x) (set! x (+ x 1))]))

(define y 5)

(incr! y)

y

(y)

> >
`,
    stderr: "Exception: attempt to apply non-procedure 6",
};

export const excApplyNonProcText = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
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

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

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

(y)

(define (hugo x)
  (+ x x))

'(1 2 3 4)

(f 6)

(environment-symbols (interaction-environment))
(get-identifiers "get-i")

(define url-list-of-forms "https://cisco.github.io/ChezScheme/csug9.5/summary.html")
`;

export const excApplyNonProcExp = h.rangeFromPositions([48, 0], [48, 3]);

export const excNotAnEnvironment: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

#vu8(255 0 255)

\`#vfx(1 2 3 3 3 3 3 3 3 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

(define (f x) (* 8 x))

(f 6)

(define-syntax incr!
  (syntax-rules () [(incr! x) (set! x (+ x 1))]))

(define y 5)

(incr! y)

y

(define (hugo x) (+ x x))

'(1 2 3 4)

(f 6)

(environment-symbols (interaction-environment))

(get-identifiers "get-i")

(define url-list-of-forms
  "https://cisco.github.io/ChezScheme/csug9.5/summary.html")

(environment-symbols 5)

> >
`,
    stderr: `Exception in environment-symbols: 5 is not an environment`,
};

export const excNotAnEnvironmentText = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
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

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

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

(environment-symbols 5)
`;

export const excNotAnEnvironmentExp = h.rangeFromPositions([60, 1], [60, 22]);

export const excNotAnEnvironment2: h.Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

#vu8(255 0 255)

\`#vfx(1 2 3 3 3 3 3 3 3 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

(define (f x) (* 8 x))

(f 6)

(define-syntax incr!
  (syntax-rules () [(incr! x) (set! x (+ x 1))]))

(define y 5)

(incr! y)

y

(define (hugo x) (+ x x))

'(1 2 3 4)

(environment-symbols hugo)

> >

`,
    stderr: `Exception in environment-symbols: #<procedure hugo at test.ss:777> is not an environment`,
};

export const excNotAnEnvironment2Text = `;; SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
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

\`#10vfx(1 2 3)

(+ 5 6)

\`#{g0 bcsfg5eq4e9b3h9o-a}

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

(environment-symbols hugo)

(environment-symbols (interaction-environment))
(get-identifiers "get-i")

(define url-list-of-forms "https://cisco.github.io/ChezScheme/csug9.5/summary.html")

(environment-symbols)

`;

export const excNotAnEnvironment2Exp = h.rangeFromPositions([52, 0], [52, 26]);
