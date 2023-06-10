/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     fileParseErrors.ts
 * Date:     10.Jun.2023
 *
 * =============================================================================
 * Outputs of source file parse errors.
 */

import { Output } from "../../src/helpers";

export const excNotBound: Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

(with-output-to-string thunk)

>
`,
    stderr: `Exception: variable thunk is not bound`,
};

export const excMissingParen: Output = {
    stdout: `(import (test test-lib))

(machine-type)

> `,
    stderr: `Exception in read: unexpected end-of-file reading list at line 15, char 1 of test/test.ss`,
};

export const excWrongLib: Output = {
    stdout: `(import (test test-lib))

(machine-type)

(waiter-prompt-string "λ>")

#vu8(255 0 255)

>
`,
    stderr: `Exception in read: #<n>vfx(...) fxvector syntax is not allowed in #!r6rs mode at line 24, char 2 of test/test.ss`,
};

export const excWrongNumArg: Output = {
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

export const invalidSyntax: Output = {
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
