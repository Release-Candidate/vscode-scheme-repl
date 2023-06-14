/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     parseError-test.ts
 * Date:     11.Jun.2023
 *
 * =============================================================================
 * Tests the parsing of error messages when trying to load a file into the REPL.
 */

/* eslint-disable max-statements */
/* eslint-disable max-lines-per-function */

import * as chai from "chai";
import * as e from "../src/evalREPL";
import * as fixture from "../test/fixtures/fileParseErrors";
import * as mocha from "mocha";
import { rangeFromPositions } from "../src/helpers";

/**
 * *****************************************************************************
 * Tests
 */
mocha.describe("Error parsing functions", () => {
    //==========================================================================
    mocha.describe("parseError", () => {
        mocha.it("Empty string", () => {
            chai.assert.deepEqual(
                e.parseError({ stderr: "", stdout: "" }, ""),
                rangeFromPositions([0, 0], [0, 0]),
                "Missing parenthesis returns start position == end position"
            );
        });
        mocha.it("Missing parenthesis", () => {
            chai.assert.deepEqual(
                e.parseError(
                    fixture.excMissingParen,
                    fixture.excMissingParenText
                ),
                fixture.excMissingParenExp,
                "Missing parenthesis returns start position == end position"
            );
        });
        mocha.it("Unbound variable", () => {
            chai.assert.deepEqual(
                e.parseError(fixture.excNotBound, fixture.excNotBoundText),
                fixture.excNotBoundExp,
                "Unbound variable -> line 17, 23 - 28"
            );
        });
        mocha.it("Invalid syntax", () => {
            chai.assert.deepEqual(
                e.parseError(
                    fixture.excInvalidSyntax,
                    fixture.excInvalidSyntaxText
                ),
                fixture.excInvalidSyntaxExp,
                "Invalid syntax -> line 30 col 1"
            );
        });
        mocha.it("R6RS instead of Chezscheme", () => {
            chai.assert.deepEqual(
                e.parseError(fixture.excWrongLib, fixture.excWrongLibText),
                fixture.excWrongLibExp,
                "R6RS instead of Chezscheme -> line 24 col 2"
            );
        });
        mocha.it("Wrong number of arguments", () => {
            chai.assert.deepEqual(
                e.parseError(
                    fixture.excWrongNumArg,
                    fixture.excWrongNumArgText
                ),
                fixture.excWrongNumArgExp,
                "Wrong number of arguments -> line 32 col 1"
            );
        });
        mocha.it("Wrong number of arguments 2", () => {
            chai.assert.deepEqual(
                e.parseError(
                    fixture.excWrongNumArg2,
                    fixture.excWrongNumArg2Text
                ),
                fixture.excWrongNumArg2Exp,
                "Wrong number of arguments 2 -> line 32 col 1"
            );
        });
        mocha.it("Apply non procedure", () => {
            chai.assert.deepEqual(
                e.parseError(
                    fixture.excApplyNonProc,
                    fixture.excApplyNonProcText
                ),
                fixture.excApplyNonProcExp,
                "Apply non procedure -> line 47 col 0-3"
            );
        });
        mocha.it("Not an environment", () => {
            chai.assert.deepEqual(
                e.parseError(
                    fixture.excNotAnEnvironment,
                    fixture.excNotAnEnvironmentText
                ),
                fixture.excNotAnEnvironmentExp,
                "Not an environment -> line 60 col 0-23"
            );
        });
        mocha.it("Not an environment 2", () => {
            chai.assert.deepEqual(
                e.parseError(
                    fixture.excNotAnEnvironment2,
                    fixture.excNotAnEnvironment2Text
                ),
                fixture.excNotAnEnvironment2Exp,
                "Not an environment 2 -> line 60 col 0-23"
            );
        });
    });
});
