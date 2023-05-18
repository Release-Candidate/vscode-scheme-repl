/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     sexps-test.ts
 * Date:     17.May.2023
 *
 * ==============================================================================
 * Tests for the functions of the `sexps.ts` module.
 */

/* eslint-disable max-statements */
/* eslint-disable max-lines-per-function */

import * as chai from "chai";
import * as mocha from "mocha";
import * as s from "../src/sexps";

/**
 * *****************************************************************************
 * Tests
 */
mocha.describe("Sexp parsing functions", () => {
    //==========================================================================
    mocha.describe("getSexpToLeft", () => {
        mocha.it("Empty string ''", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft(""),
                "",
                "Empty string does not change"
            );
        });
        mocha.it("Just some text 'dsghjkl sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl sdfgh"),
                "sdfgh",
                "String 'dsghjkl sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl,sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl,sdfgh"),
                "sdfgh",
                "String 'dsghjkl,sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl\"sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft('dsghjkl"sdfgh'),
                "sdfgh",
                "String 'dsghjkl\"sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl)sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl)sdfgh"),
                "sdfgh",
                "String 'dsghjkl)sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl]sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl]sdfgh"),
                "sdfgh",
                "String 'dsghjkl]sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl}sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl}sdfgh"),
                "sdfgh",
                "String 'dsghjkl}sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl(sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl(sdfgh"),
                "sdfgh",
                "String 'dsghjkl(sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl[sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl[sdfgh"),
                "sdfgh",
                "String 'dsghjkl[sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl{sdfgh'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl{sdfgh"),
                "sdfgh",
                "String 'dsghjkl{sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("S-exp 'dsghjkl (sdfgh)'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl (sdfgh)"),
                "(sdfgh)",
                "String 'dsghjkl (sdfgh)' -> '(sdfgh)' "
            );
        });
        mocha.it("S-exp  'dsghjkl [sdfgh]'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl [sdfgh]"),
                "[sdfgh]",
                "String 'dsghjkl [sdfgh]' -> '[sdfgh]' "
            );
        });
        mocha.it("Quoted S-exp  'dsfsadf \"dsghjkl [sdfgh]\"'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft('dsfsadf "dsghjkl [sdfgh]"'),
                '"dsghjkl [sdfgh]"',
                "String 'dsfsadf \"dsghjkl [sdfgh]\"' -> '\"dsghjkl [sdfgh]\"' "
            );
        });
        mocha.it("Quoted S-exp  'dsfsadf\"dsghjkl [sdfgh]\"'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft('dsfsadf"dsghjkl [sdfgh]"'),
                '"dsghjkl [sdfgh]"',
                "String 'dsfsadf\"dsghjkl [sdfgh]\"' -> '\"dsghjkl [sdfgh]\"' "
            );
        });
        mocha.it("S-exp  'dsghjkl {sdfgh}'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl {sdfgh}"),
                "{sdfgh}",
                "String 'dsghjkl {sdfgh}' -> '{sdfgh}' "
            );
        });
        mocha.it("S-exp 'dsghjkl \"s(dfg)h\"'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft('dsghjkl "s(dfg)h"'),
                '"s(dfg)h"',
                "String 'dsghjkl \"s(dfg)h\"' -> '\"s(dfg)h\"' "
            );
        });
        mocha.it("S-exp 'dsghjkl '(1, 2, 3, 4, 5, 6)'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl '(1, 2, 3, 4, 5, 6)"),
                "'(1, 2, 3, 4, 5, 6)",
                "String 'dsghjkl '(1, 2, 3, 4, 5, 6)' -> ''(1, 2, 3, 4, 5, 6)' "
            );
        });
        mocha.it("S-exp 'dsghjkl '(1 2 3 4 5 6)'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("dsghjkl '(1 2 3 4 5 6)"),
                "'(1 2 3 4 5 6)",
                "String 'dsghjkl '(1 2 3 4 5 6)' -> ''(1 2 3 4 5 6)' "
            );
        });
        mocha.it("S-exp  'fsdg[dsghjkl (sdfgh)]'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("fsdg[dsghjkl (sdfgh)]"),
                "[dsghjkl (sdfgh)]",
                "String 'fsdg[dsghjkl (sdfgh)]' -> '[dsghjkl (sdfgh)]' "
            );
        });
        mocha.it("S-exp  'fsdg (dsghjkl (sdfgh))'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("fsdg (dsghjkl (sdfgh))"),
                "(dsghjkl (sdfgh))",
                "String 'fsdg (dsghjkl (sdfgh))' -> '(dsghjkl (sdfgh))' "
            );
        });
        mocha.it("S-exp  'fsdg (dsghjkl {sdfgh})'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("fsdg (dsghjkl {sdfgh})"),
                "(dsghjkl {sdfgh})",
                "String 'fsdg (dsghjkl {sdfgh})' -> '(dsghjkl {sdfgh})' "
            );
        });
        mocha.it("S-exp  'sdfgdgf(define (f x)\\n(* 8 x))'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("sdfgdgf(define (f x)\n(* 8 x))"),
                "(define (f x)\n(* 8 x))",
                "String 'sdfgdgf(define (f x)\\n(* 8 x))' -> '(define (f x)\\n(* 8 x))' "
            );
        });
        mocha.it("Invalid s-exp 'fsdg (dsghjkl] {sdfgh})'", () => {
            chai.assert.strictEqual(
                s.getSexpToLeft("fsdg (dsghjkl] {sdfgh})"),
                "dsghjkl] {sdfgh})",
                "String 'fsdg (dsghjkl] {sdfgh})' -> 'fsdg (dsghjkl] {sdfgh})' "
            );
        });
    });
});
