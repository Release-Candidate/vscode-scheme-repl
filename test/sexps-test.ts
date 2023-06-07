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
            chai.assert.deepEqual(
                s.getSexpToLeft(""),
                { sexp: "", startCol: 0, startLine: 0 },
                "Empty string does not change"
            );
        });
        mocha.it("Just some text 'dsghjkl sdfgh'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl sdfgh"),
                { sexp: "sdfgh", startCol: 8, startLine: 0 },
                "String 'dsghjkl sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl,sdfgh'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl,sdfgh"),
                { sexp: "sdfgh", startCol: 8, startLine: 0 },
                "String 'dsghjkl,sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text '\\ndsghjkl\"sdfgh'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft('\ndsghjkl"sdfgh'),
                { sexp: "sdfgh", startCol: 8, startLine: 1 },
                "String '\\ndsghjkl\"sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text '\\ndsghjkl\\n)sdfgh'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("\ndsghjkl\n)sdfgh"),
                { sexp: "sdfgh", startCol: 1, startLine: 2 },
                "String '\\ndsghjkl\\n)sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl]sdfgh\\n\\n '", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl]sdfgh\n\n "),
                { sexp: "sdfgh", startCol: 8, startLine: 0 },
                "String 'dsghjkl]sdfgh\\n\\n ' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl}sdfgh    '", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl}sdfgh    "),
                { sexp: "sdfgh", startCol: 8, startLine: 0 },
                "String 'dsghjkl}sdfgh    ' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl\\n(sdfgh'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl\n(sdfgh"),
                { sexp: "sdfgh", startCol: 1, startLine: 1 },
                "String 'dsghjkl\\n(sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl[sdfgh'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl[sdfgh"),
                { sexp: "sdfgh", startCol: 8, startLine: 0 },
                "String 'dsghjkl[sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("Just some text 'dsghjkl{sdfgh'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl{sdfgh"),
                { sexp: "sdfgh", startCol: 8, startLine: 0 },
                "String 'dsghjkl{sdfgh' -> 'sdfgh' "
            );
        });
        mocha.it("S-exp 'dsghjkl (sdfgh)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl (sdfgh)"),
                { sexp: "(sdfgh)", startCol: 8, startLine: 0 },
                "String 'dsghjkl (sdfgh)' -> '(sdfgh)' "
            );
        });
        mocha.it("S-exp  'dsghjkl\\n[sdfgh]'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl\n[sdfgh]"),
                { sexp: "[sdfgh]", startCol: 0, startLine: 1 },
                "String 'dsghjkl\\n[sdfgh]' -> '[sdfgh]' "
            );
        });
        mocha.it("Quoted S-exp  'dsfsadf \"dsghjkl [sdfgh]\"'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft('dsfsadf "dsghjkl [sdfgh]"'),
                { sexp: '"dsghjkl [sdfgh]"', startCol: 8, startLine: 0 },
                "String 'dsfsadf \"dsghjkl [sdfgh]\"' -> '\"dsghjkl [sdfgh]\"' "
            );
        });
        mocha.it("Quoted S-exp  'dsfsadf\"dsghjkl [sdfgh]\"'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft('dsfsadf"dsghjkl [sdfgh]"'),
                { sexp: '"dsghjkl [sdfgh]"', startCol: 7, startLine: 0 },
                "String 'dsfsadf\"dsghjkl [sdfgh]\"' -> '\"dsghjkl [sdfgh]\"' "
            );
        });
        mocha.it("S-exp  'dsghjkl {sdfgh}'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl {sdfgh}"),
                { sexp: "{sdfgh}", startCol: 8, startLine: 0 },
                "String 'dsghjkl {sdfgh}' -> '{sdfgh}' "
            );
        });
        mocha.it("S-exp 'dsghjkl \"s(dfg)h\"'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft('dsghjkl "s(dfg)h"'),
                { sexp: '"s(dfg)h"', startCol: 8, startLine: 0 },
                "String 'dsghjkl \"s(dfg)h\"' -> '\"s(dfg)h\"' "
            );
        });
        mocha.it("S-exp 'dsghjkl '(1, 2, 3, 4, 5, 6)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl '(1, 2, 3, 4, 5, 6)"),
                { sexp: "'(1, 2, 3, 4, 5, 6)", startCol: 8, startLine: 0 },
                "String 'dsghjkl '(1, 2, 3, 4, 5, 6)' -> ''(1, 2, 3, 4, 5, 6)' "
            );
        });
        mocha.it("S-exp 'dsghjkl '(1 2 3 4 5 6)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("dsghjkl '(1 2 3 4 5 6)"),
                { sexp: "'(1 2 3 4 5 6)", startCol: 8, startLine: 0 },
                "String 'dsghjkl '(1 2 3 4 5 6)' -> ''(1 2 3 4 5 6)' "
            );
        });
        mocha.it("S-exp  'fsdg[dsghjkl (sdfgh)]'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("fsdg[dsghjkl (sdfgh)]"),
                { sexp: "[dsghjkl (sdfgh)]", startCol: 4, startLine: 0 },
                "String 'fsdg[dsghjkl (sdfgh)]' -> '[dsghjkl (sdfgh)]' "
            );
        });
        mocha.it("S-exp  'fsdg (dsghjkl (sdfgh))'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("fsdg (dsghjkl (sdfgh))"),
                { sexp: "(dsghjkl (sdfgh))", startCol: 5, startLine: 0 },
                "String 'fsdg (dsghjkl (sdfgh))' -> '(dsghjkl (sdfgh))' "
            );
        });
        mocha.it("S-exp  'fsdg (dsghjkl {sdfgh})'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("fsdg (dsghjkl {sdfgh})"),
                { sexp: "(dsghjkl {sdfgh})", startCol: 5, startLine: 0 },
                "String 'fsdg (dsghjkl {sdfgh})' -> '(dsghjkl {sdfgh})' "
            );
        });
        mocha.it("S-exp 'sdfgdgf(define (f x)\\n(* 8 x))'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf(define (f x)\n(* 8 x))"),
                { sexp: "(define (f x)\n(* 8 x))", startCol: 7, startLine: 0 },
                "String 'sdfgdgf(define (f x)\\n(* 8 x))' -> '(define (f x)\\n(* 8 x))' "
            );
        });
        mocha.it("Quoted s-exp `sdfgdgf'(define (f x)\\n(* 8 x))`", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf'(define (f x)\n(* 8 x))"),
                { sexp: "'(define (f x)\n(* 8 x))", startCol: 7, startLine: 0 },
                "String 'sdfgdgf'(define (f x)\\n(* 8 x))' -> `'(define (f x)\\n(* 8 x))`"
            );
        });
        mocha.it("Vector 'sdfgdgf #(asdf fdsa 156)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf #(asdf fdsa 156)"),
                { sexp: "#(asdf fdsa 156)", startCol: 8, startLine: 0 },
                "Vector 'sdfgdgf #(asdf fdsa 156)' -> '#(asdf fdsa 156)' "
            );
        });
        mocha.it("Vector 'sdfgdgf #10(asdf fdsa 156)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf #10(asdf fdsa 156)"),
                { sexp: "#10(asdf fdsa 156)", startCol: 8, startLine: 0 },
                "Vector 'sdfgdgf #10(asdf fdsa 156)' -> '#10(asdf fdsa 156)' "
            );
        });
        mocha.it("Bytevector 'sdfgdgf #vu8(255 0 255)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf #vu8(255 0 255)"),
                { sexp: "#vu8(255 0 255)", startCol: 8, startLine: 0 },
                "Vector 'sdfgdgf #vu8(255 0 255)' -> '#vu8(255 0 255)' "
            );
        });
        mocha.it("Bytevector 'sdfgdgf #4vu8(255 0 255)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf #4vu8(255 0 255)"),
                { sexp: "#4vu8(255 0 255)", startCol: 8, startLine: 0 },
                "Vector 'sdfgdgf #4vu8(255 0 255)' -> '#4vu8(255 0 255)' "
            );
        });
        mocha.it("Fixnum vector 'sdfgdgf #vfx(255 0 255)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf #vfx(255 0 255)"),
                { sexp: "#vfx(255 0 255)", startCol: 8, startLine: 0 },
                "Vector 'sdfgdgf #vfx(255 0 255)' -> '#vfx(255 0 255)' "
            );
        });
        mocha.it("Fixnum vector 'sdfgdgf #125vfx(255 0 255)'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf #125vfx(255 0 255)"),
                { sexp: "#125vfx(255 0 255)", startCol: 8, startLine: 0 },
                "Vector 'sdfgdgf #125vfx(255 0 255)' -> '#125vfx(255 0 255)' "
            );
        });
        mocha.it("Gensym 'sdfgdgf #{g0 gdfgez754123245}'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf #{g0 gdfgez754123245}"),
                { sexp: "#{g0 gdfgez754123245}", startCol: 8, startLine: 0 },
                "Vector 'sdfgdgf #{g0 gdfgez754123245}' -> '#{g0 gdfgez754123245}' "
            );
        });
        mocha.it("Quoted gensym `sdfgdgf '#{g0 gdfgez754123245}`", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf '#{g0 gdfgez754123245}"),
                { sexp: "'#{g0 gdfgez754123245}", startCol: 8, startLine: 0 },
                "Vector `sdfgdgf '#{g0 gdfgez754123245}` -> `'#{g0 gdfgez754123245}` "
            );
        });
        mocha.it("Quasiquoted gensym 'sdfgdgf `#{g0 gdfgez754123245}'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("sdfgdgf `#{g0 gdfgez754123245}"),
                { sexp: "`#{g0 gdfgez754123245}", startCol: 8, startLine: 0 },
                "Vector 'sdfgdgf `#{g0 gdfgez754123245}' -> '`#{g0 gdfgez754123245}' "
            );
        });
        mocha.it("Invalid s-exp 'fsdg (dsghjkl] {sdfgh})'", () => {
            chai.assert.deepEqual(
                s.getSexpToLeft("fsdg (dsghjkl] {sdfgh})"),
                { sexp: "dsghjkl] {sdfgh})", startCol: 6, startLine: 0 },
                "String 'fsdg (dsghjkl] {sdfgh})' -> 'fsdg (dsghjkl] {sdfgh})' "
            );
        });
    });
});
