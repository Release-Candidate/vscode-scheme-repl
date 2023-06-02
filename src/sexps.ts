/* eslint-disable max-statements */
/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     sexps.ts
 * Date:     15.May.2023
 *
 * ==============================================================================
 * S-expression parsing and related functions.
 */

import * as h from "./helpers";

/**
 * The kind of delimiter of the "current" sexp.
 * One of:
 *  - `"Paren"` - `()`
 *  - `"Bracket"` - `[]`
 *  - `"Brace"` - `{}`
 *  - `"Quote"` - `"`
 *  - `Any` - any one of the delimiters of a sexp, the regexp `[\s,"()\[\]{}]`.
 */
type Delimiter = "Paren" | "Bracket" | "Brace" | "Quote" | "Any";

/**
 * Regex to match the string until the next sexp delimiter to the left of the
 * end of the string.
 */
const leftUntilDelimiter = /.*?(?<sexp>[^\s()[\]{},"]+)\s*$/dsu;

/**
 * Regex to match the double quoted string to the left of the end of the string.
 */
const leftUntilQuote = /.*?(?<sexp>[^"]+)\s*$/du;

/**
 * Return the s-expression that ends at the end of `text`.
 * Also returns the start position of the sexp, in `startLine` and `startCol`.
 * @param text The text to parse.
 * @returns The the s-expression that ends at the end of `text` and the position
 * of the start of the s-expression in `text`.
 */
export function getSexpToLeft(text: string): {
    sexp: string;
    startLine: number;
    startCol: number;
} {
    const trimmed = text.trimEnd();
    const sexp = parseSexpToLeft(["Any"], trimmed, 0);
    const startPos = h.getStartPosition(text, sexp);
    return { sexp, startCol: startPos.startCol, startLine: startPos.startLine };
}

/**
 * Return the string until the next sexp delimiter to the left.
 * This is a recursive function, that calls itself and finally returns the whole
 * sexp as a string.
 * @param delimStack The stack of delimiters of the current sexp.
 * @param s The string to parse from the end to the next sexp delimiter.
 * @returns The string until the next sexp delimiter to the left.
 */
function parseSexpToLeft(
    delimStack: Delimiter[],
    s: string,
    level: number
): string {
    const delim = delimStack.pop();

    const foundStart = startOfSexp({ s, delim, delimStack, level });
    if (foundStart) {
        return foundStart;
    }

    const foundSep = listSeparators({ s, delim, delimStack, level });
    if (foundSep) {
        return foundSep;
    }

    const foundEnd = endOfSexp({ s, delim, delimStack, level });
    if (foundEnd) {
        return foundEnd;
    }

    return parseBetweenDelimiters({ delimStack, delim, s, level });
}

/**
 * Return the sexp started from the right with a delimiter.
 * The delimiter can be either `)`, `]`, `}` or `"`. The sexp is parsed from
 * the right to the left.
 * Return `undefined` if no sexp is being started, that is, no delimiter is
 * present at the right end of the string `s`.
 * @param data The needed data.
 * @returns The sexp started from the right with a delimiter.
 */
function startOfSexp(data: {
    s: string;
    delim: Delimiter | undefined;
    delimStack: Delimiter[];
    level: number;
}): string | undefined {
    if (data.s.endsWith(")") && data.delim !== "Quote") {
        return parseInSexp({
            s: data.s,
            delim: data.delim,
            delimStack: data.delimStack,
            level: data.level,
            currSexpDelim: "Paren",
            delimString: ")",
        });
    } else if (data.s.endsWith("]") && data.delim !== "Quote") {
        return parseInSexp({
            s: data.s,
            delim: data.delim,
            delimStack: data.delimStack,
            level: data.level,
            currSexpDelim: "Bracket",
            delimString: "]",
        });
    } else if (data.s.endsWith("}") && data.delim !== "Quote") {
        return parseInSexp({
            s: data.s,
            delim: data.delim,
            delimStack: data.delimStack,
            level: data.level,
            currSexpDelim: "Brace",
            delimString: "}",
        });
    } else if (data.s.endsWith('"') && data.delim !== "Quote") {
        if (data.delim) {
            data.delimStack.push(data.delim);
        }
        data.delimStack.push("Quote");
        return (
            parseSexpToLeft(
                data.delimStack,
                data.s.slice(0, -1),
                data.level + 1
            ) + '"'
        );
    }
    return undefined;
}

/**
 * Return a list delimiter (whitespace) or quasiquote delimiter (`,`) at the
 * right end of the string and continue the parsing.
 * Return `undefined` if no such delimiter is present.
 * @param data  The needed data.
 * @returns A list delimiter (whitespace) or quasiquote delimiter (`,`) at the
 * right end of the string, if present and the following string until the left
 * end of the current sexp.
 */
function listSeparators(data: {
    s: string;
    delim: Delimiter | undefined;
    delimStack: Delimiter[];
    level: number;
}): string | undefined {
    if (data.s.endsWith(",")) {
        return addDelimAndContinue({
            s: data.s,
            delim: data.delim,
            delimStack: data.delimStack,
            level: data.level,
            delimString: ",",
        });
    } else if (data.s.endsWith(" ")) {
        return addDelimAndContinue({
            s: data.s,
            delim: data.delim,
            delimStack: data.delimStack,
            level: data.level,
            delimString: " ",
        });
    } else if (data.s.endsWith("\n")) {
        return addDelimAndContinue({
            s: data.s,
            delim: data.delim,
            delimStack: data.delimStack,
            level: data.level,
            delimString: "\n",
        });
    } else if (data.s.endsWith("\t")) {
        return addDelimAndContinue({
            s: data.s,
            delim: data.delim,
            delimStack: data.delimStack,
            level: data.level,
            delimString: "\t",
        });
    }
    return undefined;
}

/**
 * Return the string between two sexp delimiters, parsing from the right to the
 * left.
 * Return `""`, the empty string, if no such string exists between at the end of
 * the given string `s`.
 * @param data  The needed data.
 * @returns The string between two sexp delimiters, parsing from the right end
 * to the left.
 */
function parseBetweenDelimiters(data: {
    delimStack: Delimiter[];
    delim: Delimiter | undefined;
    s: string;
    level: number;
}): string {
    data.delimStack.push(data.delim ? data.delim : "Any");

    switch (data.delim) {
        case "Paren":
        case "Bracket":
        case "Brace":
            return getNextLeft({
                s: data.s,
                delimStack: data.delimStack,
                level: data.level,
                regex: leftUntilDelimiter,
            });
        case "Quote":
            return getNextLeft({
                s: data.s,
                delimStack: data.delimStack,
                level: data.level,
                regex: leftUntilQuote,
            });
        case "Any":
            return getNextLeftReturnLevel0({
                s: data.s,
                delimStack: data.delimStack,
                level: data.level,
                regex: leftUntilDelimiter,
            });
    }
    return "";
}

/**
 * Return the left delimiter at the end of the string `s` or `undefined` if the
 * end is not a delimiter of a sexp.
 * If this is not the end of the whole sexp, that is, the `level` is greater
 * than 0, keep on parsing for a new sexp.
 * @param data  The needed data.
 * @returns The left delimiter at the end of the string `s` or `undefined` if the
 * end is not a delimiter of a sexp.
 */
function endOfSexp(data: {
    s: string;
    delim: Delimiter | undefined;
    delimStack: Delimiter[];
    level: number;
}): string | undefined {
    if (data.s.endsWith("'(") && data.delim === "Paren") {
        return endOfSubSexp({
            s: data.s,
            level: data.level,
            delimStack: data.delimStack,
            delimString: "'(",
        });
    } else if (data.s.endsWith("(") && data.delim === "Paren") {
        return endOfSubSexp({
            s: data.s,
            level: data.level,
            delimStack: data.delimStack,
            delimString: "(",
        });
    } else if (data.s.endsWith("[") && data.delim === "Bracket") {
        return endOfSubSexp({
            s: data.s,
            level: data.level,
            delimStack: data.delimStack,
            delimString: "[",
        });
    } else if (data.s.endsWith("{") && data.delim === "Brace") {
        return endOfSubSexp({
            s: data.s,
            level: data.level,
            delimStack: data.delimStack,
            delimString: "{",
        });
    } else if (data.s.endsWith('"') && data.delim === "Quote") {
        const newLevel = data.level - 1;
        if (newLevel === 0) {
            return '"';
        } else {
            return (
                parseSexpToLeft(
                    data.delimStack,
                    data.s.slice(0, -1),
                    newLevel
                ) + '"'
            );
        }
    }
    return undefined;
}

/**
 * Return the string between two delimiters of a sexp.
 * @param data  The needed data.
 * @returns The string between two delimiters of a sexp.
 */
function parseInSexp(data: {
    s: string;
    delim: Delimiter | undefined;
    delimStack: Delimiter[];
    level: number;
    currSexpDelim: Delimiter;
    delimString: string;
}): string {
    if (data.delim) {
        data.delimStack.push(data.delim);
    }
    data.delimStack.push(data.currSexpDelim);
    return (
        parseSexpToLeft(data.delimStack, data.s.slice(0, -1), data.level + 1) +
        data.delimString
    );
}

/**
 * Return a part of a sexp list or quasiquotation, up until the next delimiter
 * of a sexp.
 * @param data  The needed data.
 * @returns A part of a sexp list or quasiquotation, up until the next delimiter
 * of a sexp to the left.
 */
function addDelimAndContinue(data: {
    s: string;
    delim: Delimiter | undefined;
    delimStack: Delimiter[];
    level: number;
    delimString: string;
}): string {
    if (data.delim) {
        data.delimStack.push(data.delim);
    }
    data.delimStack.push(data.delim ? data.delim : "Any");
    return (
        parseSexpToLeft(data.delimStack, data.s.slice(0, -1), data.level) +
        data.delimString
    );
}

/**
 * Return the sexp beginning from the end of the string `s` to the left.
 * If we are at `level` 0 just return the delimiter, as we are just parsing a
 * part of a sexp.
 * @param data  The needed data.
 * @returns The sexp beginning from the end of the string `s` to the left.
 */
function endOfSubSexp(data: {
    s: string;
    level: number;
    delimStack: Delimiter[];
    delimString: string;
}): string {
    const newLevel = data.level - 1;
    if (newLevel === 0) {
        return data.delimString;
    } else {
        return (
            parseSexpToLeft(
                data.delimStack,
                data.s.slice(0, -1),
                data.level - 1
            ) + data.delimString
        );
    }
}

/**
 * Return the next part of a sexp from the right to the left of the string `s`.
 * Return the empty string `""`, if there is a delimiter at the end of the
 * string.
 * @param data  The needed data.
 * @returns The next part of a sexp from the right to the left of the string `s`.
 */
function getNextLeft(data: {
    s: string;
    delimStack: Delimiter[];
    level: number;
    regex: RegExp;
}): string {
    const found = data.regex.exec(data.s);
    if (found) {
        const foundVal = found.groups ? found.groups.sexp : "";
        return (
            parseSexpToLeft(
                data.delimStack,
                data.s.slice(0, found.indices ? found.indices[1][0] : 1),
                data.level
            ) + foundVal
        );
    } else {
        return "";
    }
}

/**
 * Return the next part of a sexp from the right to the left of the string `s`.
 * Return the empty string `""`, if there is a delimiter at the end of the
 * string. If the `level` of the sexp is 0, that means we are parsing an atom,
 * return this without further parsing.
 * @param data  The needed data.
 * @returns The next part of a sexp from the right to the left of the string `s`.
 */
function getNextLeftReturnLevel0(data: {
    s: string;
    delimStack: Delimiter[];
    level: number;
    regex: RegExp;
}): string {
    const found = data.regex.exec(data.s);
    if (found) {
        const foundVal = found.groups ? found.groups.sexp : "";
        if (data.level === 0) {
            return foundVal;
        }
        return (
            parseSexpToLeft(
                data.delimStack,
                data.s.slice(0, found.indices ? found.indices[1][0] : 1),
                data.level
            ) + foundVal
        );
    } else {
        return "";
    }
}
