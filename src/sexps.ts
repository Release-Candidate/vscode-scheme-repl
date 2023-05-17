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

const leftUntilQuote = /.*?(?<sexp>[^"]+)\s*$/du;

/**
 * Return the s-expression that ends at the end of `text`.
 * @param text The text to parse.
 * @returns The the s-expression that ends at the end of `text`.
 */
export function getSexpToLeft(text: string) {
    const trimmed = text.trim();
    return parseSexpToLeft("Any", trimmed, 0);
}

/**
 * Return the string until the next sexp delimiter to the left.
 * @param delim The delimiter of the current sexp.
 * @param s The string to parse from the end to the next sexp delimiter.
 * @returns The string until the next sexp delimiter to the left.
 */
// eslint-disable-next-line max-statements, max-lines-per-function, complexity
function parseSexpToLeft(delim: Delimiter, s: string, level: number): string {
    if (s.endsWith(")")) {
        return parseSexpToLeft("Paren", s.slice(0, -1), level + 1) + ")";
    } else if (s.endsWith("]")) {
        return parseSexpToLeft("Bracket", s.slice(0, -1), level + 1) + "]";
    } else if (s.endsWith("}")) {
        return parseSexpToLeft("Brace", s.slice(0, -1), level + 1) + "}";
    } else if (s.endsWith('"') && delim === "Quote") {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return '"';
        } else {
            return parseSexpToLeft("Any", s.slice(0, -1), level - 1) + '"';
        }
    } else if (s.endsWith('"')) {
        return parseSexpToLeft("Quote", s.slice(0, -1), level + 1) + '"';
    } else if (s.endsWith(" ")) {
        return parseSexpToLeft("Any", s.slice(0, -1), level) + " ";
    } else if (s.endsWith("\n")) {
        return parseSexpToLeft("Any", s.slice(0, -1), level) + "\n";
    } else if (s.endsWith("\t")) {
        return parseSexpToLeft("Any", s.slice(0, -1), level) + "\t";
    } else if (s.endsWith("(")) {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return "(";
        } else {
            return parseSexpToLeft("Any", s.slice(0, -1), level - 1) + "(";
        }
    } else if (s.endsWith("[")) {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return "[";
        } else {
            return parseSexpToLeft("Any", s.slice(0, -1), level - 1) + "[";
        }
    } else if (s.endsWith("{")) {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return "{";
        } else {
            return parseSexpToLeft("Any", s.slice(0, -1), level - 1) + "{";
        }
    }
    switch (delim) {
        case "Paren": {
            const found = leftUntilDelimiter.exec(s);
            if (found) {
                const foundVal = found.groups ? found.groups.sexp : "";

                return (
                    parseSexpToLeft(
                        delim,
                        s.slice(0, found.indices ? found.indices[1][0] : 1),
                        level
                    ) + foundVal
                );
            } else {
                return "";
            }
        }
        case "Bracket": {
            const found = leftUntilDelimiter.exec(s);
            if (found) {
                const foundVal = found.groups ? found.groups.sexp : "";

                return (
                    parseSexpToLeft(
                        delim,
                        s.slice(0, found.indices ? found.indices[1][0] : 1),
                        level
                    ) + foundVal
                );
            } else {
                return "";
            }
        }
        case "Brace": {
            const found = leftUntilDelimiter.exec(s);
            if (found) {
                const foundVal = found.groups ? found.groups.sexp : "";

                return (
                    parseSexpToLeft(
                        delim,
                        s.slice(0, found.indices ? found.indices[1][0] : 1),
                        level
                    ) + foundVal
                );
            } else {
                return "";
            }
        }
        case "Quote": {
            const found = leftUntilQuote.exec(s);
            if (found) {
                const foundVal = found.groups ? found.groups.sexp : "";

                return (
                    parseSexpToLeft(
                        delim,
                        s.slice(0, found.indices ? found.indices[1][0] : 1),
                        level
                    ) + foundVal
                );
            } else {
                return "";
            }
        }
        case "Any": {
            const found = leftUntilDelimiter.exec(s);
            if (found) {
                const foundVal = found.groups ? found.groups.sexp : "";
                if (level === 0) {
                    return foundVal;
                }
                return (
                    parseSexpToLeft(
                        delim,
                        s.slice(0, found.indices ? found.indices[1][0] : 1),
                        level
                    ) + foundVal
                );
            } else {
                return "";
            }
        }
    }
    return "";
}
