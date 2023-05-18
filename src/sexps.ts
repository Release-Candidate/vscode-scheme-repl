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

/**
 * Regex to match the double quoted string to the left of the end of the string.
 */
const leftUntilQuote = /.*?(?<sexp>[^"]+)\s*$/du;

/**
 * Return the s-expression that ends at the end of `text`.
 * @param text The text to parse.
 * @returns The the s-expression that ends at the end of `text`.
 */
export function getSexpToLeft(text: string) {
    const trimmed = text.trim();
    return parseSexpToLeft(["Any"], trimmed, 0);
}

/**
 * Return the string until the next sexp delimiter to the left.
 * @param delimStack The stack of delimiters of the current sexp.
 * @param s The string to parse from the end to the next sexp delimiter.
 * @returns The string until the next sexp delimiter to the left.
 */
// eslint-disable-next-line max-statements, max-lines-per-function, complexity, consistent-return
function parseSexpToLeft(
    delimStack: Delimiter[],
    s: string,
    level: number
): string {
    const delim = delimStack.pop();
    if (s.endsWith(")") && delim !== "Quote") {
        if (delim) {
            delimStack.push(delim);
        }
        delimStack.push("Paren");
        return parseSexpToLeft(delimStack, s.slice(0, -1), level + 1) + ")";
    } else if (s.endsWith("]") && delim !== "Quote") {
        if (delim) {
            delimStack.push(delim);
        }
        delimStack.push("Bracket");

        return parseSexpToLeft(delimStack, s.slice(0, -1), level + 1) + "]";
    } else if (s.endsWith("}") && delim !== "Quote") {
        if (delim) {
            delimStack.push(delim);
        }
        delimStack.push("Brace");

        return parseSexpToLeft(delimStack, s.slice(0, -1), level + 1) + "}";
    } else if (s.endsWith('"') && delim === "Quote") {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return '"';
        } else {
            return parseSexpToLeft(delimStack, s.slice(0, -1), newLevel) + '"';
        }
    } else if (s.endsWith('"') && delim !== "Quote") {
        if (delim) {
            delimStack.push(delim);
        }
        delimStack.push("Quote");
        return parseSexpToLeft(delimStack, s.slice(0, -1), level + 1) + '"';
    } else if (s.endsWith(",")) {
        if (delim) {
            delimStack.push(delim);
        }
        delimStack.push(delim ? delim : "Any");
        return parseSexpToLeft(delimStack, s.slice(0, -1), level) + ",";
    } else if (s.endsWith(" ")) {
        if (delim) {
            delimStack.push(delim);
        }
        delimStack.push(delim ? delim : "Any");
        return parseSexpToLeft(delimStack, s.slice(0, -1), level) + " ";
    } else if (s.endsWith("\n")) {
        if (delim) {
            delimStack.push(delim);
        }
        delimStack.push(delim ? delim : "Any");
        return parseSexpToLeft(delimStack, s.slice(0, -1), level) + "\n";
    } else if (s.endsWith("\t")) {
        if (delim) {
            delimStack.push(delim);
        }
        delimStack.push(delim ? delim : "Any");
        return parseSexpToLeft(delimStack, s.slice(0, -1), level) + "\t";
    } else if (s.endsWith("'(") && delim === "Paren") {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return "'(";
        } else {
            return (
                parseSexpToLeft(delimStack, s.slice(0, -1), level - 1) + "'("
            );
        }
    } else if (s.endsWith("(") && delim === "Paren") {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return "(";
        } else {
            return parseSexpToLeft(delimStack, s.slice(0, -1), level - 1) + "(";
        }
    } else if (s.endsWith("[") && delim === "Bracket") {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return "[";
        } else {
            return parseSexpToLeft(delimStack, s.slice(0, -1), level - 1) + "[";
        }
    } else if (s.endsWith("{") && delim === "Brace") {
        const newLevel = level - 1;
        if (newLevel === 0) {
            return "{";
        } else {
            return parseSexpToLeft(delimStack, s.slice(0, -1), level - 1) + "{";
        }
    }
    delimStack.push(delim ? delim : "Any");
    switch (delim) {
        case "Paren": {
            const found = leftUntilDelimiter.exec(s);
            if (found) {
                const foundVal = found.groups ? found.groups.sexp : "";
                return (
                    parseSexpToLeft(
                        delimStack,
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
                        delimStack,
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
                        delimStack,
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
                        delimStack,
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
                        delimStack,
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
