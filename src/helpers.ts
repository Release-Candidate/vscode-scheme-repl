/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     helpers.ts
 * Date:     18.May.2023
 *
 * ==============================================================================
 * General helper functions
 */

import * as vscode from "vscode";
import { IdentifierType } from "./functionDocumentation";

/**
 * Do nothing for the given time `ms`.
 * @param ms The sleep time in milliseconds.
 */
export async function sleep(ms: number) {
    return new Promise<void>((resolve) => {
        setTimeout(resolve, ms);
    });
}

/**
 * Regex to match all characters that need to be escaped when used in  a
 * `RegExp`.
 */
const escapeRegex = /[.*+?^${}()|[\]\\]/gu;

/**
 * Return the string `text` with all special characters escaped, for use in a
 * `RegExp`.
 * @param text The string to escape all special characters in.
 * @returns The string `text` with all special characters escaped, for use in a
 * `RegExp`.
 */
export function escapeRegexp(text: string) {
    return text.replace(escapeRegex, "\\$&");
}

/**
 * Convert the given `IdentifierType` to a `CompletionItemKind`.
 * @param identifierType The `IdentifierType` to convert.
 * @returns The given `IdentifierType` converted to a `CompletionItemKind`.
 */
export function identifierToCompletionKind(
    identifierType: IdentifierType
): vscode.CompletionItemKind {
    switch (identifierType) {
        case "global parameter":
            return vscode.CompletionItemKind.Value;
        case "module":
            return vscode.CompletionItemKind.Module;
        case "procedure":
            return vscode.CompletionItemKind.Function;
        case "syntax":
            return vscode.CompletionItemKind.Keyword;
        case "thread parameter":
            return vscode.CompletionItemKind.Value;
        case "Error: unknown":
        default:
            return vscode.CompletionItemKind.User;
    }
}
