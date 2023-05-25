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

/**
 * The `Maybe` type. Either `undefined`/`null` or a value.
 */
export type Maybe<T> = T | undefined | null;

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
 * Return `def` if `s` if `undefined` or `null`, `def` else.
 * @param s The object that can be either `undefined`/`null` or not.
 * @param def The value to return if `s` is `undefined` or `null`.
 * @returns `def` if `s` if `undefined` or `null`, `def` else.
 */
export function fromMaybe<T>(s: Maybe<T>, def: T): T {
    return s ? s : def;
}

/**
 * Return the word (determined by the language's word borders) at `position` or
 * `undefined`.
 * @param document The text.
 * @param position The position in the word to return.
 * @returns The word (determined by the language's word borders) at `position` or
 * `undefined`.
 */
export function getWordAtPosition(
    document: vscode.TextDocument,
    position: vscode.Position
): string | undefined {
    const range = document.getWordRangeAtPosition(position);
    return range ? document.getText(range) : undefined;
}

/**
 * Return the root of the only workspace, the root of the workspace that the
 * user selected or `undefined` if there is no currently open workspace
 * (only a single file has been opened).
 * @param askText The text to display if asking the user for a workspace.
 * @returns The root of the only workspace, the root of the workspace that the
 * user selected or `undefined` if there is no currently open workspace
 * (only a single file has been opened).
 */
export async function askForWorkspace(askText: string): Promise<
    vscode.WorkspaceFolder | undefined
    // eslint-disable-next-line indent
> {
    // eslint-disable-next-line no-eq-null, eqeqeq
    if (vscode.workspace.workspaceFolders == null) {
        return undefined;
    } else if (vscode.workspace.workspaceFolders?.length === 1) {
        return vscode.workspace.workspaceFolders[0];
    } else {
        return vscode.window.showWorkspaceFolderPick({
            placeHolder: askText,
        });
    }
}
