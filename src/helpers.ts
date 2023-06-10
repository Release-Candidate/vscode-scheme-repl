/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     helpers.ts
 * Date:     18.May.2023
 *
 * ==============================================================================
 * General helper functions.
 */

/* eslint-disable camelcase */

import * as child_process from "child_process";
import * as vscode from "vscode";
import internal = require("stream");

/**
 * The type of the extension's environment.
 * That is state that is almost always needed.
 */
export type Env = {
    config: vscode.WorkspaceConfiguration;
    outChannel: vscode.OutputChannel;
    context: vscode.ExtensionContext;
    diagnostics: vscode.DiagnosticCollection;
    evalDecoration: vscode.TextEditorDecorationType;
    evalDecorations: WeakMap<vscode.TextDocument, vscode.DecorationOptions[]>;
    evalErrorDecoration: vscode.TextEditorDecorationType;
    evalErrorDecorations: WeakMap<
        vscode.TextDocument,
        vscode.DecorationOptions[]
    >;
};

/**
 * The `Maybe` type. Either `undefined`/`null` or a value.
 */
export type Maybe<T> = T | undefined | null;

/**
 * The id function.
 * Return the given argument `x`.
 * @param x The argument to return.
 * @returns The given argument `x`.
 */
export function id<T>(x: T): T {
    return x;
}

/**
 * Return the last element of `a` or `undefined`, if `a` is an empty array.
 * @param a The array to return the last element of.
 * @returns The last element of `a` or `undefined`, if `a` is an empty array.
 */
export function last<T>(a: T[]): T | undefined {
    if (a.length === 0) {
        return undefined;
    }
    return a[a.length - 1];
}

/**
 * Object holding the output of a process.
 *
 * Only two possible `Output`s exist: either
 * `{ stdout: string; stderr: string }` or `{ error: string }`.
 *
 *  - If an error occurred while executing the command, the field `error` is set to
 * the message. Normally that means, that the command has not been found.
 * `stdout` and `stderr` are both `undefined`.
 * - If the command returned an error, the error message is returned in the
 * field `stderr`, the output (if any) of stdout is in the string `stdout` and
 * `error` is `undefined`.
 * - If the command finished successfully, the output is returned in the field
 * `stdout`, the field `stderr` should be the empty string `""` and `error` is
 * `undefined`.
 */
export type Output = {
    stdout?: string;
    stderr?: string;
    error?: string;
};

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
 * Return `def` if `s` if `undefined` or `null`, `s` else.
 * @param s The object that can be either `undefined`/`null` or not.
 * @param def The value to return if `s` is `undefined` or `null`.
 * @returns `def` if `s` if `undefined` or `null`, `s` else.
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

/**
 * Spawn the given command with the given arguments and return the output.
 * Set `root` as the working directory of the command.
 * `{ stdout; stderr; error }` is returned, see {@link Output}.
 * @param data.root The current working directory for the command.
 * @param data.cmd The command to call.
 * @param data.args The arguments to pass to the command.
 * @param data.input The string to send to the `stdin` of the process.
 * @returns An object containing the output of the command's execution.
 */
// eslint-disable-next-line max-statements
export async function runCommand(data: {
    root: string;
    cmd: string;
    args: string[];
    input: string;
}): Promise<Output> {
    const proc = child_process.spawn(data.cmd, data.args, {
        cwd: data.root,
        env: process.env,
    });

    const checkCmd = new Promise((_, reject) => {
        proc.on("error", reject);
    });
    proc.stdin.write(data.input);
    proc.stdin.end();

    const out = await readStream(proc.stdout);
    const err = await readStream(proc.stderr);

    const exitCode = new Promise<number>((resolve) => {
        proc.on("close", resolve);
    });

    try {
        await Promise.race([checkCmd, exitCode]);
        return { stdout: out, stderr: err };
    } catch (error) {
        return { error: (error as Error).message };
    }
}

/**
 * Return all data read from the given stream.
 * @param stream The stream to read from.
 * @returns All data read from the given stream.
 */
export async function readStream(stream: internal.Readable): Promise<string> {
    let out = "";
    for await (const chunk of stream) {
        out = out.concat(chunk);
    }

    return out;
}

/**
 * The color theme kind. Dark, light, high contrast light and high contrast
 * dark.
 * VS Code's type does not have an extra enum for "hc-light", and "hc-dark" is
 * called `HighContrast`, because the light variant has been added later.
 */
export type ColorThemeKind = "light" | "dark" | "hc-light" | "hc-dark";

/**
 * Return the current color theme kind.
 * That is, one of "light", "dark", "high contrast light" and "high contrast
 * dark".
 * @returns The current color theme kind.
 */
export function getColorThemeKind(): ColorThemeKind {
    switch (vscode.window.activeColorTheme.kind) {
        case vscode.ColorThemeKind.Light:
            return "light";
        case vscode.ColorThemeKind.Dark:
            return "dark";
        case vscode.ColorThemeKind.HighContrast:
            return "hc-dark";
        // ColorThemeKind === 4
        default:
            return "hc-light";
    }
}

/**
 * Return a `Range` from `start` to `end`.
 * @param start Either a `Position` or the tuple `[line, character]`.
 * @param end Either a `Position` or the tuple `[line, character]`.
 * @returns The `Range` from `start` to `end`.
 */
export function rangeFromPositions(
    start: vscode.Position | [number, number],
    end: vscode.Position | [number, number]
): vscode.Range {
    const startPos =
        start instanceof vscode.Position
            ? start
            : new vscode.Position(start[0], start[1]);
    const endPos =
        end instanceof vscode.Position
            ? end
            : new vscode.Position(end[0], end[1]);
    return new vscode.Range(startPos, endPos);
}

/**
 * Return the start position (line and column/character) of `end` in `text`.
 * The prerequisite is that `end` does not end in whitespace, as whitespace is
 * trimmed from `text`.
 * @param text The whole string.
 * @param end The substring at the end of `text`.
 * @returns The start position (line and column/character) of `end` in `text`.
 */
export function getStartPosition(
    text: string,
    end: string
): { startLine: number; startCol: number } {
    const trimmed = text.trimEnd();
    const whitespaceDiff = text.length - trimmed.length;
    const idx = text.length - end.length - whitespaceDiff;
    const before = text.slice(0, idx);
    const lastNewlineIdx = before.lastIndexOf("\n");
    const startCol = idx - (lastNewlineIdx < 0 ? 0 : lastNewlineIdx + 1);
    const startLine = before.split("\n").length - 1;

    return { startLine, startCol };
}
