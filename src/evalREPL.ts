/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     evalREPL.ts
 * Date:     04.Jun.2023
 *
 * =============================================================================
 * Functions to evaluate sexps, commands, ... in a "hidden" REPL, not visible
 * for the user.
 */

import * as c from "./constants";
import * as decor from "./textDecorations";
import * as h from "./helpers";
import * as sexp from "./sexps";
import * as vscode from "vscode";

/**
 * Matches an exception with line and column printed.
 * The line is saved in the match group `line`, the column is saved in group
 * `col`.
 */
const lineColumnRegex =
    /^\s*Exception.+\s+at\s+line\s+(?<line>\d+),\s+char\s+(?<col>\d+)\s+/mu;

/**
 * Matches an exception with character printed.
 * The character's number is saved in the match group `char`.
 */
const characterRegex = /^\s*Exception.+\s+at\s+.+:(?<char>\d+)\s*>/mu;

/**
 * Matches an exception of type "variable FOO not bound".
 * Saves the unbound identifier in the match group `name`.
 */
const notBoundRegex =
    /^\s*Exception:\s+variable\s+(?<name>\S+)\s+is\s+not\s+bound\s+/mu;

/**
 * Returns a list of identifiers beginning with the string `prefix` or
 * the empty list `[]` if no such identifier exists.
 * Runs the function `evalIdentifiers(prefix)` in a REPL with `document` loaded
 * to get all local identifiers.
 * @param env The needed environment.
 * @param document The source file.
 * @param prefix The string to search for.
 * @returns A list of identifiers beginning with the string `prefix` or
 * the empty list `[]` if no such identifier exists.
 */
export async function evalGetIds(
    env: h.Env,
    document: vscode.TextDocument,
    prefix: string
): Promise<string[]> {
    env.outChannel.appendLine(`Searching for completions of ${prefix}`);
    const out = await runREPLCommand(
        env.config,
        document,
        c.evalIdentifiers(prefix)
    );
    if (out.error || out.stderr) {
        env.outChannel.appendLine(
            `Completion error: ${h.fromMaybe(out.error, "")} ${h.fromMaybe(
                out.stderr,
                ""
            )}\nusing ${c.evalIdentifiers(prefix)}`
        );
    }
    const match = out.stdout
        ? out.stdout.match(matchREPLResponse("\\((.*?)\\)"))
        : "";
    const response = match ? match[1].trim() : "";
    const outArr = response.split(/\s+/gu);
    env.outChannel.appendLine(`Got completions: ${outArr}`);
    return outArr;
}

/**
 * Try to load a file to the REPL and check for errors.
 * Add the errors to the list of diagnostics.
 * @param env The needed environment.
 * @param editor The file to load.
 */
export async function loadFile(
    env: h.Env,
    editor: vscode.TextEditor
): Promise<void> {
    const out = await runREPLCommand(env.config, editor.document, "");
    if (out.error) {
        env.outChannel.appendLine(
            `Error checking file ${editor.document.fileName}:\n${out.error}\nStderr: ${out.stderr}\nStdout: ${out.stdout}`
        );
    } else if (out.stderr) {
        env.outChannel.appendLine(
            `Checking file ${editor.document.fileName} yields error:\n${out.stderr}\nStdout: ${out.stdout}`
        );
        const errRange = parseError(out, editor);
        env.diagnostics.set(editor.document.uri, [
            {
                message: out.stderr,
                range: errRange,
                severity: vscode.DiagnosticSeverity.Error,
                source: c.diagnosticsCollName,
            },
        ]);
    } else if (out.stdout) {
        env.outChannel.appendLine(
            `Checking file ${editor.document.fileName} successful:\n${out.stdout}`
        );
        env.diagnostics.delete(editor.document.uri);
    }
}

// eslint-disable-next-line max-statements
function parseError(out: h.Output, editor: vscode.TextEditor) {
    const lineMatch = out.stderr?.match(lineColumnRegex);
    const charMatch = out.stderr?.match(characterRegex);
    const unboundMatch = out.stderr?.match(notBoundRegex);
    let errRange = h.rangeFromPositions([0, 0], [0, 0]);
    if (lineMatch) {
        const line = lineMatch.groups ? Number(lineMatch.groups.line) - 1 : 0;
        const col = lineMatch.groups ? Number(lineMatch.groups.col) - 1 : 0;
        errRange = h.rangeFromPositions([line, col], [line, col]);
    } else if (charMatch) {
        const character = charMatch.groups ? Number(charMatch.groups.char) : 0;
        errRange = h.rangeFromPositions([0, character], [0, character]);
    } else if (unboundMatch) {
        const unboundName = unboundMatch.groups ? unboundMatch.groups.name : "";
        unboundName.at(0);
        editor.document.getText();
        out.stdout?.at(0);
    }
    return errRange;
}

/**
 * Evaluate the sexp to the left of the cursor and print the result inline using
 * a decoration.
 * @param env The needed environment.
 * @param editor The document containing the source to evaluate.
 */
export async function evalLastSexp(
    env: h.Env,
    editor: vscode.TextEditor
): Promise<void> {
    const selectedRange = h.rangeFromPositions([0, 0], editor.selection.end);
    const selectedText = editor.document.getText(selectedRange);
    if (selectedText.length) {
        const exp = sexp.getSexpToLeft(selectedText);
        const sexpRange = h.rangeFromPositions(
            [exp.startLine, exp.startCol],
            editor.selection.end
        );
        await evalSexp(env, {
            editor,
            exp,
            range: sexpRange,
            vscodeCommand: c.evalLast,
        });
    } else {
        env.outChannel.appendLine(
            `Not sent ${editor.selection.end.line}:${editor.selection.end.character} to REPL using command ${c.cfgSection}.${c.sendLastToREPL}`
        );
    }
}

/**
 * Evaluate the selected sexp and print the result inline using a decoration.
 * @param env The needed environment.
 * @param editor The document containing the source to evaluate.
 */
export async function evalSelectedSexp(
    env: h.Env,
    editor: vscode.TextEditor
): Promise<void> {
    const selectedRange = h.rangeFromPositions(
        editor.selection.start,
        editor.selection.end
    );
    const selectedText = editor.document.getText(selectedRange);
    if (selectedText.length) {
        const exp = sexp.getSexpToLeft(selectedText);
        await evalSexp(env, {
            editor,
            exp,
            range: selectedRange,
            vscodeCommand: c.evalSelection,
        });
    } else {
        env.outChannel.appendLine(
            `Not sent ${editor.selection.end.line}:${editor.selection.end.character} to REPL using command ${c.cfgSection}.${c.evalSelection}`
        );
    }
}

/**
 * Eval the given sexp `data.exp` and display the result using the given
 * decorations.
 * @param env The needed environment.
 * @param data The needed data.
 */
// eslint-disable-next-line max-lines-per-function, max-statements
async function evalSexp(
    env: h.Env,
    data: {
        editor: vscode.TextEditor;
        exp: {
            sexp: string;
            startLine: number;
            startCol: number;
        };
        range: vscode.Range;
        vscodeCommand: string;
    }
): Promise<void> {
    const out = await runREPLCommand(
        env.config,
        data.editor.document,
        data.exp.sexp
    );
    env.outChannel.appendLine(
        `Sent ${data.exp.sexp} to REPL using command ${c.cfgSection}.${data.vscodeCommand}`
    );
    if (out.stderr) {
        const errMsg = out.stderr.trim();
        decor.addEditorDecoration({
            editor: data.editor,
            evalDecoration: env.evalErrorDecoration,
            evalDecorations: env.evalErrorDecorations,
            range: data.range,
            text: errMsg,
        });
        decor.removeRange({
            decorations: env.evalDecorations,
            decoration: env.evalDecoration,
            editor: data.editor,
            range: data.range,
        });
    } else {
        const match = out.stdout
            ? out.stdout.match(matchREPLResponse("(.+?)"))
            : "";
        const response = match ? match[1].trim() : "";
        decor.addEditorDecoration({
            editor: data.editor,
            evalDecoration: env.evalDecoration,
            evalDecorations: env.evalDecorations,
            range: data.range,
            text: response,
        });
        decor.removeRange({
            decorations: env.evalErrorDecorations,
            decoration: env.evalErrorDecoration,
            editor: data.editor,
            range: data.range,
        });
    }
}

/**
 * Return a `RegExp` to parse the output of a REPL evaluation or completion.
 * @param group The regexp string to match the actual result.
 * @returns A `RegExp` to parse the output of a REPL evaluation or completion.
 */
function matchREPLResponse(group: string): RegExp {
    return new RegExp(
        `(?:${c.replPrompt}\\s+)+${group}\\n${c.replPrompt}\\s+$`,
        "su"
    );
}

/**
 * Return the output of the Chez REPL after loading the file of `editor` and
 * evaluating `exp`.
 * @param config The extension's configuration.
 * @param document The source code containing the sexp to evaluate.
 * @param exp The sexp to evaluate.
 * @returns The output of the Chez REPL after loading the file of `editor` and
 * evaluating `exp`.
 */
async function runREPLCommand(
    config: vscode.WorkspaceConfiguration,
    document: vscode.TextDocument,
    exp: string
): Promise<h.Output> {
    const root = await h.askForWorkspace("Scheme");
    return h.runCommand({
        root: root ? root.uri.fsPath : "./",
        args: [c.replQuietArg],
        cmd: c.getCfgREPLPath(config),
        input: c.replLoadFileAndSexp(document.fileName, exp),
    });
}
