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
// eslint-disable-next-line max-lines-per-function
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
    const out = await runREPLCommand(env.config, data.editor, data.exp.sexp);
    if (out.stderr) {
        const errMsg = out.stderr.trim();
        decor.addEditorDecoration({
            editor: data.editor,
            evalDecoration: env.evalErrorDecoration,
            evalDecorations: env.evalErrorDecorations,
            range: data.range,
            text: errMsg,
        });
    } else {
        const match = out.stdout ? out.stdout.match(/>([^>]+)$/u) : "";
        const response = match ? match[1].trim() : "";
        env.outChannel.appendLine(
            `Sent ${data.exp.sexp} to REPL using command ${c.cfgSection}.${data.vscodeCommand}`
        );

        decor.addEditorDecoration({
            editor: data.editor,
            evalDecoration: env.evalDecoration,
            evalDecorations: env.evalDecorations,
            range: data.range,
            text: response,
        });
    }
}

/**
 * Return the output of the Chez REPL after loading the file of `editor` and
 * evaluating `exp`.
 * @param config The extension's configuration.
 * @param editor The source code containing the sexp to evaluate.
 * @param exp The sexp to evaluate.
 * @returns The output of the Chez REPL after loading the file of `editor` and
 * evaluating `exp`.
 */
async function runREPLCommand(
    config: vscode.WorkspaceConfiguration,
    editor: vscode.TextEditor,
    exp: string
): Promise<h.Output> {
    const root = await h.askForWorkspace("Scheme");
    return h.runCommand({
        root: root ? root.uri.fsPath : "./",
        args: [c.replQuietArg],
        cmd: c.getCfgREPLPath(config),
        input: c.replLoadFileAndSexp(editor.document.fileName, exp),
    });
}
