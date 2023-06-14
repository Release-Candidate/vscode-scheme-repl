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
 * Matches an exception of type "wrong number of arguments".
 * The function's name is saved in the group `name`.
 * The character's number is saved in the match group `char`.
 */
const wrongNumArgRegex =
    /^\s*Exception:\s+incorrect\s+number.+\s+arguments.+procedure\s+(?<name>\S+)\s+at\s+.+:(?<char>\d+)\s*>/mu;

/**
 * Matches an exception of type "variable FOO not bound".
 * Saves the unbound identifier in the match group `name`.
 */
const notBoundRegex =
    /^\s*Exception:\s+variable\s+(?<name>\S+)\s+is\s+not\s+bound/mu;

/**
 * Matches an exception of type "non-procedure called".
 * The name of the identifier, which isn't the name of a function is saved in
 * the match group `func`.
 */
const applyNonProcRegex =
    /^\s*Exception:\s+.+apply\s+non-procedure\s+(?<func>.*)/mu;

/**
 * Matches an exception of type "not an environment".
 * The function's name which's argument should be an environment is saved in the
 * match group `func`.
 * The value, which isn't an environment is saved in the match group `val`.
 */
const notAnEnvRegex =
    /^\s*Exception\s+in\s+(?<func>.*?):\s+(?<val>.*?)\s+is\s+not\s+an\s+environment/mu;

/**
 * Regex to match the last (real) expression in the output of the `load`
 * function of the REPL.
 * The last expression is saved in the match group `last`.
 */
const lastExprRegex = /\n(?<last>[^\n]+)\s+(\S?\S?\S?[>%@#$~^&])\s\2?\s?\n*$/su;

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
        const errRange = parseError(out, editor.document.getText());
        env.diagnostics.set(editor.document.uri, [
            {
                message: `${out.stderr}\nThe error has been thrown at the end of:\n${out.stdout}`,
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

// eslint-disable-next-line max-statements, max-lines-per-function
export function parseError(out: h.Output, text: string): vscode.Range {
    const lineMatch = out.stderr?.match(lineColumnRegex);
    const unboundMatch = out.stderr?.match(notBoundRegex);
    const nonProcMatch = out.stderr?.match(applyNonProcRegex);
    const notEnvMatch = out.stderr?.match(notAnEnvRegex);
    if (lineMatch?.groups) {
        const line = Number(lineMatch.groups.line) - 1;
        const col = Number(lineMatch.groups.col) - 1;
        return h.rangeFromPositions([line, col], [line, col]);
    } else if (unboundMatch?.groups) {
        const inTextRegex = new RegExp(
            `(${h.escapeRegexp(unboundMatch.groups.name)})`,
            "dsu"
        );
        const inText = text.match(inTextRegex);
        // eslint-disable-next-line max-depth
        if (inText?.indices) {
            const lineColS = h.getLineColFromCharIndex(
                inText.indices[1][0],
                text
            );
            const lineColE = h.getLineColFromCharIndex(
                inText.indices[1][1],
                text
            );
            return h.rangeFromPositions(
                [lineColS.startLine, lineColS.startCol],
                [lineColE.startLine, lineColE.startCol]
            );
        }
    } else if (notEnvMatch?.groups) {
        const notEnvFunc = notEnvMatch.groups.func;
        const notEnvVal = notEnvMatch.groups.val;
        const inTextRegex = new RegExp(
            `\\((${notEnvFunc}\\s+(?!.*${notEnvFunc}.*?\\s+${notEnvVal})(?:.+\\s+)?${notEnvVal})`,
            "dsu"
        );
        const inText = text.match(inTextRegex);
        // eslint-disable-next-line max-depth
        if (inText?.indices) {
            const lineColS = h.getLineColFromCharIndex(
                inText.indices[1][0],
                text
            );
            const lineColE = h.getLineColFromCharIndex(
                inText.indices[1][1],
                text
            );
            return h.rangeFromPositions(
                [lineColS.startLine, lineColS.startCol],
                [lineColE.startLine, lineColE.startCol]
            );
        }
    } else if (nonProcMatch?.groups) {
        const nonProcName = nonProcMatch.groups.func;
        const inTextRegex = new RegExp(`\\(\\s*(${nonProcName})`, "dsu");
        const inText = text.match(inTextRegex);
        // eslint-disable-next-line max-depth
        if (inText?.indices) {
            const lineColS = h.getLineColFromCharIndex(
                inText.indices[1][0],
                text
            );
            const lineColE = h.getLineColFromCharIndex(
                inText.indices[1][1],
                text
            );
            return h.rangeFromPositions(
                [lineColS.startLine, lineColS.startCol],
                [lineColE.startLine, lineColE.startCol]
            );
        }
    }
    if (out.stdout) {
        const lastMatch = out.stdout.match(lastExprRegex);
        if (lastMatch?.groups?.last) {
            const inTextRegex = new RegExp(
                `(${h.makeWhitespaceGeneric(
                    h.escapeRegexp(lastMatch?.groups?.last)
                )})`,
                "dsu"
            );
            const inText = text.match(inTextRegex);
            // eslint-disable-next-line max-depth
            if (inText?.indices) {
                const lineColS = h.getLineColFromCharIndex(
                    inText.indices[1][0],
                    text
                );
                const lineColE = h.getLineColFromCharIndex(
                    inText.indices[1][1],
                    text
                );
                return h.rangeFromPositions(
                    [lineColS.startLine, lineColS.startCol],
                    [lineColE.startLine, lineColE.startCol]
                );
            }
        }
    }
    return h.rangeFromPositions([0, 0], [0, 0]);
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
