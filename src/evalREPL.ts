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
 * Matches an exception of type "variable FOO not bound".
 * Saves the unbound identifier in the match group `name`.
 */
const notBoundRegex =
    /^\s*Exception:\s+variable\s+(?<name>\S+)\s+is\s+not\s+bound/mu;

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
const lastExprRegex =
    /\r?\n(?<last>[^\n]+)\s+(\S?\S?\S?[>%@#$~^&])\s\2?\s?\r?\n*$/su;

/**
 * Regex to match the output of the `version` command of the Chez executable.
 */
const versionOutRegex = /^(?:\d+\.)+\d+.*$/mu;

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
        c.evalIdentifiers(prefix),
        false
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
    const out = await runREPLCommand(env.config, editor.document, "", false);
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

/**
 * Return the `Range` of the expression in the source file's content `text` that
 * caused the error written to `out.stderr`.
 * Returns the first Character `0,0` if no such expression has been found.
 * @param out The output of the REPL when loading the source file.
 * @param text The contents of the source file.
 * @returns The `Range` of the expression in the source file's content `text` that
 * caused the error written to `out.stderr`.
 */
// eslint-disable-next-line max-statements
export function parseError(out: h.Output, text: string): vscode.Range {
    const unboundMatch = out.stderr?.match(notBoundRegex);
    const notEnvMatch = out.stderr?.match(notAnEnvRegex);
    const lineMatch = out.stderr?.match(lineColumnRegex);
    if (lineMatch?.groups) {
        return getLineAndColumn(lineMatch);
    } else if (unboundMatch?.groups?.name) {
        const inTextRegex = new RegExp(
            `(${h.escapeRegexp(unboundMatch.groups.name)})`,
            "dsu"
        );
        const hasRange = searchInText(text, inTextRegex);
        if (hasRange) {
            return hasRange;
        }
    } else if (notEnvMatch?.groups) {
        const notEnvFunc = notEnvMatch.groups.func;
        const notEnvVal = notEnvMatch.groups.val;
        const inTextRegex = new RegExp(
            `\\((${notEnvFunc}\\s+(?!.*${notEnvFunc}.*?\\s+${notEnvVal})(?:.+\\s+)?${notEnvVal})`,
            "dsu"
        );
        const hasRange = searchInText(text, inTextRegex);
        if (hasRange) {
            return hasRange;
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
            const hasRange = searchInText(text, inTextRegex);
            if (hasRange) {
                return hasRange;
            }
        }
    }
    return h.rangeFromPositions([0, 0], [0, 0]);
}

/**
 * Return the line number and column of the match `lineMatch`.
 * `lineMatch` must have match groups `line` and `col`.
 * @param lineMatch The successfully matched regexp.
 * @returns The line number and column of the match `lineMatch`.
 */
function getLineAndColumn(lineMatch: RegExpMatchArray) {
    const line = Number(lineMatch.groups?.line) - 1;
    const col = Number(lineMatch.groups?.col) - 1;
    return h.rangeFromPositions([line, col], [line, col]);
}

/**
 * Return the `Range` of the regex `inTextRegex` in `text` if it matches, or
 * `undefined`.
 * @param text The source code to search in.
 * @param inTextRegex The regex to match.
 * @returns The `Range` of the regex `inTextRegex` in `text` if it matches, or
 * `undefined`.
 */
function searchInText(
    text: string,
    inTextRegex: RegExp
): vscode.Range | undefined {
    const inText = text.match(inTextRegex);
    if (inText?.indices) {
        const lineColS = h.getLineColFromCharIndex(inText.indices[1][0], text);
        const lineColE = h.getLineColFromCharIndex(inText.indices[1][1], text);
        return h.rangeFromPositions(
            [lineColS.startLine, lineColS.startCol],
            [lineColE.startLine, lineColE.startCol]
        );
    }
    return undefined;
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
    const out = await runREPLCommand(
        env.config,
        data.editor.document,
        data.exp.sexp,
        true
    );
    env.outChannel.appendLine(
        `Sent ${data.exp.sexp} to REPL using command ${c.cfgSection}.${data.vscodeCommand} Range ${data.range.start.line} - ${data.range.end.line}`
    );
    if (out.error || out.stderr) {
        const errMsg =
            h.fromMaybe(out.error, "") + " " + h.fromMaybe(out.stderr, "");
        decor.addEditorDecoration({
            editor: data.editor,
            evalDecoration: env.evalErrorDecoration,
            evalDecorations: env.evalErrorDecorations,
            range: data.range,
            text: errMsg.trim(),
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
 * Remove all evaluated values from the current view.
 * @param env The extension environment.
 * @param editor The current active editor.
 */
export async function removeEvalVals(
    env: h.Env,
    editor: vscode.TextEditor
): Promise<void> {
    const allRange = new vscode.Range(
        editor.document.positionAt(0),
        editor.document.positionAt(editor.document.getText().length)
    );
    decor.removeRange({
        decorations: env.evalDecorations,
        decoration: env.evalDecoration,
        editor,
        range: allRange,
    });
    decor.removeRange({
        decorations: env.evalErrorDecorations,
        decoration: env.evalErrorDecoration,
        editor,
        range: allRange,
    });
    env.outChannel.appendLine(
        `Remove eval decorations from ${editor.document.fileName}`
    );
    env.evalDecorations.delete(editor.document);
    env.evalErrorDecorations.delete(editor.document);
    editor.setDecorations(env.evalDecoration, []);
    editor.setDecorations(env.evalErrorDecoration, []);
}

/**
 * Return a `RegExp` to parse the output of a REPL evaluation or completion.
 * @param group The regexp string to match the actual result.
 * @returns A `RegExp` to parse the output of a REPL evaluation or completion.
 */
function matchREPLResponse(group: string): RegExp {
    return new RegExp(
        `(?:${c.replPrompt}\\s+)+${group}\\r?\\n${c.replPrompt}\\s+$`,
        "su"
    );
}

/**
 * Return the output of the Chez REPL after loading the file of `editor` and
 * evaluating `exp`.
 * @param config The extension's configuration.
 * @param document The source code containing the sexp to evaluate.
 * @param exp The sexp to evaluate.
 * @param checkForSave If `true`, check if the file has unsaved changes, and if
 * so, ask the user to save them.
 * @returns The output of the Chez REPL after loading the file of `editor` and
 * evaluating `exp`.
 */
// eslint-disable-next-line max-params
async function runREPLCommand(
    config: vscode.WorkspaceConfiguration,
    document: vscode.TextDocument,
    exp: string,
    checkForSave: boolean
): Promise<h.Output> {
    const root = await h.askForWorkspace("Scheme");
    if (document.isUntitled) {
        await document.save();
    }
    if (checkForSave && document.isDirty) {
        const response = await vscode.window.showWarningMessage(
            "The file has unsaved changes, these will not be send to the REPL.",
            "Save changes and eval",
            "Eval without saving"
        );
        if (response === "Save changes and eval") {
            await document.save();
        }
    }
    return h.runCommand({
        root: root ? root.uri.fsPath : "./",
        args: [c.replQuietArg],
        cmd: c.getCfgREPLPath(config),
        input: c.replLoadFileAndSexp(document.fileName, exp),
    });
}

/**
 * Return `true` if the configured Chez executable is working.
 * If an error occurred, the output is printed in the "OUTPUT" tab.
 * @param env The needed environment.
 * @returns `true` if the configured Chez executable is working, `false` else.
 */
export async function isSchemeWorking(env: h.Env): Promise<boolean> {
    const out = await runREPLVersion(env.config);
    if (out.stderr) {
        if (checkVersionOutput(out.stderr)) {
            env.outChannel.appendLine(
                `Command ${c.getCfgREPLPath(
                    env.config
                )} in version ${out.stderr.trim()} is working.`
            );
            return true;
        }
    }
    const errMsg =
        h.fromMaybe(out.error, "") + " " + h.fromMaybe(out.stderr, "");
    env.outChannel.appendLine(
        `Error running command ${c.getCfgREPLPath(env.config)}: ${errMsg}`
    );
    return false;
}

/**
 * Return the output of the Chez Scheme version command `scheme --version`.
 * @param config The extension's configuration.
 * @returns The output of the Chez Scheme version command `scheme --version`.
 */
async function runREPLVersion(
    config: vscode.WorkspaceConfiguration
): Promise<h.Output> {
    const root = await h.askForWorkspace("Scheme");
    return h.runCommand({
        root: root ? root.uri.fsPath : "./",
        args: [c.replVersionArg],
        cmd: c.getCfgREPLPath(config),
        input: "",
    });
}

/**
 * Return `true` if the output of the Chez Scheme version command if valid,
 * `false` else.
 * @param output The output of the Chez Scheme version command
 * `scheme --version` to check..
 * @returns `true` if the output of the Chez Scheme version command is valid,
 * `false` else.
 */
function checkVersionOutput(output: string): boolean {
    return versionOutRegex.test(output);
}
