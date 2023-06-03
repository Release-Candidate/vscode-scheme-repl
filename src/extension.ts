/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     extension.ts
 * Date:     14.May.2023
 *
 * ==============================================================================
 * The main extension file.
 */

import * as c from "./constants";
import * as decor from "./textDecorations";
import * as h from "./helpers";
import * as iD from "./identifierDocumentation";
import * as pR from "./paneREPL";
import * as sexp from "./sexps";
import * as vscode from "vscode";
import { functionDocs } from "./functionDocumentation";

/**
 * Called when the extension is being activated.
 * That is, the registered `Activation Event` has happened. The
 * `Activation Events` are configured in `package.json`, in the
 * `activationEvents` field.
 * @param context The `vscode.ExtensionContext` to use.
 */
export async function activate(context: vscode.ExtensionContext) {
    const outChannel = vscode.window.createOutputChannel(c.outputChannelName);
    outChannel.appendLine("Chez Scheme REPL starting.");

    await setupExtension({
        context,
        outChannel,
        evalDecoration: decor.getEvalDecorationStyle(),
        evalErrorDecoration: decor.getEvalErrorDecorationStyle(),
    });

    outChannel.appendLine("Extension startup finished.");
}

/**
 * Setup the extension.
 * @param env The extension's needed environment.
 */
// eslint-disable-next-line max-lines-per-function, max-statements
async function setupExtension(env: {
    context: vscode.ExtensionContext;
    outChannel: vscode.OutputChannel;
    evalDecoration: vscode.TextEditorDecorationType;
    evalErrorDecoration: vscode.TextEditorDecorationType;
}) {
    const config = vscode.workspace.getConfiguration(c.cfgSection);

    const evalDecorations = new WeakMap<
        vscode.TextDocument,
        vscode.DecorationOptions[]
    >();

    const evalErrorDecorations = new WeakMap<
        vscode.TextDocument,
        vscode.DecorationOptions[]
    >();

    const editorChangedSubscription = vscode.window.onDidChangeActiveTextEditor(
        (editor) => {
            if (editor) {
                const decorations = evalDecorations.get(editor.document);
                if (decorations) {
                    editor.setDecorations(env.evalDecoration, decorations);
                }
                const errorDecorations = evalErrorDecorations.get(
                    editor.document
                );
                if (errorDecorations) {
                    editor.setDecorations(
                        env.evalErrorDecoration,
                        errorDecorations
                    );
                }
            }
        }
    );
    env.context.subscriptions.push(editorChangedSubscription);

    const symbolSubscription = vscode.languages.registerDocumentSymbolProvider(
        c.languageName,
        {
            provideDocumentSymbols(document, token) {
                document.getText();
                if (token.isCancellationRequested) {
                    return [];
                }
                return [];
            },
        }
    );
    env.context.subscriptions.push(symbolSubscription);

    const hoverSubscription = vscode.languages.registerHoverProvider(
        c.languageName,
        {
            provideHover(document, position, token) {
                const word = h.getWordAtPosition(document, position);
                // eslint-disable-next-line no-eq-null, eqeqeq
                if (word == null) {
                    return undefined;
                }
                const wordRegex = new RegExp(
                    `^[(]?${h.escapeRegexp(word)}(:?\\s+.*)?[)]?$`,
                    "u"
                );
                const funcID = functionDocs.find((id) =>
                    id.name.match(wordRegex)
                );
                if (token.isCancellationRequested) {
                    return undefined;
                }
                return funcID
                    ? new vscode.Hover(iD.functionDocToMarkdown(funcID))
                    : undefined;
            },
        }
    );

    env.context.subscriptions.push(hoverSubscription);

    const completionSubscription =
        vscode.languages.registerCompletionItemProvider(c.languageName, {
            provideCompletionItems(
                document: vscode.TextDocument,
                position: vscode.Position
            ) {
                const word = h.getWordAtPosition(document, position);
                const wordRegex = new RegExp(
                    `^[(]?${h.escapeRegexp(h.fromMaybe(word, ""))}(:?.*)?[)]?$`,
                    "u"
                );
                const funcIDs = word
                    ? functionDocs.filter((id) => id.name.match(wordRegex))
                    : functionDocs;
                const completions = funcIDs.map((id) =>
                    iD.functionDocToCompletionItem(id)
                );
                return completions;
            },
        });
    env.context.subscriptions.push(completionSubscription);

    registerCommands({
        config,
        outChannel: env.outChannel,
        context: env.context,
        evalDecoration: env.evalDecoration,
        evalDecorations,
        evalErrorDecoration: env.evalErrorDecoration,
        evalErrorDecorations,
    });
}

/**
 * Register all commands that the extension provides with VS Code.
 * @param env The needed environment of the extension.
 */
// eslint-disable-next-line max-statements, max-lines-per-function
function registerCommands(env: {
    config: vscode.WorkspaceConfiguration;
    outChannel: vscode.OutputChannel;
    context: vscode.ExtensionContext;
    evalDecoration: vscode.TextEditorDecorationType;
    evalDecorations: WeakMap<vscode.TextDocument, vscode.DecorationOptions[]>;
    evalErrorDecoration: vscode.TextEditorDecorationType;
    evalErrorDecorations: WeakMap<
        vscode.TextDocument,
        vscode.DecorationOptions[]
    >;
}) {
    const replCommand = vscode.commands.registerCommand(
        `${c.cfgSection}.${c.startREPLCommand}`,
        () => {
            pR.createREPL(env.config);
            env.outChannel.appendLine(
                `REPL started by command ${c.cfgSection}.${c.startREPLCommand}`
            );
        }
    );
    env.context.subscriptions.push(replCommand);

    const sendREPLCommand = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${c.sendSelectionToREPL}`,
        (editor) => pR.sendSelectionToRepl(env.config, env.outChannel, editor)
    );
    env.context.subscriptions.push(sendREPLCommand);

    const sendREPLLastCommand = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${c.sendLastToREPL}`,
        (editor) => pR.sendLastToRepl(env.config, env.outChannel, editor)
    );
    env.context.subscriptions.push(sendREPLLastCommand);

    const evalLastCommand = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${c.evalLast}`,
        // eslint-disable-next-line max-statements, max-lines-per-function
        async (editor) => {
            const selectedRange = h.rangeFromPositions(
                [0, 0],
                editor.selection.end
            );
            const selectedText = editor.document.getText(selectedRange);
            if (selectedText.length) {
                const exp = sexp.getSexpToLeft(selectedText);
                const root = await h.askForWorkspace("Scheme");
                const out = await h.runCommand({
                    root: root ? root.uri.fsPath : "./",
                    args: ["-q"],
                    cmd: "scheme",
                    input: `(load "${editor.document.fileName}" (lambda (x) (pretty-print x) (eval x))) ${exp.sexp}`,
                });
                const match = out.stdout ? out.stdout.match(/>([^>]+)$/u) : "";
                const response = match ? match[1].trim() : "";
                env.outChannel.appendLine(
                    `Sent ${exp.sexp} to REPL using command ${c.cfgSection}.${c.evalLast}`
                );
                editor.setDecorations(env.evalDecoration, []);
                const options: vscode.DecorationOptions = {
                    range: h.rangeFromPositions(
                        [exp.startLine, exp.startCol],
                        editor.selection.end
                    ),
                    hoverMessage: response,
                    renderOptions: {
                        after: {
                            contentText:
                                // eslint-disable-next-line no-useless-concat
                                " => " + response,
                        },
                    },
                };
                const decoration = env.evalDecorations.get(editor.document);
                if (decoration) {
                    decoration.push(options);
                    env.evalDecorations.set(editor.document, decoration);
                    editor.setDecorations(env.evalDecoration, decoration);
                } else {
                    editor.setDecorations(env.evalDecoration, [options]);
                    env.evalDecorations.set(editor.document, [options]);
                }
            } else {
                env.outChannel.appendLine(
                    `Not sent ${editor.selection.end.line}:${editor.selection.end.character} to REPL using command ${c.cfgSection}.${c.sendLastToREPL}`
                );
            }
        }
    );
    env.context.subscriptions.push(evalLastCommand);

    const evalSelection = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${c.evalSelection}`,
        // eslint-disable-next-line max-statements, max-lines-per-function
        async (editor) => {
            const selectedRange = h.rangeFromPositions(
                editor.selection.start,
                editor.selection.end
            );
            const selectedText = editor.document.getText(selectedRange);
            if (selectedText.length) {
                const exp = sexp.getSexpToLeft(selectedText);
                const root = await h.askForWorkspace("Scheme");
                const out = await h.runCommand({
                    root: root ? root.uri.fsPath : "./",
                    args: ["-q"],
                    cmd: "scheme",
                    input: `(load "${editor.document.fileName}" (lambda (x) (pretty-print x) (eval x))) ${exp.sexp}`,
                });
                env.outChannel.appendLine(
                    `Sent ${exp} to REPL using command ${c.cfgSection}.${c.evalSelection}`
                );
                if (out.stderr) {
                    const errMsg = out.stderr.trim();
                    editor.setDecorations(env.evalErrorDecoration, []);
                    const options = decor.textEvalErrorDecoration(
                        errMsg,
                        selectedRange
                    );
                    const decoration = env.evalErrorDecorations.get(
                        editor.document
                    );
                    if (decoration) {
                        decoration.push(options);
                        env.evalErrorDecorations.set(
                            editor.document,
                            decoration
                        );
                        editor.setDecorations(
                            env.evalErrorDecoration,
                            decoration
                        );
                    } else {
                        editor.setDecorations(env.evalErrorDecoration, [
                            options,
                        ]);
                        env.evalErrorDecorations.set(editor.document, [
                            options,
                        ]);
                    }
                } else {
                    const match = out.stdout
                        ? out.stdout.match(/>([^>]+)$/u)
                        : "";
                    const response = match ? match[1].trim() : "";
                    editor.setDecorations(env.evalDecoration, []);
                    const options = decor.textEvalDecoration(
                        response,
                        selectedRange
                    );
                    const decoration = env.evalDecorations.get(editor.document);
                    if (decoration) {
                        decoration.push(options);
                        env.evalDecorations.set(editor.document, decoration);
                        editor.setDecorations(env.evalDecoration, decoration);
                    } else {
                        editor.setDecorations(env.evalDecoration, [options]);
                        env.evalDecorations.set(editor.document, [options]);
                    }
                }
            } else {
                env.outChannel.appendLine(
                    `Not sent ${editor.selection.end.line}:${editor.selection.end.character} to REPL using command ${c.cfgSection}.${c.evalSelection}`
                );
            }
        }
    );
    env.context.subscriptions.push(evalSelection);

    const sendFileREPLCommand = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${c.sendFileToREPL}`,
        (editor) => pR.sendFileToRepl(env.config, env.outChannel, editor)
    );
    env.context.subscriptions.push(sendFileREPLCommand);

    env.outChannel.appendLine(`Registered all commands`);
}
