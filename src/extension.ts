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
import * as eR from "./evalREPL";
import * as h from "./helpers";
import * as iD from "./identifierDocumentation";
import * as pR from "./paneREPL";
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

    registerTextEditorCommand(
        env,
        c.sendSelectionToREPL,
        pR.sendSelectionToRepl
    );

    registerTextEditorCommand(env, c.expandSelection, pR.expandSelectionInRepl);

    registerTextEditorCommand(env, c.sendLastToREPL, pR.sendLastToRepl);

    registerTextEditorCommand(env, c.expandLast, pR.expandLastInRepl);

    registerTextEditorCommand(env, c.evalLast, eR.evalLastSexp);

    registerTextEditorCommand(env, c.evalSelection, eR.evalSelectedSexp);

    registerTextEditorCommand(env, c.sendFileToREPL, pR.sendFileToRepl);

    env.outChannel.appendLine(`Registered all commands`);
}

/**
 * Bind a VS Code command to a function.
 * @param env The needed environment of the extension.
 * @param commandString The command ID to register.
 * @param f The function to bind the command to.
 */
function registerTextEditorCommand(
    env: {
        config: vscode.WorkspaceConfiguration;
        outChannel: vscode.OutputChannel;
        context: vscode.ExtensionContext;
        evalDecoration: vscode.TextEditorDecorationType;
        evalDecorations: WeakMap<
            vscode.TextDocument,
            vscode.DecorationOptions[]
        >;
        evalErrorDecoration: vscode.TextEditorDecorationType;
        evalErrorDecorations: WeakMap<
            vscode.TextDocument,
            vscode.DecorationOptions[]
        >;
    },
    commandString: string,
    f: (
        // eslint-disable-next-line no-unused-vars
        fEnv: {
            config: vscode.WorkspaceConfiguration;
            outChannel: vscode.OutputChannel;
            evalDecoration: vscode.TextEditorDecorationType;
            evalDecorations: WeakMap<
                vscode.TextDocument,
                vscode.DecorationOptions[]
            >;
            evalErrorDecoration: vscode.TextEditorDecorationType;
            evalErrorDecorations: WeakMap<
                vscode.TextDocument,
                vscode.DecorationOptions[]
            >;
        },
        // eslint-disable-next-line no-unused-vars
        editor: vscode.TextEditor
    ) => Promise<void>
) {
    const subscription = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${commandString}`,
        (editor) => f(env, editor)
    );
    env.context.subscriptions.push(subscription);
}
