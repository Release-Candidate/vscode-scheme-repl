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

    await setupExtension(context, outChannel);

    outChannel.appendLine("Extension startup finished.");
}

/**
 * Setup the extension.
 * @param context The extension's context.
 * @param outChannel The channel to log to.
 */
// eslint-disable-next-line max-lines-per-function
async function setupExtension(
    context: vscode.ExtensionContext,
    outChannel: vscode.OutputChannel
) {
    const config = vscode.workspace.getConfiguration(c.cfgSection);

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
    context.subscriptions.push(symbolSubscription);

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

    context.subscriptions.push(hoverSubscription);

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
    context.subscriptions.push(completionSubscription);

    registerCommands({ config, outChannel, context });
}

/**
 * Register all commands that the extension provides with VS Code.
 * @param env The needed environment of the extension.
 */
function registerCommands(env: {
    config: vscode.WorkspaceConfiguration;
    outChannel: vscode.OutputChannel;
    context: vscode.ExtensionContext;
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

    const sendFileREPLCommand = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${c.sendFileToREPL}`,
        (editor) => pR.sendFileToRepl(env.config, env.outChannel, editor)
    );
    env.context.subscriptions.push(sendFileREPLCommand);

    env.outChannel.appendLine(`Registered all commands`);
}
