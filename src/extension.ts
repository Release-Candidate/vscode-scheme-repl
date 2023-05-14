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
import * as vscode from "vscode";

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
async function setupExtension(
    context: vscode.ExtensionContext,
    outChannel: vscode.OutputChannel
) {
    const config = vscode.workspace.getConfiguration(c.cfgSection);

    const replCommand = vscode.commands.registerCommand(
        `${c.cfgSection}.${c.startREPLCommand}`,
        () => {
            createREPL(config);
        }
    );

    const sendREPLCommand = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${c.sendSelectionToREPL}`,
        (editor) => {
            const selectedRange = new vscode.Range(
                editor.selection.start,
                editor.selection.end
            );
            const selectedText = editor.document.getText(selectedRange);
            if (selectedText.length) {
                const repl = createREPL(config);
                repl.sendText(selectedText);
            }
        }
    );

    const sendFileREPLCommand = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${c.sendFileToREPL}`,
        (editor) => {
            const fileText = editor.document.getText();
            if (fileText.length) {
                const repl = createREPL(config);
                repl.sendText(fileText);
            }
        }
    );

    context.subscriptions.push(replCommand);
    context.subscriptions.push(sendREPLCommand);
    context.subscriptions.push(sendFileREPLCommand);

    outChannel.appendLine("OK");
}

/**
 * Start the REPL in a terminal.
 * @param config The configuration holding the command to call the REPL with.
 * @returns The `Terminal` object.
 */
function createREPL(config: vscode.WorkspaceConfiguration) {
    const replTerminals = vscode.window.terminals.filter(
        (term) => term.name === c.replTerminalName
    );
    if (replTerminals.length) {
        replTerminals[0].show();
        return replTerminals[0];
    }
    const terminal = vscode.window.createTerminal({
        name: c.replTerminalName,
        isTransient: false,
        location: { viewColumn: vscode.ViewColumn.Beside },
    });
    terminal.sendText(`${c.getCfgREPLPath(config)}`);
    return terminal;
}
