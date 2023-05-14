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
    outChannel.appendLine("OK");
    outChannel.appendLine(`${context.extension}`);
}
