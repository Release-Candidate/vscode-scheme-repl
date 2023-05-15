/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     paneREPL.ts
 * Date:     15.May.2023
 *
 * ==============================================================================
 * Functions to handle the visible interactive REPL running in an extra pane.
 * Send files, sexps, ... to it.
 */

import * as c from "./constants";
import * as vscode from "vscode";

/**
 * Send the contents of `editor` to the interactive REPL.
 * @param config The extension's configuration object.
 * @param outChannel Logs go here.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export function sendFileToRepl(
    config: vscode.WorkspaceConfiguration,
    outChannel: vscode.OutputChannel,
    editor: vscode.TextEditor
) {
    const fileText = editor.document.getText();
    if (fileText.length) {
        const repl = createREPL(config);
        repl.sendText(fileText);
    }
    outChannel.appendLine(
        `Sent to REPL using command ${c.cfgSection}.${c.sendSelectionToREPL}`
    );
}

/**
 * Send a single selected sexp to the interactive REPL.
 * @param config The extension's configuration object.
 * @param outChannel Logs go here.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export function sendSelectionToRepl(
    config: vscode.WorkspaceConfiguration,
    outChannel: vscode.OutputChannel,
    editor: vscode.TextEditor
) {
    const selectedRange = new vscode.Range(
        editor.selection.start,
        editor.selection.end
    );
    const selectedText = editor.document.getText(selectedRange);
    if (selectedText.length) {
        const repl = createREPL(config);
        repl.sendText(selectedText);
    }
    outChannel.appendLine(
        `Sent ${editor.selection.start.character} - ${editor.selection.end.character} to REPL using command ${c.cfgSection}.${c.sendSelectionToREPL}`
    );
}

/**
 * Send the sexp to the left of the cursor to the interactive REPL.
 * @param config The extension's configuration object.
 * @param outChannel Logs go here.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export function sendLastToRepl(
    config: vscode.WorkspaceConfiguration,
    outChannel: vscode.OutputChannel,
    editor: vscode.TextEditor
) {
    const selectedRange = new vscode.Range(
        editor.selection.start,
        editor.selection.end
    );
    const selectedText = editor.document.getText(selectedRange);
    if (selectedText.length) {
        const repl = createREPL(config);
        repl.sendText(selectedText);
    }
    outChannel.appendLine(
        `Sent ${editor.selection.start.character} - ${editor.selection.end.character} to REPL using command ${c.cfgSection}.${c.sendLastToREPL}`
    );
}

/**
 * Start the interactive REPL in a terminal if it isn't already running.
 * @param config The configuration holding the command to call the REPL with.
 * @returns The `Terminal` object of the running REPL.
 */
export function createREPL(config: vscode.WorkspaceConfiguration) {
    const replTerminals = vscode.window.terminals.filter(
        (term) => term.name === c.replTerminalName
    );
    if (replTerminals.length) {
        replTerminals[0].show();
        return replTerminals[0];
    }
    const terminal = vscode.window.createTerminal({
        name: c.replTerminalName,
        isTransient: true,
        location: { viewColumn: vscode.ViewColumn.Beside, preserveFocus: true },
    });
    terminal.sendText(`${c.getCfgREPLPath(config)}`);
    return terminal;
}
