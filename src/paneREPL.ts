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
import * as help from "./helpers";
import * as sexp from "./sexps";
import * as vscode from "vscode";

/**
 * Send the contents of `editor` to the interactive REPL.
 * @param config The extension's configuration object.
 * @param outChannel Logs go here.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export async function sendFileToRepl(
    config: vscode.WorkspaceConfiguration,
    outChannel: vscode.OutputChannel,
    editor: vscode.TextEditor
) {
    const fileText = editor.document.getText();
    if (fileText.length) {
        const repl = await createREPL(config);
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
export async function sendSelectionToRepl(
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
        const repl = await createREPL(config);
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
export async function sendLastToRepl(
    config: vscode.WorkspaceConfiguration,
    outChannel: vscode.OutputChannel,
    editor: vscode.TextEditor
) {
    const selectedRange = new vscode.Range(
        new vscode.Position(0, 0),
        editor.selection.end
    );
    const selectedText = editor.document.getText(selectedRange);
    if (selectedText.length) {
        const repl = await createREPL(config);
        repl.sendText(sexp.getSexpToLeft(selectedText).sexp);
        outChannel.appendLine(
            `Sent ${
                sexp.getSexpToLeft(selectedText).sexp
            } to REPL using command ${c.cfgSection}.${c.sendLastToREPL}`
        );
    } else {
        outChannel.appendLine(
            `Not sent ${editor.selection.end.line}:${editor.selection.end.character} to REPL using command ${c.cfgSection}.${c.sendLastToREPL}`
        );
    }
}

/**
 * Start the interactive REPL in a terminal if it isn't already running.
 * @param config The configuration holding the command to call the REPL with.
 * @returns The `Terminal` object of the running REPL.
 */
export async function createREPL(config: vscode.WorkspaceConfiguration) {
    const replTerminals = vscode.window.terminals.filter(
        (term) => term.name === c.replTerminalName
    );
    if (replTerminals.length) {
        replTerminals[0].show();
        return replTerminals[0];
    }
    const root = await help.askForWorkspace(
        "Select the Workspace to run the REPL in:"
    );
    const terminal = vscode.window.createTerminal({
        name: c.replTerminalName,
        isTransient: true,
        cwd: root ? root.uri.fsPath : "./",
        location: { viewColumn: vscode.ViewColumn.Beside, preserveFocus: true },
    });
    terminal.sendText(`${c.getCfgREPLPath(config)}`);
    await help.sleep(c.replSleepTime);
    return terminal;
}
