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
 * Send files, sexps, commands, ... to it.
 */

import * as c from "./constants";
import * as help from "./helpers";
import * as sexp from "./sexps";
import * as vscode from "vscode";

/**
 * Send the contents of `editor` to the interactive REPL.
 * @param env The needed environment.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export async function sendFileToRepl(
    env: {
        config: vscode.WorkspaceConfiguration;
        outChannel: vscode.OutputChannel;
    },
    editor: vscode.TextEditor
): Promise<void> {
    const fileText = editor.document.getText();
    if (fileText.length) {
        const repl = await createREPL(env.config);
        repl.sendText(fileText);
    }
    env.outChannel.appendLine(
        `Sent to REPL using command ${c.cfgSection}.${c.sendSelectionToREPL}`
    );
}

/**
 * Send a single selected sexp to the interactive REPL.
 * @param env The needed environment.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export async function sendSelectionToRepl(
    env: {
        config: vscode.WorkspaceConfiguration;
        outChannel: vscode.OutputChannel;
    },
    editor: vscode.TextEditor
): Promise<void> {
    doSelectionInRepl({
        config: env.config,
        outChannel: env.outChannel,
        editor,
        vscodeCommand: c.sendSelectionToREPL,
        f: help.id,
    });
}

/**
 * Send a single selected sexp to be macro expanded to the interactive REPL.
 * @param env The needed environment.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export async function expandSelectionInRepl(
    env: {
        config: vscode.WorkspaceConfiguration;
        outChannel: vscode.OutputChannel;
    },
    editor: vscode.TextEditor
): Promise<void> {
    doSelectionInRepl({
        config: env.config,
        outChannel: env.outChannel,
        editor,
        vscodeCommand: c.expandSelection,
        f: c.expandSexp,
    });
}

/**
 * Send the sexp to the left of the cursor to the interactive REPL.
 * @param env The needed environment.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export async function sendLastToRepl(
    env: {
        config: vscode.WorkspaceConfiguration;
        outChannel: vscode.OutputChannel;
    },
    editor: vscode.TextEditor
): Promise<void> {
    doLastInRepl({
        config: env.config,
        outChannel: env.outChannel,
        editor,
        f: help.id,
        vscodeCommand: c.sendLastToREPL,
    });
}

/**
 * Send the sexp to the left of the cursor to be macro expanded to the
 * interactive REPL.
 * @param env The needed environment.
 * @param editor The `TextEditor` containing the sources to send to the REPL.
 */
export async function expandLastInRepl(
    env: {
        config: vscode.WorkspaceConfiguration;
        outChannel: vscode.OutputChannel;
    },
    editor: vscode.TextEditor
): Promise<void> {
    doLastInRepl({
        config: env.config,
        outChannel: env.outChannel,
        editor,
        f: c.expandSexp,
        vscodeCommand: c.expandLast,
    });
}

/**
 * Send the result of applying `data.f` to the current selection to the pane REPL.
 * @param data The needed data.
 */
async function doSelectionInRepl(data: {
    config: vscode.WorkspaceConfiguration;
    outChannel: vscode.OutputChannel;
    editor: vscode.TextEditor;
    vscodeCommand: string;
    // eslint-disable-next-line no-unused-vars
    f: (selection: string) => string;
}): Promise<void> {
    const selectedRange = help.rangeFromPositions(
        data.editor.selection.start,
        data.editor.selection.end
    );
    const selectedText = data.editor.document.getText(selectedRange);
    if (selectedText.length) {
        const repl = await createREPL(data.config);
        const replCommand = data.f(selectedText.trim());
        repl.sendText(replCommand);
        data.outChannel.appendLine(
            `Sent ${replCommand} to REPL using command ${c.cfgSection}.${data.vscodeCommand}`
        );
    } else {
        data.outChannel.appendLine(
            `Not sent ${data.editor.selection.start.character} - ${data.editor.selection.end.character} to REPL using command ${c.cfgSection}.${data.vscodeCommand}`
        );
    }
}

/**
 * Send the result of applying `data.f` to the sexp to the left of the cursor to the
 * pane REPL.
 * @param data The needed data.
 */
async function doLastInRepl(data: {
    config: vscode.WorkspaceConfiguration;
    outChannel: vscode.OutputChannel;
    editor: vscode.TextEditor;
    vscodeCommand: string;
    // eslint-disable-next-line no-unused-vars
    f: (selection: string) => string;
}): Promise<void> {
    const selectedRange = help.rangeFromPositions(
        [0, 0],
        data.editor.selection.end
    );
    const selectedText = data.editor.document.getText(selectedRange);
    if (selectedText.length) {
        const repl = await createREPL(data.config);
        const replCommand = data.f(sexp.getSexpToLeft(selectedText).sexp);
        repl.sendText(replCommand);
        data.outChannel.appendLine(
            `Sent ${replCommand} to REPL using command ${c.cfgSection}.${data.vscodeCommand}`
        );
    } else {
        data.outChannel.appendLine(
            `Not sent ${data.editor.selection.end.line}:${data.editor.selection.end.character} to REPL using command ${c.cfgSection}.${data.vscodeCommand}`
        );
    }
}

/**
 * Start the interactive REPL in a terminal if it isn't already running.
 * @param config The configuration holding the command to call the REPL with.
 * @returns The `Terminal` object of the running REPL.
 */
export async function createREPL(
    config: vscode.WorkspaceConfiguration
): Promise<vscode.Terminal> {
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
    let terminalOpts: vscode.TerminalOptions = {
        name: c.replTerminalName,
        isTransient: true,
        cwd: root ? root.uri.fsPath : "./",
        location: {
            viewColumn: vscode.ViewColumn.Beside,
            preserveFocus: true,
        },
    };
    if (process.platform === "win32") {
        terminalOpts.shellPath = "cmd.exe";
    }
    const terminal = vscode.window.createTerminal(terminalOpts);
    const replPath = c.getCfgREPLPath(config);
    const whitespaceReg = /\s/g;
    if (replPath.search(whitespaceReg) < 0) {
        terminal.sendText(`${replPath}`);
    } else {
        terminal.sendText(`"${replPath}"`);
    }
    await help.sleep(c.getCfgREPLDelay(config));
    terminal.sendText(`${c.getCfgREPLPromptFunction(config)}`);
    return terminal;
}
