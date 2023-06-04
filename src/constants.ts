/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     constants.ts
 * Date:     14.May.2023
 *
 * ==============================================================================
 * Constants used in the extensions.
 */

import * as vscode from "vscode";

/**
 * The version of VS Code to use for testing.
 */
export const vscodeVersion = "1.65.0";

/**
 * The name of the language this extension supports.
 */
export const languageName = "scheme";

/**
 * The name of the VS Code output channel - that's the `OUTPUT` tab of the
 * panel.
 */
export const outputChannelName = "Chez Scheme REPL";

/**
 * The name of the terminal the REPL runs in.
 */
export const replTerminalName = "Chez Scheme REPL";

/**
 * The command to start the Chez Scheme REPL.
 */
export const replCommand = "scheme";

/**
 * The time in milliseconds to wait for a new REPL before sending strings to it.
 */
export const replSleepTime = 1000;

/**
 *******************************************************************************
 * Command names
 */

/**
 * Start a REPL in a pane beside the current editor.
 */
export const startREPLCommand = "startREPL";

/**
 * Send the current selection to the REPL.
 */
export const sendSelectionToREPL = "sendSelectionToREPL";

/**
 * Eval the current selection.
 */

export const evalSelection = "evalSelection";

/**
 * Expand all macros in the current selection.
 */
export const expandSelection = "expandSelection";

/**
 * Return the command to macro-expand `sexp` in the REPL.
 * @param sexp The sexp to macro-expand.
 * @returns The command to macro-expand `sexp` in the REPL.
 */
export function expandSexp(sexp: string): string {
    return `(expand '${sexp})`;
}

/**
 * Send the current source file to the REPL.
 */
export const sendFileToREPL = "sendFileToREPL";

/**
 * Send the sexp left of the cursor to the REPL.
 */
export const sendLastToREPL = "sendLastSexp";

/**
 * Eval the sexp left of the cursor.
 */
export const evalLast = "evalLastSexp";

/**
 * Expand all macros in the sexp left of the cursor.
 */
export const expandLast = "expandLastSexp";

/**
 ******************************************************************************
 *  Color constants.
 */

/**
 * The 'root' part of the color identifier.
 */
export const colorBaseName = "chezScheme";

/**
 * The actual color identifier of the background or border color of an
 * evaluation.
 */
export const colorEvalBackgroundName = "evalBackgroundColor";

/**
 * The actual color identifier of the background or border color of an
 * evaluation.
 */
export const colorEvalErrorBackgroundName = "evalErrorBackgroundColor";

/**
 ******************************************************************************
 *  Configuration constants.
 */

/**
 * The name of the configuration section of the extension.
 */
export const cfgSection = "chezScheme";

/**
 * The path of the Chez Scheme REPL Executable.
 */
export const cfgREPLPath = "schemePath";

/**
 * The default value for the Chez Scheme REPL path.
 */
export const cfgREPLDefaultPath = replCommand;

/**
 * Return the configuration value for `schemePath`.
 * @param config The configuration object to use.
 * @returns The configuration value for `schemePath`.
 */
export function getCfgREPLPath(config: vscode.WorkspaceConfiguration) {
    return config.get<string>(cfgREPLPath) || cfgREPLDefaultPath;
}
