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
 * The name of the VS Code output channel - that's the `OUTPUT` tab of the
 * panel.
 */
export const outputChannelName = "Chez Scheme REPL";

/**
 * The command to start the Chez Scheme REPL.
 */
export const replCommand = "scheme";

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
export function getCfgDunePath(config: vscode.WorkspaceConfiguration) {
    return config.get<string>(cfgREPLPath) || cfgREPLDefaultPath;
}
