/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     helpers.ts
 * Date:     18.May.2023
 *
 * ==============================================================================
 * General helper functions
 */

/**
 * Do nothing for the given time `ms`.
 * @param ms The sleep time in milliseconds.
 */
export async function sleep(ms: number) {
    const timer = new Promise<void>((resolve) => {
        setTimeout(resolve, ms);
    });
    await timer;
}
