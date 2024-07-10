/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     main.ts
 * Date:     14.May.2023
 *
 * ==============================================================================
 * The mocha test runner.
 */
import * as Mocha from "mocha";
import * as path from "path";
import { glob } from "glob";

/**
 * The actual Mocha test runner.
 */
export async function run(): Promise<void> {
    const mocha = new Mocha({
        ui: "tdd",
    });

    const testsRoot = path.resolve(__dirname, "..");

    const testFiles = await glob("**/**-test.js", { cwd: testsRoot });
    testFiles.forEach((f) => mocha.addFile(path.resolve(testsRoot, f)));
    return new Promise((resolve, reject) => {
        mocha.run((failures) => {
            if (failures > 0) {
                reject(new Error(`${failures} tests failed.`));
            } else {
                resolve();
            }
        });
    });
}
