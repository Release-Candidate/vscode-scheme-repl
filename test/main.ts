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
import * as glob from "glob";
import * as path from "path";

/**
 * The actual Mocha test runner.
 */
export function run(): Promise<void> {
    const mocha = new Mocha({
        ui: "tdd",
    });

    const testsRoot = path.resolve(__dirname, "..");

    return new Promise((c, e) => {
        // eslint-disable-next-line consistent-return
        glob("**/**-test.js", { cwd: testsRoot }, (err, files) => {
            if (err) {
                return e(err);
            }

            files.forEach((f) => mocha.addFile(path.resolve(testsRoot, f)));

            try {
                mocha.run((failures) => {
                    if (failures > 0) {
                        e(new Error(`${failures} tests failed.`));
                    } else {
                        c();
                    }
                });
            } catch (error) {
                console.error(error);
                e(error);
            }
        });
    });
}
