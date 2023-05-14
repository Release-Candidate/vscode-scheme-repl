/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2023 Roland Csaszar
 *
 * Project:  vscode-ocaml-expect-inline
 * File:     main.ts
 * Date:     28.Feb.2023
 *
 * ==============================================================================
 * The mocha test runner.
 */
import Mocha = require("mocha");
import * as path from "path";
import glob from "glob";

/**
 * The actual Mocha test runner.
 */
export async function run(): Promise<void> {
    const mocha = new Mocha({
        ui: "tdd",
    });

    const testsRoot = path.resolve(__dirname, "..");

    return new Promise((c, e) => {
        // eslint-disable-next-line consistent-return
        glob("**/**-test.js", { cwd: testsRoot })
            .then((files) => {
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
            })
            .catch((err) => e(err));
    });
}
