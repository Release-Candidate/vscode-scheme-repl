/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2023 Roland Csaszar
 *
 * Project:  vscode-ocaml-expect-inline
 * File:     generate_changelog.ts
 * Date:     28.Feb.2023
 *
 * ==============================================================================
 * Parse the latest changelog from the changelog file and save that.
 * Returns an error if the version given as command line argument doesn't match
 * the version of the latest changelog.
 * Run with `yarn --ignore-engines ts-node generate_changelog.ts`.
 */

import { readFile, writeFile } from "fs";

/**
 * The name of the file to save the latest changelog to.
 */
const outFilename = "latest_changelog.md";

/**
 * The index of the version command line argument.
 */
const idxVersionArg = 2;

/**
 * Regex to get a part of a changelog. The version is returned in group
 * `version`.
 */
const changelogRegex =
    /\n(##\s+Version\s+(?<version>[\p{N}\p{P}~]+)\s+.+?)(?:(?:\n##\s+Version)|$)/su;

/**
 * Main entry point.
 */
async function main() {
    const textProm = new Promise<string>((resolve, reject) => {
        readFile("./CHANGELOG.md", { encoding: "utf8" }, (r, d) => {
            resolve(d);
            reject(r);
        });
    });
    try {
        const text = await textProm;
        const match = text.match(changelogRegex);
        if (match) {
            const lastChangelog = processChangelog(match);
            await writeChangelog(lastChangelog);
        }
    } catch (error) {
        console.error(`Caught: ${error}`);
    }
    process.exit();
}

/**
 * Process the changelog, write to file if version matches command line argument.
 * If the version of the changelog is not the same as the command line argument,
 * exit this script with an error code.
 * @param match The match object to process.
 * @returns The latest part of the changelog.
 */
function processChangelog(match: RegExpMatchArray) {
    // eslint-disable-next-line prefer-destructuring, no-magic-numbers
    const lastChangelog = match[1];
    const version = match.groups ? match.groups.version : "";
    if (version !== process.argv[idxVersionArg]) {
        console.error(
            `ERROR: version mismatch: ${version} !== ${process.argv[idxVersionArg]}`
        );
        // eslint-disable-next-line no-magic-numbers
        process.exit(1);
    }
    return lastChangelog;
}

/**
 * Write the changelog to the file `outFilename`.
 * @param lastChangelog The changelog text to save.
 */
async function writeChangelog(lastChangelog: string) {
    const writeProm = new Promise<void>((_resolve, reject) => {
        writeFile(outFilename, lastChangelog, { encoding: "utf8" }, (r) => {
            if (r) {
                reject(r);
            }
        });
    });

    await writeProm;
}

main();
