/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     generate_function_documentation.ts
 * Date:     19.May.2023
 *
 * ==============================================================================
 * Parse the HTML documentation from the Chez Scheme page
 * https://cisco.github.io/ChezScheme/csug9.5/summary.html
 * and generate a list of objects holding the parsed data in `outFilename`.
 * Run with `yarn --ignore-engines ts-node generate_function_documentation.ts`.
 *
 * The Chez Scheme user's guide is licensed under the Apache License Version 2:
 * https://cisco.github.io/ChezScheme/csug9.5/canned/copyright.html.
 */

/* eslint-disable operator-linebreak */
/* eslint-disable indent */

import * as https from "https";
import {
    createWriteStream,
    existsSync,
    readFile,
    unlinkSync,
    writeFile,
} from "fs";
import { JSDOM } from "jsdom";
import { basename } from "path";

/**
 * The type of a identifier.
 * Warning: copied from `./src/identifierDocumentation.ts` because of problems
 * with module imports.
 */
type IdentifierType =
    | "syntax"
    | "module"
    | "procedure"
    | "thread parameter"
    | "global parameter"
    | "Error: unknown";

/**
 * Return the string `s` converted to an `IdentifierType`.
 * Return `"Error: unknown"` if the string isn't recognized.
 * @param s The string to convert to an `IdentifierType`.
 * @returns The string `s` converted to an `IdentifierType`.
 * Return `"Error: unknown"` if the string isn't recognized.
 */
function stringToIdentifierType(s: string): IdentifierType {
    switch (s) {
        case "syntax":
            return "syntax";
        case "procedure":
            return "procedure";
        case "module":
            return "module";
        case "thread param":
            return "thread parameter";
        case "global param":
            return "global parameter";
    }

    return "Error: unknown";
}

/**
 * The object to save the data of a function documentation to.
 * Warning: copied from `./src/identifierDocumentation.ts` because of problems
 * with module imports.
 */
type FunctionDoc = {
    name: string;
    startParen: boolean;
    endParen: boolean;
    params: string[];
    type: IdentifierType;
    moduleNames: string[];
    url: URL;
    description: string;
};

/**
 * The `RegExp` to match an example in a description.
 * The first group matches the whole example code.
 */
const exampleRegex = /\n((?:\\`.*?`\s*(?:=>|\*)?\s*<br>\n)+)(?:<br>)?\n*$/u;

/**
 * The `RegExp` to match a single line of a multi-line code example with many
 * individual backticks.
 * The first group contains the actual data.
 */
const lineFormatRegex = /^\\`(.*)\\`\s*<br>\s*$/gmu;

/**
 * The `RegExp` to match a `libraries` stanza in a description.
 * The first group contains the libraries names.
 */
const librariesRegex =
    /^\s*\*\*libraries:\*\*\s*(\\`\(.*?\)\\`(?:,\s*\\`\(.*?\)\\`)*)\s*<br>\s*$/mu;

/**
 * The base part of the Chez Scheme documentation URL.
 */
const baseURL = "https://cisco.github.io/ChezScheme/csug9.5/";

/**
 * The URL of the Chez Scheme documentation website.
 */
const docURL = baseURL + "summary.html";

/**
 * The name of the file to save the parsed data to.
 */
const outFilename = "./src/functionDocumentation.ts";

/**
 * The list of downloaded files to delete when exiting the program.
 */
const filesToDelete: string[] = [];

/**
 * Main entry point.
 */
async function main(): Promise<void> {
    try {
        const htmlText = await downloadAndRead(docURL);
        const tsText = await processHTML(htmlText);
        await writeFunctionDocumentation(tsText);
        const deleteSet = new Set(filesToDelete);
        deleteSet.forEach((file) => unlinkSync(file));
    } catch (error) {
        console.error(
            `Caught "${error}" trying to process the HTML and saving it.`
        );
        process.exit(1);
    }
}

/**
 * Return the data of the Chez function documentation as Typescript objects in a
 * text file.
 * @param text The HTML documentation file to parse.
 * @returns The data of the Chez function documentation as Typescript objects in a
 * text file.
 */
async function processHTML(text: string): Promise<string> {
    const htmlDoc = new JSDOM(text).window.document;
    const trs = Array.from(htmlDoc.querySelectorAll("tr")).filter(
        // eslint-disable-next-line no-magic-numbers
        (e) => e.childElementCount === 3 && e.children[0].nodeName !== "TH"
    );
    const ids: FunctionDoc[] = trs.map((tr) => parseTR(tr));
    const allUrl = Array.from(
        new Set(
            ids.map((id) => id.url.protocol + id.url.hostname + id.url.pathname)
        )
    );
    await Promise.all(
        allUrl.map((url) =>
            download(new URL(url), fileNameFromURL(new URL(url)))
        )
    );
    await Promise.all(ids.map((id) => addDescription(id)));

    ids.forEach((id) => addLibraries(id));

    return idsDocToTSFile(ids);
}

/**
 * Return a TS file content with the list of `FunctionDoc`s.
 * @param ids The list of `FunctionDoc`s to convert.
 * @returns A TS file content with the list of `FunctionDoc`s.
 */
function idsDocToTSFile(ids: FunctionDoc[]): string {
    const today = new Date();
    const date = today.getDate();
    const month = today.getMonth() + 1;
    const year = today.getFullYear();

    return `/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     functionDocumentation.ts
 * Date:     ${date}.${month}.${year}
 *
 * ==============================================================================
 * The Chez Scheme user's guide is licensed under the Apache License Version 2:
 * https://cisco.github.io/ChezScheme/csug9.5/canned/copyright.html.
 * Autogenerated by the script \`../generate_function_documentation.ts\`, from
 * ${docURL}
 * DO NOT EDIT!
 */

/* eslint-disable max-lines */

import { FunctionDoc } from "./identifierDocumentation";

export const functionDocs: FunctionDoc[] = [
${ids
    .map(
        (id) =>
            `    {
        name: "${id.name}",
        startParen: ${id.startParen},
        endParen: ${id.endParen},
        params: ["${id.params.join('", "')}"],
        type: "${id.type}",
        moduleNames: ["${id.moduleNames.join('", "')}"],
        url: new URL("${id.url}"),
        description: \`${id.description}\`
    },`
    )
    .join("\n")}
]
`;
}

/**
 * Parse the given `tr` element and add the date to `ids`.
 * @param tr The `tr` element to parse.
 * @returns The filled `FunctionDoc` object.
 */
// eslint-disable-next-line max-statements, max-lines-per-function
function parseTR(tr: HTMLTableRowElement): FunctionDoc {
    const tds = Array.from(tr.childNodes) as HTMLTableCellElement[];
    const idType = stringToIdentifierType(tds[1].innerHTML);
    let name = "";
    let params: string[] = [];
    let startParen = false;
    let endParen = false;
    const nameElems = tds[0].childNodes[0].childNodes;
    if (idType === "global parameter" || idType === "thread parameter") {
        const tmpName = stringOrEmpty(nameElems[0].textContent);
        startParen = tmpName.startsWith("(");
        name = startParen ? tmpName.slice(1) : tmpName;
    } else if (nameElems.length > 1) {
        ({ startParen, name, endParen } = parseParamsAndName({
            nameElems,
            startParen,
            name,
            params,
            endParen,
        }));
    } else {
        name = stringOrEmpty(nameElems[0].textContent);
    }
    const url = new URL(
        // eslint-disable-next-line no-magic-numbers, dot-notation
        (tds[2].childNodes[0] as HTMLAnchorElement).href.startsWith("./")
            ? // eslint-disable-next-line no-magic-numbers
              baseURL + (tds[2].childNodes[0] as HTMLAnchorElement).href
            : // eslint-disable-next-line no-magic-numbers
              (tds[2].childNodes[0] as HTMLAnchorElement).href
    );
    url.protocol = "https";
    return {
        name,
        startParen,
        endParen,
        type: idType,
        moduleNames: [],
        params,
        url,
        description: "",
    };
}

/**
 * Return the parsed data in the object
 * `{ startParen: boolean; name: string; endParen: boolean }`.
 * @param data The data needed for this function.
 * @returns The parsed data in the object
 * `{ startParen: boolean; name: string; endParen: boolean }`.
 */
// eslint-disable-next-line max-statements
function parseParamsAndName(data: {
    nameElems: NodeListOf<ChildNode>;
    startParen: boolean;
    name: string;
    params: string[];
    endParen: boolean;
}): { startParen: boolean; name: string; endParen: boolean } {
    const tmpName = stringOrEmpty(data.nameElems[0].textContent);
    data.startParen = tmpName.startsWith("(");
    data.name = data.startParen ? tmpName.slice(1) : tmpName;

    // eslint-disable-next-line no-plusplus
    for (let nameIdx = 1; nameIdx < data.nameElems.length - 1; nameIdx++) {
        if (data.nameElems[nameIdx].nodeName === "I") {
            data.params.push(
                stringOrEmpty(data.nameElems[nameIdx].textContent)
            );
        }
    }
    const end = stringOrEmpty(
        data.nameElems[data.nameElems.length - 1].textContent
    ).trim();
    if (end.endsWith(")")) {
        if (end !== ")") {
            data.params.push(end.slice(0, -1).trim());
        }
        data.endParen = true;
    }
    return {
        startParen: data.startParen,
        name: data.name,
        endParen: data.endParen,
    };
}

/**
 * Download the identifier's description from the URL in the `FunctionDoc`,
 * parse it and save it into the field `id.description`.
 * @param id The `FunctionDoc` to process.
 */
// eslint-disable-next-line max-statements
async function addDescription(id: FunctionDoc) {
    const htmlString = await downloadAndRead(id.url.toString());
    const htmlDoc = new JSDOM(htmlString).window.document;
    const anchor = id.url.hash.slice(1);
    let currP = htmlDoc.querySelector(`a[name="${anchor}"]`)?.closest("p");
    const first = currP;
    const text = [""];
    while (
        currP &&
        (currP === first ||
            // eslint-disable-next-line no-eq-null, eqeqeq
            currP.querySelector(`a[name]:not(a[name="${anchor}"])`) == null)
    ) {
        currP.childNodes.forEach((c) => parseChildNode(c, text));
        text.push("<br>\n");
        currP = currP.nextElementSibling as HTMLParagraphElement;
    }
    id.description = sanitizeDescription(text.join(""));
}

/**
 * Parse the `FunctionDoc` description and add the needed libraries as
 * `moduleNames` to the object.
 * @param id The `FunctionDoc` object to process.
 */
function addLibraries(id: FunctionDoc) {
    const match = id.description.match(librariesRegex);
    if (match) {
        // eslint-disable-next-line prefer-destructuring
        const librariesRaw = match[1];
        const libraries = librariesRaw.replace(/\\`/gu, "").split(/,\s*/gu);
        id.moduleNames = libraries;
    } else {
        id.moduleNames = [];
    }
}

/**
 * Parse a single HTML node of the description.
 * @param c The HTML node to parse.
 * @param text The description's text to append to.
 */
// eslint-disable-next-line max-lines-per-function
function parseChildNode(c: ChildNode, text: string[]) {
    switch (c.nodeName) {
        case "BR":
            text.push(`<br>\n`);
            break;
        case "B":
            text.push(`**${c.textContent?.replace(/\n/gu, " ").trim()}** `);
            break;
        case "TT":
            c.childNodes.forEach((cN) => {
                switch (cN.nodeName) {
                    case "BR":
                        text.push(`<br>\n`);
                        break;
                    case "B":
                        text.push(
                            `**${cN.textContent
                                ?.replace(/\n/gu, " ")
                                .trim()}** `
                        );
                        break;
                    case "I":
                        text.push(
                            "*`" +
                                `${cN.textContent
                                    ?.replace(/\n/gu, " ")
                                    .trim()}` +
                                "`*"
                        );
                        break;
                    case "IMG":
                        if ((cN as HTMLImageElement).src.endsWith("0.gif")) {
                            text.push("=>");
                        }
                        break;
                    case "#text":
                        text.push(
                            // eslint-disable-next-line no-useless-concat
                            "`" +
                                `${cN.textContent?.replace(/\n/gu, " ")}` +
                                "`"
                        );

                        break;
                }
            });
            break;
        case "#text":
            text.push(`${c.textContent?.replace(/\n/gu, " ")}`);
            break;
        case "SPAN":
            c.childNodes.forEach((cN) => parseChildNode(cN, text));
            break;
    }
}

/**
 * Return a sanitized version of the given text.
 * That is, without excessive whitespace and with escaped backticks and
 * backslashes. Also puts examples at the end into one big code block instead of
 * many individual backticks.
 * @param text The description text to sanitize.
 * @returns The sanitized description.
 */
function sanitizeDescription(text: string): string {
    let sanitized = text
        .replace(/[ ]+/gu, " ")
        .replace(/^ /gmu, "")
        .replace(/[ ]+\n/gu, "\n")
        .replace(/\n[\n]+$/u, "\n")
        .replace(/\n\n[\n]+/gu, "\n\n")
        // Non-breaking-space.
        .replace(/\u00A0/gu, " ")
        .replace(/\\/gu, "\\\\")
        .replace(/`/gu, "\\`");
    const match = sanitized.match(exampleRegex);
    if (match) {
        // eslint-disable-next-line prefer-destructuring
        const example = match[1];
        const exampleNoBackticks = example
            .replace(lineFormatRegex, "$1")
            .replace(/\\`/gu, "")
            .replace(/^ /gmu, "");
        sanitized = sanitized.replace(
            example,
            "**Examples:**\n\n\\`\\`\\`scheme\n" +
                exampleNoBackticks +
                "\n\\`\\`\\`\n"
        );
    }
    return sanitized;
}

/**
 * Return the string `s` if it isn't `undefined` or `null`, the empty string
 * `""` else. Changes all non breaking spaces (`\u00A0`) to "normal" spaces.
 * @param s The `string` or `undefined` value to "convert".
 * @returns The string `s` if it isn't `undefined` or `null`, the empty string
 * `""` else.
 */
function stringOrEmpty(s: string | undefined | null): string {
    return s ? s.replace(/\u00A0/gu, " ") : "";
}

/**
 * Return the filename to use for the downloaded file from the given URL.
 * @param url The URL to generate the filename from.
 * @returns The filename to use for the downloaded file from the given URL.
 */
function fileNameFromURL(url: URL) {
    return url.hostname + basename(url.pathname);
}

/**
 * Download the given URL and return the content of the file.
 * Exits the program if the download fails. Deletes the downloaded file after
 * reading it
 * @param url The URL of the website to download.
 * @returns The content of the downloaded file.
 */
async function downloadAndRead(url: string) {
    const urlUrl = new URL(url);
    const downloadTo = fileNameFromURL(urlUrl);
    if (!existsSync(downloadTo)) {
        try {
            await download(urlUrl, downloadTo);
        } catch (exp) {
            console.error(`Caught "${exp}" trying to download from ${url}`);
            process.exit(1);
        }
    }
    filesToDelete.push(downloadTo);
    return new Promise<string>((resolve, reject) => {
        readFile(downloadTo, { encoding: "utf8" }, (r, d) => {
            if (r) {
                reject(r);
            }
            resolve(d);
        });
    });
}

/**
 * Download a file to the given path `fileName`.
 * @param url The URL to download.
 * @param fileName The path to save the downloaded file to.
 * @returns Nothing.
 */
async function download(url: URL, fileName: string): Promise<void> {
    const fileStream = createWriteStream(fileName);
    return new Promise<void>((resolve, reject) => {
        https.get(url, (res) => {
            res.pipe(fileStream);
            res.on("error", (e) => reject(e));
            fileStream.on("finish", () =>
                fileStream.close((err) => {
                    if (err) {
                        reject(err);
                    }
                    resolve();
                })
            );
        });
    });
}

/**
 * Write the parsed data to the file `outFilename`.
 * @param text The text to save.
 */
async function writeFunctionDocumentation(text: string): Promise<void> {
    return new Promise<void>((resolve, reject) => {
        writeFile(outFilename, text, { encoding: "utf8" }, (r) => {
            if (r) {
                reject(r);
            }
            resolve();
        });
    });
}

main();
