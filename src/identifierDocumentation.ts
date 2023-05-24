/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     identifierDocumentation.ts
 * Date:     24.May.2023
 *
 * ==============================================================================
 * Types and functions to deal with identifier documentation which is contained
 * in the file `./functionDocumentation`.
 */

import * as vscode from "vscode";

/**
 * The type of a identifier.
 */
export type IdentifierType =
    | "syntax"
    | "module"
    | "procedure"
    | "thread parameter"
    | "global parameter"
    | "Error: unknown";

/**
 * The object to save the data of a function documentation to.
 */
export type FunctionDoc = {
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
 * Convert the given `IdentifierType` to a `CompletionItemKind`.
 * @param identifierType The `IdentifierType` to convert.
 * @returns The given `IdentifierType` converted to a `CompletionItemKind`.
 */
export function identifierToCompletionKind(
    identifierType: IdentifierType
): vscode.CompletionItemKind {
    switch (identifierType) {
        case "global parameter":
            return vscode.CompletionItemKind.Value;
        case "module":
            return vscode.CompletionItemKind.Module;
        case "procedure":
            return vscode.CompletionItemKind.Function;
        case "syntax":
            return vscode.CompletionItemKind.Keyword;
        case "thread parameter":
            return vscode.CompletionItemKind.Value;
        case "Error: unknown":
        default:
            return vscode.CompletionItemKind.User;
    }
}

/**
 * Return the Markdown formatted description of `funcId`.
 * @param funcID The `FunctionDoc` to return the Markdown description of.
 * @returns The Markdown formatted description of `funcId`.
 */
export function functionDocToMarkdown(
    funcID: FunctionDoc
): vscode.MarkdownString {
    let markdown = new vscode.MarkdownString(
        `\`\`\`scheme\n${funcID.startParen ? "(" : ""}${
            funcID.name
        }${funcID.params.join(" ")}${funcID.endParen ? ")" : ""}\n\`\`\`\n${
            funcID.description
        }\n[${funcID.url.toString()}](${funcID.url.toString()})`
    );
    markdown.supportHtml = true;
    return markdown;
}

/**
 * Return the `CompletionItem` of the given `funcId`.
 * @param funcID The `FunctionDoc` to return the completion object of.
 * @returns The `CompletionItem` of the given `funcId`.
 */
export function functionDocToCompletionItem(
    funcID: FunctionDoc
): vscode.CompletionItem {
    let completionItem = new vscode.CompletionItem(
        `${funcID.name}${funcID.params.join(" ")}`,
        identifierToCompletionKind(funcID.type)
    );
    completionItem.documentation = functionDocToMarkdown(funcID);
    completionItem.insertText = functionDocToSnippet(funcID);
    return completionItem;
}

/**
 * Return a `SnippetString` of the given `FunctionDoc`.
 * @param funcID The identifier documentation to make a snippet of.
 * @returns A `SnippetString` of the given `FunctionDoc`.
 */
export function functionDocToSnippet(
    funcID: FunctionDoc
): vscode.SnippetString {
    const paramSnippet = funcID.params.reduce(
        (acc, id, idx) => acc + `${idx === 0 ? "" : " "}\${${idx + 1}:${id}}`,
        ""
    );
    return new vscode.SnippetString(`${funcID.name}${paramSnippet}`);
}
