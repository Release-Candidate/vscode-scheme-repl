/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     textDecorations.ts
 * Date:     03.Jun.2023
 *
 * ==============================================================================
 * Functions and constants for use with "TextDecorations". That is, special
 * inline text to display evaluation results or evaluation error messages of
 * sexps.
 */

import * as c from "./constants";
import * as h from "./helpers";
import * as vscode from "vscode";

/**
 * Construct the color of the background or the border of an eval decoration.
 */
export const evalBackground = new vscode.ThemeColor(
    `${c.colorBaseName}.${c.colorEvalBackgroundName}`
);

/**
 * Construct the color of the background or the border of an eval decoration if
 * an error occurred.
 */
export const evalBackgroundError = new vscode.ThemeColor(
    `${c.colorBaseName}.${c.colorEvalErrorBackgroundName}`
);

/**
 * The CSS style of an eval decorator, for use with a light color theme.
 */
export const evalDecorationStyleLight =
    vscode.window.createTextEditorDecorationType({
        backgroundColor: evalBackground,
        after: {
            margin: "5px",
            backgroundColor: evalBackground,
        },
    });

/**
 * The CSS style of an eval decorator, for use with a dark color theme.
 */
export const evalDecorationStyleDark =
    vscode.window.createTextEditorDecorationType({
        backgroundColor: evalBackground,
        after: {
            margin: "5px",
            backgroundColor: evalBackground,
        },
    });

/**
 * The CSS style of an eval decorator, for use with a dark high contrast color
 * theme.
 */
export const evalDecorationStyleHCDark =
    vscode.window.createTextEditorDecorationType({
        borderColor: evalBackground,
        border: "1px solid",
        after: {
            margin: "5px",
            border: "1px solid",
            borderColor: evalBackground,
        },
    });

/**
 * The CSS style of an eval decorator, for use with a light high contrast color
 * theme.
 */
export const evalDecorationStyleHCLight =
    vscode.window.createTextEditorDecorationType({
        borderColor: evalBackground,
        border: "1px solid",
        after: {
            margin: "5px",
            border: "1px solid",
            borderColor: evalBackground,
        },
    });

/**
 * The CSS style of an eval error decorator, for use with a light color theme.
 */
export const evalErrorDecorationStyleLight =
    vscode.window.createTextEditorDecorationType({
        backgroundColor: evalBackgroundError,
        after: {
            margin: "5px",
            backgroundColor: evalBackgroundError,
        },
    });

/**
 * The CSS style of an eval error decorator, for use with a dark color theme.
 */

export const evalErrorDecorationStyleDark =
    vscode.window.createTextEditorDecorationType({
        backgroundColor: evalBackgroundError,
        after: {
            margin: "5px",
            backgroundColor: evalBackgroundError,
        },
    });

/**
 * The CSS style of an eval error decorator, for use with a dark high contrast
 *  color theme.
 */
export const evalErrorDecorationStyleHCDark =
    vscode.window.createTextEditorDecorationType({
        borderColor: evalBackgroundError,
        border: "1px solid",
        after: {
            margin: "5px",
            border: "1px solid",
            borderColor: evalBackgroundError,
        },
    });

/**
 * The CSS style of an eval error decorator, for use with a light high contrast
 *  color theme.
 */
export const evalErrorDecorationStyleHCLight =
    vscode.window.createTextEditorDecorationType({
        borderColor: evalBackgroundError,
        border: "1px solid",
        after: {
            margin: "5px",
            border: "1px solid",
            borderColor: evalBackgroundError,
        },
    });

/**
 * Return the `TextEditorDecorationType` colors and styles for the current
 * theme.
 * @returns The `TextEditorDecorationType` colors and styles for the current
 * theme.
 */
export function getEvalDecorationStyle(): vscode.TextEditorDecorationType {
    switch (h.getColorThemeKind()) {
        case "light":
            return evalDecorationStyleLight;
        case "dark":
            return evalDecorationStyleDark;
        case "hc-light":
            return evalDecorationStyleHCLight;
        case "hc-dark":
        default:
            return evalDecorationStyleHCDark;
    }
}

/**
 * Return the `TextEditorDecorationType` colors and styles for the current
 * theme, for evaluation errors.
 * @returns The `TextEditorDecorationType` colors and styles for the current
 * theme, for evaluation errors.
 */
export function getEvalErrorDecorationStyle(): vscode.TextEditorDecorationType {
    switch (h.getColorThemeKind()) {
        case "light":
            return evalErrorDecorationStyleLight;
        case "dark":
            return evalErrorDecorationStyleDark;
        case "hc-light":
            return evalErrorDecorationStyleHCLight;
        case "hc-dark":
        default:
            return evalErrorDecorationStyleHCDark;
    }
}

/**
 * Return the `DecorationOptions` for the evaluated sexp and the value of the
 * evaluation.
 * @param text The string to display in the decoration.
 * @param range The `Range` to apply the decoration to.
 * @returns The `DecorationOptions` for the evaluated sexp and the value of the
 * evaluation.
 */
export function textEvalDecoration(
    text: string,
    range: vscode.Range
): vscode.DecorationOptions {
    return {
        range,
        hoverMessage: `\`\`\`scheme\n${text}\n\`\`\``,
        renderOptions: {
            after: {
                contentText:
                    // eslint-disable-next-line no-useless-concat
                    " => " + text,
            },
        },
    };
}

/**
 * Return the `DecorationOptions` for the evaluated sexp and the error message
 * of the evaluation.
 * @param text The string to display in the decoration.
 * @param range The `Range` to apply the decoration to.
 * @returns The `DecorationOptions` for the evaluated sexp and the error message
 * of the evaluation.
 */
export function textEvalErrorDecoration(
    text: string,
    range: vscode.Range
): vscode.DecorationOptions {
    return {
        range,
        hoverMessage: text,
        renderOptions: {
            after: {
                contentText:
                    // eslint-disable-next-line no-useless-concat
                    " => " + text,
            },
        },
    };
}

/**
 * Add a text decoration with the string `data.text` to `data.editor`.
 * @param data The data needed to add the decoration.
 */
export function addEditorDecoration(data: {
    evalDecoration: vscode.TextEditorDecorationType;
    evalDecorations: WeakMap<vscode.TextDocument, vscode.DecorationOptions[]>;
    editor: vscode.TextEditor;
    range: vscode.Range;
    text: string;
}): void {
    data.editor.setDecorations(data.evalDecoration, []);
    const options = textEvalDecoration(data.text, data.range);
    const decoration = data.evalDecorations.get(data.editor.document);
    if (decoration) {
        decoration.push(options);
        data.evalDecorations.set(data.editor.document, decoration);
        data.editor.setDecorations(data.evalDecoration, decoration);
    } else {
        data.editor.setDecorations(data.evalDecoration, [options]);
        data.evalDecorations.set(data.editor.document, [options]);
    }
}
