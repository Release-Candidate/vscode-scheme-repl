/*
 * SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
 * SPDX-License-Identifier: MIT
 *
 * Project:  vscode-scheme-repl
 * File:     extension.ts
 * Date:     14.May.2023
 *
 * ==============================================================================
 * The main extension file.
 */

import * as c from "./constants";
import * as decor from "./textDecorations";
import * as eR from "./evalREPL";
import * as h from "./helpers";
import * as iD from "./identifierDocumentation";
import * as pR from "./paneREPL";
import * as vscode from "vscode";
import { functionDocs } from "./functionDocumentation";

/**
 * Called when the extension is being activated.
 * That is, the registered `Activation Event` has happened. The
 * `Activation Events` are configured in `package.json`, in the
 * `activationEvents` field.
 * @param context The `vscode.ExtensionContext` to use.
 */
export async function activate(context: vscode.ExtensionContext) {
    const outChannel = vscode.window.createOutputChannel(c.outputChannelName);
    outChannel.appendLine("Chez Scheme REPL starting.");

    const config = vscode.workspace.getConfiguration(c.cfgSection);

    const evalDecorations = new WeakMap<
        vscode.TextDocument,
        vscode.DecorationOptions[]
    >();

    const evalErrorDecorations = new WeakMap<
        vscode.TextDocument,
        vscode.DecorationOptions[]
    >();

    const diagnostics = vscode.languages.createDiagnosticCollection(
        c.diagnosticsCollName
    );

    await setupExtension({
        config,
        context,
        outChannel,
        diagnostics,
        evalDecoration: decor.getEvalDecorationStyle(),
        evalDecorations,
        evalErrorDecoration: decor.getEvalErrorDecorationStyle(),
        evalErrorDecorations,
    });

    outChannel.appendLine("Extension startup finished.");
}

/**
 * Setup the extension.
 * @param env The extension's environment.
 */
async function setupExtension(env: h.Env) {
    const editorChangedSubscription = vscode.window.onDidChangeActiveTextEditor(
        (editor) => {
            textEditorChanged(editor, env);
        }
    );
    env.context.subscriptions.push(editorChangedSubscription);

    const symbolSubscription = vscode.languages.registerDocumentSymbolProvider(
        c.languageName,
        { provideDocumentSymbols }
    );
    env.context.subscriptions.push(symbolSubscription);

    const hoverSubscription = vscode.languages.registerHoverProvider(
        c.languageName,
        { provideHover }
    );
    env.context.subscriptions.push(hoverSubscription);

    const completionSubscription =
        vscode.languages.registerCompletionItemProvider(c.languageName, {
            provideCompletionItems: (document, position) =>
                provideCompletionItems(env, document, position),
        });
    env.context.subscriptions.push(completionSubscription);

    registerCommands(env);
}

/**
 * Called if the current active editor has changed.
 * @param editor The current active editor.
 * @param env The extension's environment.
 */
function textEditorChanged(editor: vscode.TextEditor | undefined, env: h.Env) {
    if (editor) {
        redrawDecorations(editor, env);
    }
}

/**
 * Return a hover text containing the identifier's documentation or `undefined`.
 * @param document The source code to provide hover information for.
 * @param position The position of the identifier.
 * @param token Whether to cancel the hover action or not.
 * @returns a hover text containing the identifier's documentation or
 * `undefined`.
 */
function provideHover(
    document: vscode.TextDocument,
    position: vscode.Position,
    token: vscode.CancellationToken
): vscode.Hover | undefined {
    const word = h.getWordAtPosition(document, position);
    // eslint-disable-next-line no-eq-null, eqeqeq
    if (word == null) {
        return undefined;
    }
    const wordRegex = new RegExp(
        `^[(]?${h.escapeRegexp(word)}(:?\\s+.*)?[)]?$`,
        "u"
    );
    const funcID = functionDocs.find((id) => id.name.match(wordRegex));
    if (token.isCancellationRequested) {
        return undefined;
    }
    return funcID
        ? new vscode.Hover(iD.functionDocToMarkdown(funcID))
        : undefined;
}

/**
 * Return a list of completions of the partial identifier at `position`.
 * @param env The needed environment.
 * @param document The current document containing the source code.
 * @param position The position of the partial identifier to complete.
 * @returns a list of completions of the partial identifier at `position`.
 */
async function provideCompletionItems(
    env: h.Env,
    document: vscode.TextDocument,
    position: vscode.Position
): Promise<vscode.CompletionItem[]> {
    const word = h.getWordAtPosition(document, position);
    const wordRegex = new RegExp(
        `^[(]?${h.escapeRegexp(h.fromMaybe(word, ""))}(:?.*)?[)]?$`,
        "u"
    );
    if (word) {
        const funcIDs = functionDocs.filter((id) => id.name.match(wordRegex));
        const completions = funcIDs.map((id) =>
            iD.functionDocToCompletionItem(id)
        );
        const local = await eR.evalGetIds(env, document, word);
        local.forEach((id) => {
            if (!iD.isIdInList(id, funcIDs)) {
                completions.push(new vscode.CompletionItem(id));
            }
        });
        return completions;
    } else {
        return functionDocs.map((id) => iD.functionDocToCompletionItem(id));
    }
}

/**
 * Return the list of identifiers of the source `document`.
 * TODO.
 * @param document The source code to provide identifier information of.
 * @param token Whether to cancel the action or not.
 * @returns The list of identifiers of the source `document`.
 */
function provideDocumentSymbols(
    document: vscode.TextDocument,
    token: vscode.CancellationToken
) {
    document.getText();
    if (token.isCancellationRequested) {
        return [];
    }
    return [];
}

/**
 * Redraw all saved decorations (evaluation results).
 * @param editor The current active editor.
 * @param env The needed environment.
 */
function redrawDecorations(editor: vscode.TextEditor, env: h.Env): void {
    const decorations = env.evalDecorations.get(editor.document);
    if (decorations) {
        editor.setDecorations(env.evalDecoration, decorations);
    }
    const errorDecorations = env.evalErrorDecorations.get(editor.document);
    if (errorDecorations) {
        editor.setDecorations(env.evalErrorDecoration, errorDecorations);
    }
}

/**
 * Register all commands that the extension provides with VS Code.
 * @param env The needed environment of the extension.
 */
function registerCommands(env: h.Env): void {
    const replCommand = vscode.commands.registerCommand(
        `${c.cfgSection}.${c.startREPLCommand}`,
        () => {
            pR.createREPL(env.config);
            env.outChannel.appendLine(
                `REPL started by command ${c.cfgSection}.${c.startREPLCommand}`
            );
        }
    );
    env.context.subscriptions.push(replCommand);

    registerTextEditorCommand(
        env,
        c.sendSelectionToREPL,
        pR.sendSelectionToRepl
    );

    registerTextEditorCommand(env, c.expandSelection, pR.expandSelectionInRepl);

    registerTextEditorCommand(env, c.sendLastToREPL, pR.sendLastToRepl);

    registerTextEditorCommand(env, c.expandLast, pR.expandLastInRepl);

    registerTextEditorCommand(env, c.evalLast, eR.evalLastSexp);

    registerTextEditorCommand(env, c.evalSelection, eR.evalSelectedSexp);

    registerTextEditorCommand(env, c.sendFileToREPL, pR.sendFileToRepl);

    env.outChannel.appendLine(`Registered all commands`);
}

/**
 * Bind a VS Code command to a function.
 * @param env The needed environment of the extension.
 * @param commandString The command ID to register.
 * @param f The function to bind the command to.
 */
function registerTextEditorCommand(
    env: h.Env,
    commandString: string,
    f: (
        // eslint-disable-next-line no-unused-vars
        fEnv: h.Env,
        // eslint-disable-next-line no-unused-vars
        editor: vscode.TextEditor
    ) => Promise<void>
): void {
    const subscription = vscode.commands.registerTextEditorCommand(
        `${c.cfgSection}.${commandString}`,
        (editor) => f(env, editor)
    );
    env.context.subscriptions.push(subscription);
}
