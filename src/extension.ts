/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2023 Roland Csaszar
 *
 * Project:  vscode-ocaml-expect-inline
 * File:     extension.ts
 * Date:     28.Feb.2023
 *
 * ==============================================================================
 * The main extension file.
 * Implement VS Code's `Testing API`, see
 * https://code.visualstudio.com/api/extension-guides/testing.
 */

import * as c from "./constants";
import * as h from "./extension_helpers";
import * as p from "./parsing";
import * as parseSource from "./parse_source";
import * as rt from "./run_tests";
import * as t from "./list_tests";
import * as vscode from "vscode";

/**
 * Called when the extension is being activated.
 * That is, the registered `Activation Event` has happened. The
 * `Activation Events` are configured in `package.json`, in the
 * `activationEvents` field.
 *
 * @param context The `vscode.ExtensionContext` to use.
 */
export async function activate(context: vscode.ExtensionContext) {
    const outChannel = vscode.window.createOutputChannel(c.outputChannelName);
    outChannel.appendLine("OCaml Expect and Inline Test Explorer starting.");

    /*
     * If no workspace exists (that includes an opened folder), we can't do
     * anything sensible anyway.
     */
    if (!vscode.workspace.workspaceFolders) {
        outChannel.appendLine("Not in a workspace/no folder opened. Exiting.");
        return;
    }
    await setupExtension(context, outChannel);
    outChannel.appendLine("Extension startup finished.");
}

/**
 * Setup the extension and add the tests to the Text Explorer view.
 * Only add the tests if `discoverOnStartup` is set to `true`.
 * @param context The extension's context.
 * @param outChannel The channel to log to.
 */
async function setupExtension(
    context: vscode.ExtensionContext,
    outChannel: vscode.OutputChannel
) {
    const testData: h.TestData = new WeakMap();

    const config = vscode.workspace.getConfiguration(c.cfgSection);

    const controller = vscode.tests.createTestController(
        c.testControllerID,
        c.testControllerLabel
    );
    context.subscriptions.push(controller);

    const env = { config, controller, outChannel, testData };

    const runProfile = controller.createRunProfile(
        c.runProfileLabel,
        vscode.TestRunProfileKind.Run,
        (r, tok) => rt.runHandler(env, r, tok)
    );
    context.subscriptions.push(runProfile);

    if (c.getCfgDiscover(config)) {
        await t.addTests(env, h.workspaceFolders());
    }

    await subscribeToChanges(env, context);
}

/**
 * Subscribe to changes of workspaces, documents, ...
 * @param env The extension's environment.
 * @param context The extension's context to use.
 */
async function subscribeToChanges(
    env: h.Env,
    context: vscode.ExtensionContext
) {
    // eslint-disable-next-line no-unused-vars
    env.controller.refreshHandler = async (_) => {
        t.addTests(env, h.workspaceFolders());
    };

    const configDisposable = vscode.workspace.onDidChangeConfiguration((e) =>
        configChanged(env, e)
    );
    context.subscriptions.push(configDisposable);

    const disposable = vscode.workspace.onDidChangeWorkspaceFolders(
        async (e) => {
            if (c.getCfgDiscover(env.config)) {
                t.addTests(env, e.added);
            }
            e.removed.map((r) => env.controller.items.delete(r.name));
        }
    );
    context.subscriptions.push(disposable);

    if (c.getCfgDiscoverInSources(env.config)) {
        registerSourceChanges(env, context);
    }
}

/**
 * Register callbacks if a text editor has been opened or saved.
 * @param env The extension's environment.
 * @param context The extension context.
 */
function registerSourceChanges(env: h.Env, context: vscode.ExtensionContext) {
    if (!c.getCfgDiscover(env.config)) {
        const disposable2 = vscode.workspace.onDidOpenTextDocument(
            async (e) => {
                if (p.isOCamlFile(e.uri.path)) {
                    await parseSource.parseTextDocument(env, e);
                }
            }
        );
        context.subscriptions.push(disposable2);
    }
    const disposable3 = vscode.workspace.onDidSaveTextDocument(async (e) => {
        if (p.isOCamlFile(e.uri.path)) {
            await parseSource.parseTextDocument(env, e);
        }
    });
    context.subscriptions.push(disposable3);
}

/**
 * Called, if the configuration has changed.
 * @param env The extension's environment.
 * @param e The change event.
 */
function configChanged(env: h.Env, e: vscode.ConfigurationChangeEvent) {
    if (e.affectsConfiguration(c.cfgSection)) {
        env.outChannel.appendLine(`Config changed!`);
        vscode.window
            .showInformationMessage(
                "The configuration has changed!\nReload the window for the changes to take effect.",
                "Reload Now"
            )
            // eslint-disable-next-line no-unused-vars
            .then((_) =>
                vscode.commands.executeCommand("workbench.action.reloadWindow")
            );
    }
}
