# Chez Scheme REPL for Visual Studio Code

[![Test](https://github.com/Release-Candidate/vscode-scheme-repl/actions/workflows/test.yml/badge.svg)](https://github.com/Release-Candidate/vscode-scheme-repl/actions/workflows/test.yml)
[![Lint](https://github.com/Release-Candidate/vscode-scheme-repl/actions/workflows/lint.yml/badge.svg)](https://github.com/Release-Candidate/vscode-scheme-repl/actions/workflows/lint.yml)
[![Release](https://github.com/Release-Candidate/vscode-scheme-repl/actions/workflows/release.yml/badge.svg)](https://github.com/Release-Candidate/vscode-scheme-repl/actions/workflows/release.yml)
[![Visual Studio Marketplace Downloads](https://img.shields.io/visual-studio-marketplace/d/Release-Candidate.vscode-scheme-repl)](https://marketplace.visualstudio.com/items?itemName=release-candidate.vscode-scheme-repl)
[![Visual Studio Marketplace Installs](https://img.shields.io/visual-studio-marketplace/i/Release-Candidate.vscode-scheme-repl)](https://marketplace.visualstudio.com/items?itemName=release-candidate.vscode-scheme-repl)
[![Visual Studio Marketplace Version](https://img.shields.io/visual-studio-marketplace/v/Release-Candidate.vscode-scheme-repl)](https://marketplace.visualstudio.com/items?itemName=release-candidate.vscode-scheme-repl)
[![Open VSX Version](https://img.shields.io/open-vsx/v/Release-Candidate/vscode-scheme-repl)](https://open-vsx.org/extension/Release-Candidate/vscode-scheme-repl)

![Extension logo](./images/banner.png)

- [Features and drawbacks](#features-and-drawbacks)
  - [Drawbacks](#drawbacks)
- [Getting started](#getting-started)
  - [Dependencies](#dependencies)
    - [Suggested Additional VS Code Extensions](#suggested-additional-vs-code-extensions)
  - [Installation](#installation)
  - [Q \& A](#q--a)
  - [Commands](#commands)
  - [Supported Source File Extensions](#supported-source-file-extensions)
- [Configuration](#configuration)
- [Changes](#changes)
- [Contributing](#contributing)
- [License](#license)

## Features and drawbacks

- Hover with documentation of all Chez Scheme identifiers.
- Autocompletion with documentation of all Chez Scheme identifiers.
- Interactive REPL pane to the side of the editor, with commands to send code to the REPL (see [Commands](#commands)).

### Drawbacks

- The REPL pane is left empty on restart of VS Code and has to be manually closed. I do not know of a way to automatically close that on exit.

## Getting started

### Dependencies

- Visual Studio Code version 1.65 (February 2022) or higher
- [Chez Scheme](https://github.com/cisco/chezscheme). For Mac OS use the [Racket version of Chez Scheme](https://github.com/racket/ChezScheme)

#### Suggested Additional VS Code Extensions

- Syntax highlighting: [chez-scheme](https://marketplace.visualstudio.com/items?itemName=abhi18av-vscode.chez-scheme-vscode)
- Structural editing (Paredit): [Strict Paredit](https://marketplace.visualstudio.com/items?itemName=ailisp.strict-paredit)
- To display errors inline in the source: [Error Lens](https://marketplace.visualstudio.com/items?itemName=usernamehw.errorlens)

### Installation

Either

- install the extension directly from the Visual Studio Code Marketplace [Chez Scheme REPL](https://marketplace.visualstudio.com/items?itemName=release-candidate.vscode-scheme-repl)
- install the extension directly from the Open VSX Registry [Chez Scheme REPL](https://open-vsx.org/extension/Release-Candidate/vscode-scheme-repl)
- or download the extension from the [latest release at GitHub](https://github.com/Release-Candidate/vscode-scheme-repl/releases/latest)
- or build the extension yourself by cloning the [GitHub Repository](https://github.com/Release-Candidate/vscode-scheme-repl) and running `yarn install` and `yarn package` in the root directory of the cloned repo.

### Q & A

### Commands

- `Chez Scheme REPL: Start REPL` (`chezScheme.startREPL`) - start a Chez Scheme REPL in a terminal pane to the side of the current active window.
- `Chez Scheme REPL: Send selected s-expression to the REPL.` (`chezScheme.sendSelectionToREPL`) - send the current selected s-expression to the REPL. Opens a new REPL window if no REPL is running.
- `Chez Scheme REPL: Send s-expression left of the cursor to the REPL.` (`chezScheme.sendLastSexp`) - send the s-expression to the left of the cursor to the REPL. Opens a new REPL window if no REPL is running.
- `Chez Scheme REPL: Send the whole current file to the REPL` (`chezScheme.sendFileToREPL`) - send the contents of the currently active source file to the REPL. Opens a new REPL window if no REPL is running.
- `Chez Scheme REPL: Eval the selected s-expression.` (`chezScheme.evalSelection`) - evaluate the selected s-expression and print the value inline after the selected s-expression.
- `Chez Scheme REPL: Eval s-expression left of the cursor.` (`chezScheme.evalLastSexp`) - evaluate the s-expression to the left of the cursor and print the value inline after this s-expression.
- `Chez Scheme REPL: Expand all macros in the selected s-expression.` (`chezScheme.expandSelection`) - expand all macros in the selected s-expression in the REPL. Opens a new REPL window if no REPL is running.
- `Chez Scheme REPL: Expand all macros in the s-expression left of the cursor.` (`chezScheme.expandLastSexp`) - expand all macros in the s-expression to the left of the cursor in the REPL. Opens a new REPL window if no REPL is running.
- `Chez Scheme REPL: Check the current file for errors.` (`chezScheme.checkFile`) - check the current file by loading it into the REPL. Errors are shown in the `Problems` tab.

### Supported Source File Extensions

The following list of extensions is recognized as a Chez Scheme source file:

- `.ss`
- `.sps`
- `.scm`
- `.sch`
- `.sls`

## Configuration

- `chezScheme.schemePath` - Path to the Chez Scheme executable `scheme`. Can be either an absolute path or relative to the workspace root. Default: `scheme`, which works if `scheme` is in your `PATH`.
- `chezScheme.waiterPrompt` - The string to display as an interactive REPL prompt. Default: `λ>`.

## Changes

See file [CHANGELOG.md](CHANGELOG.md).

## Contributing

See file [CONTRIBUTING.md](CONTRIBUTING.md)

## License

Chez Scheme REPL for Visual Studio Code is licensed under MIT license. See file [LICENSE](LICENSE).

The documentation of the Chez Scheme identifiers is automatically generated from *Chez Scheme Version 9 User's Guide*. *Chez Scheme Version 9 User's Guide* is licensed under the Apache License Version 2, see [https://cisco.github.io/ChezScheme/csug9.5/canned/copyright.html](https://cisco.github.io/ChezScheme/csug9.5/canned/copyright.html)
