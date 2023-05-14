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
- [Configuration](#configuration)
- [Changes](#changes)
- [Contributing](#contributing)
- [License](#license)

## Features and drawbacks

### Drawbacks

## Getting started

### Dependencies

- Visual Studio Code version 1.65 (February 2022) or higher
- [Chez Scheme](https://github.com/cisco/chezscheme). For Mac OS use the [Racket version of Chez Scheme](https://github.com/racket/ChezScheme)

**Attention:** you must be in a trusted workspace. Tests (test runners) can execute arbitrary code, so you do **not** want to run them in untrusted directories!

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

## Configuration

- `chezScheme.schemePath` - Path to the Chez Scheme executable `scheme`. Can be either an absolute path or relative to the workspace root. Default: `scheme`, which works if `scheme` is in your `PATH`.

## Changes

See file [CHANGELOG.md](CHANGELOG.md).

## Contributing

See file [CONTRIBUTING.md](CONTRIBUTING.md)

## License

Chez Scheme REPL for Visual Studio Code is licensed under MIT license. See file [LICENSE](LICENSE)
