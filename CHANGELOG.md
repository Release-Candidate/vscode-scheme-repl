# Chez Scheme REPL for Visual Studio Code Changelog

## Version 0.7.4 (2024-08-27)

- Make the file icon SVG use a path instead of a font, so it renders correctly when the font is missing.

Special thanks to [monospod](https://github.com/monospod):

### Bugfixes

- Fix scheme executable path in Powershell on Windows. Quote the scheme executable path only if it contains whitespace. Always use `cmd.exe` to start the pane REPL on Windows.

### Internal Changes

- Update dependencies to new versions.

## Version 0.7.3 (2024-08-05)

Special thanks to [tracker1398](https://github.com/tracker1398):

### Bugfixes

- handle spaces in the Scheme executable path.

## Version 0.7.2 (2024-07-15)

Special thanks to [migraine-user](https://github.com/migraine-user) for helping with these:

### Bugfixes

- Do not check for unsaved changes for auto-completions, only when explicitly evaluating expressions.

## Version 0.7.1 (2024-07-13)

Special thanks to [migraine-user](https://github.com/migraine-user) for helping with these:

Add a popup warning if the file has unsaved changes before inline evaluating some expression.

### Bugfixes

- Fix the S-expression parser's handling of escaped quotes in strings

## Version 0.7.0 (2024-07-12)

New command `Chez Scheme REPL: Remove all evaluated values from the view.`, `chezScheme.removeEvalVals` to remove all evaluated values from the current view.

Add syntax highlighting for the new functions and parameters  of Chez 10.0 to the grammar.

### Bugfixes

Special thanks to [migraine-user](https://github.com/migraine-user) for helping to fix these, it would not have been possible to fix them by myself.

- Do not show more than one evaluated value for the same line: [#23](https://github.com/Release-Candidate/vscode-scheme-repl/issues/23)
- Remove all evaluation results from the view if the text has been changed:[#23](https://github.com/Release-Candidate/vscode-scheme-repl/issues/23)
- Check if the configured Scheme executable is working: [#20](https://github.com/Release-Candidate/vscode-scheme-repl/issues/20). If not, display an error popup.
- Save the current Scheme file before evaluating if it doesn't exist yet: [#20](https://github.com/Release-Candidate/vscode-scheme-repl/issues/20).

### Internal Changes

- Update dependencies to new versions

## Version 0.6.0 (2024-05-04)

Add support for Chez Scheme 10.

### Internal Changes

- Change package manager from `yarn` to `npm`.
- Update dependencies to new versions.

## Version 0.5.0 (2023-09-13)

Add support for syntax highlighting in Markdown code fences using the `scheme` language id.

### Internal Changes

- Update dependencies to new versions

## Version 0.4.2 (2023-07-22)

Version bump for the Open VSX registry only, a bug in the registry broke the publishing of updates.
See [https://github.com/EclipseFdn/open-vsx.org/issues/2000](https://github.com/EclipseFdn/open-vsx.org/issues/2000)

## Version 0.4.1 (2023-07-17)

### Bugfixes

- Make the package smaller by not including GIFs.

### Internal Changes

- Update dependencies to new versions and make Yarn compatible to version 3

## Version 0.4.0 (2023-06-26)

- Make the delay between starting the terminal for the interactive REPL and sending data to this terminal configurable. Configuration value `chezScheme.replDelay`. See [#5](https://github.com/Release-Candidate/vscode-scheme-repl/issues/5).
- If the extension's configuration has changed, pop up a window asking the user to reload the extension to activate the changes.

## Version 0.3.0 (2023-06-26)

### Bugfixes

- Fix [#3](https://github.com/Release-Candidate/vscode-scheme-repl/issues/3). Windows line endings - `\r\n` instead of `\n` break parsing of sexps for **evalLastSexp**, only the last line of a sexp is evaluated if the sexp spans multiple lines.

## Version 0.2.0 (2023-06-26)

### Bugfixes

- Fix [#1](https://github.com/Release-Candidate/vscode-scheme-repl/issues/1). Evaluating does not work on Windows, because backslashes in Windows file paths are not escaped. Leads to Chez error messages like `Exception in read: invalid string character \U` for a file path `C:\User\...`.

## Version 0.1.0 (2023-06-23)

Initial release
