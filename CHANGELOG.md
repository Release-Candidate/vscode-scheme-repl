# Chez Scheme REPL for Visual Studio Code Changelog

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
