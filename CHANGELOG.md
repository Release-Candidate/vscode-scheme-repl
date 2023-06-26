# Chez Scheme REPL for Visual Studio Code Changelog

## Version 0.3.0 (2023-06-26)

### Bugfixes

- Fix [#3](https://github.com/Release-Candidate/vscode-scheme-repl/issues/3). Windows line endings - `\r\n` instead of `\n` break parsing of sexps for **evalLastSexp**, only the last line of a sexp is evaluated if the sexp spans multiple lines.

## Version 0.2.0 (2023-06-26)

### Bugfixes

- Fix [#1](https://github.com/Release-Candidate/vscode-scheme-repl/issues/1). Evaluating does not work on Windows, because backslashes in Windows file paths are not escaped. Leads to Chez error messages like `Exception in read: invalid string character \U` for a file path `C:\User\...`.

## Version 0.1.0 (2023-06-23)

Initial release
