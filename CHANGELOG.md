# Chez Scheme REPL for Visual Studio Code Changelog

## Version 0.2.0 (2023-06-26)

### Bugfixes

- Fix [#1](https://github.com/Release-Candidate/vscode-scheme-repl/issues/1). Evaluating does not work on Windows, because backslashes in Windows file paths are not escaped. Leads to Chez error messages like `Exception in read: invalid string character \U` for a file path `C:\User\...`.

## Version 0.1.0 (2023-06-23)

Initial release
