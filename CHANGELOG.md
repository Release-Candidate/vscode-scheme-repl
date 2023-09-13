# Chez Scheme REPL for Visual Studio Code Changelog

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
