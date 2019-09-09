# VS Code Syntax Highlighting for Obsidian Language

## About the Files

* `package.json` - manifest file language support is declared in, defines the location of the grammar file
* `syntaxes/obs.tmLanguage.json` - Text mate grammar file used for tokenization
* `language-configuration.json` - defines the tokens used for comments and brackets

## Usage

* tsc
* vsce package
* code --install-extension obsidian-0.0.1.vsix 
* Restart VSCode.

## Citations

* The tmLanguage code was taken and modified from the VSCode Java language extension at https://github.com/microsoft/vscode/blob/master/extensions/java/syntaxes/java.tmLanguage.json

* This package was generated using VSCode's Yeoman extension generator
