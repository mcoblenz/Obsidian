# VS Code Syntax Highlighting for Obsidian Language

## About the Files

* `package.json` - manifest file language support is declared in, defines the location of the grammar file
* `syntaxes/obs.tmLanguage.json` - Text mate grammar file used for tokenization
* `language-configuration.json` - defines the tokens used for comments and brackets

## Setup
* Install Node.js: http://nodejs.org
* npm install -g typescript
* npm install -g vsce

## Usage

Compiling: 
* npm install 
* npm run compile

To develop the extension/LSP:
* code -n . (to open the project in vscode)
* npm run compile to rebuild (VS Code does not automatically rebuild the extension when you run the debugger)
* Go to the "Run" menu on the left 
* First, start "Launch client". Or, to see server messages, instead choose "Client + Server" and skip "Attach to server" below.
* The extension will open in a new VSCode window
* Now select "Attach to server"
* You can now trigger breakpoints, etc. 
* Log to console from server with console.log. Messages appear in the "Attach to server" section of the Debug Console in the host VSCode (not the one being debugged).

To install:
* vsce package
* code --install-extension obsidian-\*.vsix 
* Restart VSCode.

## Citations

* The tmLanguage code was taken and modified from the VSCode Java language extension at https://github.com/microsoft/vscode/blob/master/extensions/java/syntaxes/java.tmLanguage.json

* This package was generated using VSCode's Yeoman extension generator
