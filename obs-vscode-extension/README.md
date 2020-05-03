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
* Go to the "Run" menu on the left 
* First, start "Launch client"
* The extension will open in a new VSCode window
* Now select "Attach to server"
* You can now trigger breakpoints, etc. 

To install:
* vsce package
* code --install-extension obsidian-\*.vsix 
* Restart VSCode.

## Citations

* The tmLanguage code was taken and modified from the VSCode Java language extension at https://github.com/microsoft/vscode/blob/master/extensions/java/syntaxes/java.tmLanguage.json

* This package was generated using VSCode's Yeoman extension generator
