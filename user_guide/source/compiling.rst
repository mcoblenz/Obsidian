Using the Compiler
===================
We recommend using VSCode. To set it up:

- Install VSCode (https://code.visualstudio.com).
- Copy the obs-vscode-extension directory into ~/.vscode/extensions.
- `cd ~/.vscode/extensions/obs-vscode-extension` and run `tsc` to build the extension.
- Use VSCode to open a directory containing Obsidian files.

You can use commands provided within VSCode to build and deploy an Obsidian smart contract to a local test network. You can also build and run client applications. In order to tell the extension which file contains the smart contract's main contract and which file contains the client's main transaction, you should provide a `project.json` file at the top level of your project directory. It should contain:

::

   {
      "chaincode": "<path-to-chaincode>",
      "client": "<path-to-client>.obs"
   }


Paths should be relative to the project.json file.