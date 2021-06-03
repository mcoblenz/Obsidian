Getting Started
===============

Installation
------------
- Check out the Obsidian project: `git clone https://github.com/mcoblenz/Obsidian.git`
- Install sbt (https://www.scala-sbt.org/release/docs/Setup.html).
- In the root of the Obsidian project folder, run `sbt assembly`. This will build the compiler, generating a Jar file in target/scala-2.12/.

Editor setup
------------
- Install nodejs (https://nodejs.org/en/).
- Install typescript (`npm install -g typescript`).
- Install VSCode (https://code.visualstudio.com) and make sure that the commandline tool it provides, `code` is in your path.
- Install vsce `npm install -g vsce`
- Go to the extension directory `extensions/obs-vscode-extension`
- Run `npm install`
- Run `npm run compile`
- Run `vsce package`
- Run `code --install-extension obs-vscode-extension/*.vsix`
- Use VSCode to edit Obsidian files, possibly after closing and reopening VSCode to refresh its extensions.

Running the compiler from VSCode
---------------------------------
- Make sure the bin directory of the Obsidian codebase is in your $PATH.
- Open a .obs file (Obsidian source code; you can start with a demo project in `resources/demos`).
- Use the Obsidian: Compile File command. On the Mac, you can find this by typing ⌘-Shift-P and typing "Obsidian".

Running the compiler from the command line
------------------------------------------
- To run the compiler, run `bin/obsidianc foo.obs`, where foo.obs is the path to the Obsidian file you want to compile. The file should include a main contract but can reference other files via `import`.
- A folder named after the input class will be generated at the root of the directory containing the structure needed for Fabric deployment

To generate the Fabric structure elsewhere, pass `--output-path` with the path to the directory.

Fabric deployment
------------------
To use the chaincode on Fabric, some pre-requisites have to be met. First of all, you should have Docker installed on your machine. Then:
- In a terminal, go to the root of the Obsidian project folder.
- Run the following command: `curl -sSL http://bit.ly/2ysbOFE | bash -s 1.4.4 -s`

This installs all the platform-specific binaries you need to run a Fabric network and places them in the `bin` sub-directory of the Obsidian project.
It also downloads all the required Docker images and places them in your local Docker registry, tagged as `latest`.
For detailed instructions, go to `https://hyperledger-fabric.readthedocs.io/en/release-1.4/install.html`.

To deploy and invoke the generated chaincode in a real Fabric environment, follow these steps:

- Generate the chaincode following the *compiler usage instructions* above.
- Go into the `network-framework` folder and run the command `./up.sh -s PATH_TO_CHAINCODE`, where the path is from the root of the repository, i.e if the folder was generated with default settings, you simply specify the name of the folder (ex: StringContainer)
- Run `./invoke.sh FUNCTION_NAME ARG1 ARG2 ...`, for instance: `./invoke.sh setS randomstring`
- After you are done, run `./down.sh` to kill and cleanup all Docker containers.

If you wish to upgrade the chaincode on the network without destroying and recreating the entire network, you can run `./upgrade.sh`.
This command uses the same path to the chaincode that you originally uploaded, so there is no need to pass any arguments.

Clients
---------
The most convenient way to build and run a client is via the VSCode extension. However, if you need to do it manually (perhaps for debugging purposes), you can follow these steps.

To build a client, pass --build-client to the compiler. 
The compiler will generate a directory that contains a build.gradle file.
To run the client:

- `sbt shadowJar -b <path/to/client/build.gradle>`
- cd path/to/client/build/lib/
- java -jar chaincode.jar
