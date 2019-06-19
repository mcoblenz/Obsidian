# Obsidian
Obsidian language development

See http://obsidian-lang.com.

###### Preliminary setup
1. Install IntelliJ and the Scala plugin (https://www.jetbrains.com/idea/download/).
2. Install protoc (https://github.com/protocolbuffers/protobuf). On macOS: `brew install protoc`.

###### Compiler usage instructions
1. Open the Obsidian project in IntellJ (sorry, the compiler is not packaged to be run as a standalone executable yet).
2. Run > Edit Configurations…
3. New Application Configuration.
4. Set the main class as `edu.cmu.cs.obsidian.Main`
5. Edit the configurations to run the program with the following arguments:
`PATH_TO_OBS_CODE.obs`
6. Run the program
7. A folder named after the input class will be generated at the root of the directory containing the structure needed for Fabric deployment

To generate the Fabric structure elsewhere, pass the `output-path` argument with the path to the directory.

###### Fabric deployment
To use the chaincode on Fabric, some pre-requisites have to be met. First of all, you should have Docker installed on your machine. Then:
1. In a terminal, go to the root of the Obsidian project folder.
2. Run the following command: `curl -sSL http://bit.ly/2ysbOFE | bash -s 1.4.1 -s`

This installs all the platform-specific binaries you need to run a Fabric network and places them in the `bin` sub-directory of the Obsidian project.
It also downloads all the required Docker images and places them in your local Docker registry, tagged as `latest`.
For detailed instructions, go to `https://hyperledger-fabric.readthedocs.io/en/release-1.4/install.html`.

To deploy and invoke the generated chaincode in a real Fabric environment, follow these steps:
1. Generate the chaincode following the _compiler usage instructions_ above.
2. Go into the `network-framework` folder and run the command `./up.sh -s PATH_TO_CHAINCODE`, where the path is from the root of the repository, i.e if the folder was generated with default settings, you simply specify the name of the folder (ex: StringContainer)
3. Run `./invoke.sh FUNCTION_NAME ARG1 ARG2 ...`, for instance: `./invoke.sh setS randomstring`
4. After you are done, run `./down.sh` to kill and cleanup all Docker containers.

If you wish to upgrade the chaincode on the network without destroying and recreating the entire network, you can run `./upgrade.sh`.
This command uses the same path to the chaincode that you originally uploaded, so there is no need to pass any arguments.

###### Runtime .jar generation (for those working on Obsidian itself)
If changes are made to the Obsidian_Runtime code, a new .jar has to be generated and published since the Fabric chaincode relies on it.
To do so, go to the Obsidian_Runtime folder and run `gradle publish`.
Then, push the modified .jar to the Git master branch, and it will be available online at `http://obsidian-lang.com/repository`

