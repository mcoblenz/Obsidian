# Obsidian
Obsidian language development

See http://obsidian-lang.com.

###### Usage instructions
1. Set the main class as `edu.cmu.cs.obsidian.Main`
2. Edit the configurations to run the program with the following arguments:
`PATH_TO_OBS_CODE.obs --hyperledger --lazy`
3. Run the program
4. A folder named after the input class will be generated at the root of the directory containing the structure needed for Fabric deployment

To generate the Fabric structure elsewhere, pass the `output-path` argument with the path to the directory.

###### Runtime .jar generation
If changes are made to the Obsidian_Runtime code, a new .jar has to be generated and published since the Fabric chaincode relies on it.
To do so, go to the Obsidian_Runtime folder and run `gradle publish`.
Then, push the modified .jar to the Git master branch, and it will be available online at `http://obsidian-lang.com/repository`

###### Fabric deployment
To use the chaincode on Fabric, some pre-requisites have to be met. First of all, you should have Docker installed on your machine. Then:
1. In a terminal, go to the root of the Obsidian project folder.
2. Run the following command: `curl -sSL http://bit.ly/2ysbOFE | bash -s 1.4.0`

This installs all the platform-specific binaries you need to run a Fabric network and places them in the `bin` sub-directory of the Obsidian project.
It also downloands all the required Docker images and places them in your local Docker registry, tagged as `latest`.
For detailed instructions, go to `https://hyperledger-fabric.readthedocs.io/en/release-1.4/install.html`.

To deploy and invoke the generated chaincode in a real Fabric environment, follow these steps:
1. Generate the chaincode following the `Usage instructions` above.
2. In the `first-network` folder, open `docker-compose-cli.yaml` and change the two instances of `{{FOLDER_NAME}}` line 84 to the name of the class you are generating.
3. In the same folder, in the sub-directory `scripts`, open `script.sh`. Line 26, change `{{FOLDER_NAME}}` to the same value as in point 3.
4. To change the function that has to be invoked, open `utils.sh` in the `scripts` directory. Line 313, change the value of the `Args`. The value passed has to be an array of strings. The first argument is the name of the function, and then the following values are the arguments to this function. Example: `"Args":["sum", "10", "15"]`.

Now that all the setup is done, you can start using Fabric.
1. Go to the `first-network` folder in the Obsidian project
2. Run the following command `./byfn.sh generate`
3. On completion, run `./byfn.sh up`
4. If chaincode has changed or you want to invoke new functions, you always have to run `./byfn.sh down` before going back to step 2. 