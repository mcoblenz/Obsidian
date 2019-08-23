Contibuting to Obsidian
=======================

Setup
-----
- Install IntelliJ and the Scala plugin (https://www.jetbrains.com/idea/download/).
- Install protoc (https://github.com/protocolbuffers/protobuf). On macOS: `brew install protoc`.

Running the compiler in the IntelliJ debugger 
----------------------------------------------
- Run > Edit Configurations…
- New Application Configuration.
- Set the main class as `edu.cmu.cs.obsidian.Main`
- Edit the configurations to run the program with the following arguments: `PATH_TO_OBS_CODE.obs`
- A folder named after the input class will be generated at the root of the directory containing the structure needed for Fabric deployment.

Runtime library generation
---------------------------
If changes are made to the Obsidian_Runtime code, a new .jar has to be generated and published since the Fabric chaincode relies on it.
To do so, go to the Obsidian_Runtime folder and run `gradle publish`.
Then, push the modified .jar to the Git master branch, and it will be available online at `http://obsidian-lang.com/repository`.

You can test your changes locally by publishing to a private repository and editing fabric/build.gradle to point there.

Compiler Configuration
-----------------------

Environment Variables:
- `OBSIDIAN_COMPILER_DIR`: The root directory of the compiler repository. Needed to be able to locate some compiler resources.
