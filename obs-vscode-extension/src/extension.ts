// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as fs from 'fs'
import * as path from 'path'

import {
   LanguageClient,
   LanguageClientOptions,
   ServerOptions,
   TransportKind,
   RevealOutputChannelOn
} from 'vscode-languageclient';


interface ProjectConfig {
   chaincode: string; // Path within this project to the main chaincode contract
   client: string; // Path within this project to the main client contract
}

let taskProvider: vscode.Disposable | undefined;
let languageClient: LanguageClient | null = null;

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
   // Use the console to output diagnostic information (console.log) and errors (console.error)
   // This line of code will only be executed once when your extension is activated

   // The command has been defined in the package.json file
   // Now provide the implementation of the command with registerCommand
   // The commandId parameter must match the command field in package.json
   let compileCommand = vscode.commands.registerCommand('extension.compileObsidian', () => {
         // The code you place here will be executed every time your command is executed
            let task = getCompilationTask();
            if (task != undefined) {
               vscode.tasks.executeTask(task as vscode.Task).then(
                  compilationExecution => vscode.tasks.onDidEndTaskProcess(((e) => {
                     if (e.execution.task == task) {
                        if (e.exitCode == 0) {
                           vscode.window.showInformationMessage('Compilation succeeded.');
                        }
                        else {
                           vscode.window.showInformationMessage('Compilation failed.');
                        }
                     }
                  })
               ))
            }
         }       
   );

   context.subscriptions.push(compileCommand);

   let compileClientCommand = vscode.commands.registerCommand('extension.compileObsidianClient', () => {
      // The code you place here will be executed every time your command is executed
         let task = getClientCompilationTask();
         if (task != undefined) {
            vscode.tasks.onDidEndTaskProcess(((e) => {
               if (e.execution.task == task) {
                  if (e.exitCode == 0) {
                     vscode.window.showInformationMessage('Compilation succeeded.');
                  }
                  else {
                     vscode.window.showInformationMessage('Compilation failed.');
                  }
               }
            })
         )

         vscode.tasks.executeTask(task as vscode.Task)
      }
   }
   );       
   context.subscriptions.push(compileClientCommand);
      
   let deployCommand = vscode.commands.registerCommand('extension.deployObsidian', () => {
         let compilationTask = getCompilationTask();
         if (compilationTask != undefined) {
            // First, make sure the build is up to date.
            vscode.tasks.onDidEndTaskProcess((e) => {
               if (e.execution.task == compilationTask) {
                  if (e.exitCode == 0) {
                     // Then figure out what the name of the main contract was.
                     let contractName = chaincodeContractName()
                     if (contractName == undefined) {
                        vscode.window.showErrorMessage('Failed to find name of main contract.');
                     }
                     else {
                        let deployTask = getDeployObsidianTask(contractName);
                        if (deployTask != undefined) {
                           vscode.tasks.onDidEndTaskProcess(((e) => {
                              if (e.execution.task == deployTask) {
                                 if (e.exitCode == 0) {
                                    vscode.window.showInformationMessage('Fabric network ready to accept transaction requests.');
                                 }
                                 else {
                                    let downTask = getNetworkDownTask();
                                    vscode.tasks.onDidEndTaskProcess((e) => {
                                       if (e.execution.task == downTask) {
                                          vscode.window.showErrorMessage("Deployment failed. Consider trying again; sometimes this happens.")
                                       }
                                    })
                                    vscode.tasks.executeTask(downTask as vscode.Task)
                                 }
                              }
                           }
                           ))
                           vscode.tasks.executeTask(deployTask as vscode.Task)
                        }
                     }
                  }
                  else {
                     vscode.window.showErrorMessage('Compilation failed.');
                  }
               }
            })

            vscode.tasks.executeTask(compilationTask as vscode.Task)
         }
      }
   );       
   context.subscriptions.push(deployCommand);

   let downCommand = vscode.commands.registerCommand('extension.obsidianDown', () => {
      let task = getNetworkDownTask();
      if (task != undefined) {
         vscode.tasks.executeTask(task as vscode.Task).then(
            compilationExecution => vscode.tasks.onDidEndTaskProcess(((e) => {
               if (e.execution.task == task) {
                  if (e.exitCode == 0) {
                     vscode.window.showInformationMessage('Fabric network is now down.');
                  }
                  else {
                     vscode.window.showErrorMessage('Taking the network down failed.');
                  }
               }
            })
         ))
      }
   });

   context.subscriptions.push(downCommand);

   let runClientCommand = vscode.commands.registerCommand('extension.runObsidianClient', () => {
      let compilationTask = getClientCompilationTask();
      if (compilationTask != undefined) {
         // First, make sure the build is up to date.
         vscode.tasks.onDidEndTaskProcess((e) => {
            if (e.execution.task == compilationTask) {
               if (e.exitCode == 0) {
                  let contractName = clientContractName()
                  if (contractName != undefined) {
                     let runTask = getRunObsidianClientTask(contractName);
                     if (runTask != undefined) {
                        vscode.tasks.onDidEndTaskProcess(((e) => {
                           if (e.execution.task == runTask) {
                              if (e.exitCode == 0) {
                                 vscode.window.showInformationMessage('Client exited.');
                              }
                              else {
                                 vscode.window.showErrorMessage("Failed to run client successfully.")
                              }
                           }
                        }
                        ))
                        vscode.tasks.executeTask(runTask as vscode.Task)
                     }
                  }
               }
               else {
                  vscode.window.showErrorMessage('Compilation failed.');
               }
            }
         })

         vscode.tasks.executeTask(compilationTask as vscode.Task)
      }
   });

   // Initialize the LSP server, written in node
   const serverModule = context.asAbsolutePath(path.join("server", "out", "server.js"));
   // The debug options for the server
   // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
   const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

   // If the extension is launched in debug mode then the debug server options are used
   // Otherwise the run options are used
   const serverOptions: ServerOptions = {
      run: { module: serverModule, transport: TransportKind.ipc },
      debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: debugOptions,
      }
   };

   const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: "file", language: "obs" }],
      synchronize: {
         fileEvents: [vscode.workspace.createFileSystemWatcher("**/*.obs")]
      },
      uriConverters: {
         code2Protocol: (uri: vscode.Uri) => uri.fsPath,
         protocol2Code: vscode.Uri.parse
      }
   };

   languageClient = new LanguageClient(
      "obs-lsp", "Obsidian Language Server", 
      serverOptions, clientOptions);

   languageClient.start();
}

   interface ObsidianCompilationTaskDefinition extends vscode.TaskDefinition {
      /**
      * The file to compile
      */
      file: string;
   }

   interface ObsidianDownTaskDefinition extends vscode.TaskDefinition {
   }


   let _channel: vscode.OutputChannel;
   function getOutputChannel(): vscode.OutputChannel {
      if (!_channel) {
         _channel = vscode.window.createOutputChannel('Rake Auto Detection');
      }
      return _channel;
   }

   function buildPathForFile(filename: string) {
      return path.resolve(path.dirname(filename), "build");
   }

   function logPathForFile(filename: string) {
       return path.resolve(path.dirname(filename), "logs");
   }

// Assumes logPath is an existing directory.
    function copyFileToLogDir(sourcePath: string, logPath: string, timestamp: Date) {
        const logInputPath = path.resolve(logPath, timestamp.toISOString() + '-' + path.basename(sourcePath));
        fs.copyFile(sourcePath, logInputPath, (err) => {
            if (err) { console.log('failed to copy file: ' + err); throw err; }
        });
    }

   function getCompilationTask(): vscode.Task | undefined {
      const chaincodePath = chaincodeContractPath()
      if (chaincodePath == undefined) {
         return undefined;
      }

      let kind: ObsidianCompilationTaskDefinition = {
            type: 'obsidian-compile',
            file: chaincodePath
      }
   
      const buildPath = buildPathForFile(chaincodePath)

      // Copy the source code for future analysis.
      const logPath = logPathForFile(buildPath);
      const timestamp = new Date();
      
      // This needs to be synchronous so that the directory will be there when the task below runs.
      if (!fs.existsSync(logPath)) {
            fs.mkdirSync(logPath);
      }
        
      copyFileToLogDir(chaincodePath, logPath, timestamp);
    
      const logOutputPath = path.resolve(logPath, timestamp.toISOString() + '-' + path.basename(chaincodePath) + '.out');


      fs.mkdir(buildPath, (err) => {if (err) throw err;} )
      let execution = new vscode.ShellExecution(`obsidianc --output-path ${buildPath} ${chaincodePath} | tee '${logOutputPath}'`);
      let task = new vscode.Task(kind, chaincodePath, 'obsidian-compile', execution, ["$obsidian"]);
      task.presentationOptions.clear = true
      task.presentationOptions.showReuseMessage = false
      task.presentationOptions.echo = false
      task.problemMatchers = ["ObsidianProblemMatcher"]
      task.group = vscode.TaskGroup.Build;
      return (task);
   }

    function getClientCompilationTask(): vscode.Task | undefined {
      let clientPath = clientContractPath()
      if (clientPath == undefined) {
         return undefined
      }

      let buildPath = buildPathForFile(clientPath)
      
      fs.mkdir(buildPath, (err) => {if (err) throw err;} )

      let execution = new vscode.ShellExecution(`obsidianc --output-path ${buildPath} --build-client ${clientPath}`);
      let kind: ObsidianCompilationTaskDefinition = {
         type: 'obsidian-compile',
         file: clientPath
      }
      let task = new vscode.Task(kind, clientPath, 'obsidian-compile-client', execution, ["$obsidian"]);
      task.presentationOptions.clear = true
      task.presentationOptions.showReuseMessage = false
      task.problemMatchers = ["ObsidianProblemMatcher"]
      task.group = vscode.TaskGroup.Build;
      return (task);
    }

    function getDeployObsidianTask(contractName: string): vscode.Task | undefined {
      let chaincodePath = chaincodeContractPath()
      if (chaincodePath == undefined) {
         return undefined
      }

      let kind: ObsidianCompilationTaskDefinition = {
         type: 'obsidian-deploy',
         file: chaincodePath
      }

      let buildPath = buildPathForFile(chaincodePath);
      let contractPath = path.resolve(buildPath, contractName)
      let execution = new vscode.ShellExecution(`obsidianautoup -s ${contractPath}`);

      let task = new vscode.Task(kind, chaincodePath, 'obsidian-deploy', execution, ["$obsidian"]);
      task.presentationOptions.clear = false
      task.presentationOptions.showReuseMessage = false
      task.problemMatchers = ["ObsidianProblemMatcher"]
      return (task);
   }

   function getNetworkDownTask(): vscode.Task {
      let kind: ObsidianDownTaskDefinition = {
         type: 'obsidian-down',
      }
      let execution = new vscode.ShellExecution(`obsidiandown`);

      let task = new vscode.Task(kind, "down", 'obsidian-down', execution, []);
      task.presentationOptions.clear = false
      task.presentationOptions.showReuseMessage = false
      return (task);
   }

   function getRunObsidianClientTask(clientName: string): vscode.Task | undefined {
      let clientPath = clientContractPath()
      if (clientPath == undefined) {
         return undefined
      }

      let kind: ObsidianCompilationTaskDefinition = {
            type: 'obsidian-deploy',
            file: clientPath
      }

      let buildPath = buildPathForFile(clientPath);
      let jarPath = path.resolve(buildPath, clientName, "build", "libs", "chaincode.jar")
      
      let execution = new vscode.ShellExecution(`java -jar ${jarPath}`);

      let task = new vscode.Task(kind, clientPath, 'obsidian-deploy', execution, []);
      task.presentationOptions.clear = false
      task.presentationOptions.showReuseMessage = false
      return (task);
   }
  
   function projectPath(): string | undefined {
      let workspaceFolders = vscode.workspace.workspaceFolders
      if (workspaceFolders != undefined && workspaceFolders.length > 0) {
         let workspacePath = workspaceFolders[0]
         return workspacePath.uri.fsPath;
      }
      return undefined;
   }

   function projectConfig(): ProjectConfig | undefined {
      let workspacePath = projectPath();
      if (workspacePath != undefined) {
         try {
            let configFile = fs.readFileSync(path.resolve(workspacePath, "project.json"))
            let projectConfig = JSON.parse(configFile.toString('utf8'))
            return projectConfig
         }
         catch (e) {
            console.log("Unable to find  project.json file at path " + path.resolve(workspacePath, "project.json"))
            return undefined
         }
      }
      else {
         console.log("undefined root path")
      }

      return undefined
   }

   // If there's a config file, use that. Otherwise, assume the current text editor has a client in it.
   function clientContractPath(): string | undefined {
      let activeEditor = vscode.window.activeTextEditor

      if (activeEditor != undefined) {
         let config = projectConfig()
         if (config != undefined) {
            let basePath = path.dirname(activeEditor.document.fileName)
            return path.resolve(basePath, config.client)
         }

         return activeEditor.document.fileName
      }
      else {
         return undefined
      }         
   }

   // If there's a config file, use that. Otherwise, assume the current text editor has chaincode in it.
   function chaincodeContractPath(): string | undefined {
      let activeEditor = vscode.window.activeTextEditor
      if (activeEditor != undefined) {
         let config = projectConfig()
         if (config != undefined) {
            let basePath = path.dirname(activeEditor.document.fileName)
            return path.resolve(basePath, config.chaincode)
         }

         return activeEditor.document.fileName
      }
      else {
         return undefined
      }         
   }

   function mainContractName(contractPath: string): string | undefined {
      try {
         let chaincodeFile = fs.readFileSync(contractPath)
         let contractText = chaincodeFile.toString('utf8')
         let mainDeclIndex = contractText.match(/main.*contract\s+(\S+)/);
         if (mainDeclIndex != null && mainDeclIndex.length > 1) {                     
            return mainDeclIndex[1]
         }
         else {
            return undefined
         }
      }
      catch (e) {
         return undefined
      }
   }

   function chaincodeContractName(): string | undefined {
      let chaincodePath = chaincodeContractPath()
      if (chaincodePath != undefined) {
        return mainContractName(chaincodePath)
      }
      else {
         return undefined
      }
   }

   function clientContractName(): string | undefined {
      let contractPath = clientContractPath()
      if (contractPath != undefined) {
        return mainContractName(contractPath)
      }
      else {
         return undefined
      }
   }

// this method is called when your extension is deactivated
export function deactivate() {
   return languageClient?.stop();
}