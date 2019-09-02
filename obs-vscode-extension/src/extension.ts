// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as fs from 'fs'
import * as path from 'path'

let taskProvider: vscode.Disposable | undefined;

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
                     let editor = vscode.window.activeTextEditor
                     if (editor != undefined) {
                        let contractText = editor.document.getText(undefined) // Get the whole file
                        let mainDeclIndex = contractText.match(/main.*contract\s+(\S+)/);
                        if (mainDeclIndex != null && mainDeclIndex.length > 1) {                     
                           let deployTask = getDeployObsidianTask(mainDeclIndex[1]);
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
                  // Then figure out what the name of the main contract was.
                  let editor = vscode.window.activeTextEditor
                  if (editor != undefined) {
                     let contractText = editor.document.getText(undefined) // Get the whole file
                     let mainDeclIndex = contractText.match(/main.*contract\s+(\S+)/);
                     if (mainDeclIndex != null && mainDeclIndex.length > 1) {                     
                        let runTask = getRunObsidianClientTask(mainDeclIndex[1]);
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
               }
               else {
                  vscode.window.showErrorMessage('Compilation failed.');
               }
            }
         })

         vscode.tasks.executeTask(compilationTask as vscode.Task)
      }
   });

   context.subscriptions.push(runClientCommand);



}

    interface ObsidianCompilationTaskDefinition extends vscode.TaskDefinition {
        /**
         * The file to compile
         */
        file: string;
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

    function getCompilationTask(): vscode.Task | undefined {
        let activeEditor = vscode.window.activeTextEditor
        if (activeEditor != undefined) {
            let filename = activeEditor.document.fileName
            let kind: ObsidianCompilationTaskDefinition = {
                type: 'obsidian-compile',
                file: filename
            }
           
            let buildPath = buildPathForFile(filename)
            console.log("build path " + buildPath)
            fs.mkdir(buildPath, (err) => {if (err) throw err;} )
            let execution = new vscode.ShellExecution(`obsidianc --output-path ${buildPath} ${filename}`);
            let task = new vscode.Task(kind, filename, 'obsidian-compile', execution, ["$obsidian"]);
            task.presentationOptions.clear = true
            task.presentationOptions.showReuseMessage = false
            task.problemMatchers = ["ObsidianProblemMatcher"]
            task.group = vscode.TaskGroup.Build;
            return (task);
        }
        else {
            return undefined;
        }
    }

    function getClientCompilationTask(): vscode.Task | undefined {
        let activeEditor = vscode.window.activeTextEditor
        if (activeEditor != undefined) {
            let filename = activeEditor.document.fileName
            let kind: ObsidianCompilationTaskDefinition = {
                type: 'obsidian-compile-client',
                file: filename
            }
            let execution = new vscode.ShellExecution(`obsidianc --build-client ${filename}`);

            let task = new vscode.Task(kind, filename, 'obsidian-compile-client', execution, ["$obsidian"]);
            task.presentationOptions.clear = true
            task.presentationOptions.showReuseMessage = false
            task.problemMatchers = ["ObsidianProblemMatcher"]
            task.group = vscode.TaskGroup.Build;
            return (task);
        }
        else {
            return undefined;
        }
    }

    function getDeployObsidianTask(contractName: string): vscode.Task | undefined {
      let activeEditor = vscode.window.activeTextEditor
      if (activeEditor != undefined) {
          let filename = activeEditor.document.fileName
          let kind: ObsidianCompilationTaskDefinition = {
              type: 'obsidian-deploy',
              file: filename
          }

          let buildPath = buildPathForFile(filename);
          let contractPath = path.resolve(buildPath, contractName)
          let execution = new vscode.ShellExecution(`obsidianautoup -s ${contractPath}`);

          let task = new vscode.Task(kind, filename, 'obsidian-deploy', execution, ["$obsidian"]);
          task.presentationOptions.clear = false
          task.presentationOptions.showReuseMessage = false
          task.problemMatchers = ["ObsidianProblemMatcher"]
          return (task);
      }
      else {
          return undefined;
      }
   }

   function getNetworkDownTask(): vscode.Task | undefined {
      let activeEditor = vscode.window.activeTextEditor
      if (activeEditor != undefined) {
            let filename = activeEditor.document.fileName
            let kind: ObsidianCompilationTaskDefinition = {
               type: 'obsidian-deploy',
               file: filename
            }
            let execution = new vscode.ShellExecution(`obsidiandown`);

            let task = new vscode.Task(kind, filename, 'obsidian-deploy', execution, []);
            task.presentationOptions.clear = false
            task.presentationOptions.showReuseMessage = false
            return (task);
      }
      else {
            return undefined;
      }
   }

   function getRunObsidianClientTask(jarPath: String): vscode.Task | undefined {
      let activeEditor = vscode.window.activeTextEditor
      if (activeEditor != undefined) {
            let filename = activeEditor.document.fileName
            let kind: ObsidianCompilationTaskDefinition = {
               type: 'obsidian-deploy',
               file: filename
            }
            let execution = new vscode.ShellExecution(`java -jar ${jarPath}`);

            let task = new vscode.Task(kind, filename, 'obsidian-deploy', execution, []);
            task.presentationOptions.clear = false
            task.presentationOptions.showReuseMessage = false
            return (task);
      }
      else {
            return undefined;
      }
   }
  

// this method is called when your extension is deactivated
export function deactivate() {}