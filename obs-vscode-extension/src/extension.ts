// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

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
                        val => {vscode.window.showInformationMessage('Compilation succeeded.');},
                        err => {vscode.window.showInformationMessage('Compilation failed.');}
                    )
                }
            }       
    );

    context.subscriptions.push(compileCommand);

    let compileClientCommand = vscode.commands.registerCommand('extension.compileObsidianClient', () => {
        // The code you place here will be executed every time your command is executed
            let task = getClientCompilationTask();
            if (task != undefined) {
                vscode.tasks.executeTask(task as vscode.Task).then( 
                    val => {vscode.window.showInformationMessage('Compilation succeeded.');},
                    err => {vscode.window.showInformationMessage('Compilation failed.');}
                )
            }
        }
    );       
    context.subscriptions.push(compileClientCommand);
        

    context.subscriptions.push(compileCommand);

        let commandPromise: Thenable<vscode.Task[]> | undefined = undefined;
        console.log("in activate");

        taskProvider = vscode.tasks.registerTaskProvider('obsidian-compile', {
            provideTasks: () => {
                console.log("in provideTasks()");
                if (!commandPromise) {
                     commandPromise = getCompilationTasks();
                 }
                return commandPromise;
            },
            resolveTask(_task: vscode.Task): vscode.Task | undefined {
                console.log("in resolveTask");
                return undefined;
            }
        });


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

    function getCompilationTask(): vscode.Task | undefined {
        let activeEditor = vscode.window.activeTextEditor
        if (activeEditor != undefined) {
            let filename = activeEditor.document.fileName
            let kind: ObsidianCompilationTaskDefinition = {
                type: 'obsidian-compile',
                file: filename
            }
            let execution = new vscode.ShellExecution(`obsidianc ${filename}`);
            return (new vscode.Task(kind, filename, 'obsidian-compile', execution, ["$obsidian"]));
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
            return (new vscode.Task(kind, filename, 'obsidian-compile-client', execution, ["$obsidian"]));
        }
        else {
            return undefined;
        }
    }

    async function getCompilationTasks(): Promise<vscode.Task[]> {
        console.log("in getCompilationTasks");
        let result: vscode.Task[] = [];

        let task = getCompilationTask();
        if (task != undefined) {
            result.push(task as vscode.Task);
        }

        return result;
    }

// this method is called when your extension is deactivated
export function deactivate() {}