import { createConnection, ProposedFeatures, TextDocuments, InitializeParams, TextDocumentSyncKind, TextDocumentChangeEvent, Position, Diagnostic, Range, DiagnosticSeverity } from "vscode-languageserver";
import { TextDocument } from "vscode-languageserver-textdocument";

import * as process from "child_process";
import { promises as fs } from "fs";
import * as path from "path";

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

connection.onInitialize((params: InitializeParams) => {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
      // completionProvider: {
      //   triggerCharacters: ["."],
      //   resolveProvider: false
      // }
    }
  }
});

async function validateTextDocument(change: TextDocumentChangeEvent<TextDocument>) {
  connection.console.log("Validating document");

  // Create temporary file containing the current set of changes 
  // TODO: Modify obsidianc to accept source code via stdin instead 
  // Note: obsidianc crashes if the source file begins with a dot
  const tmpName = __dirname + "/ObsidianLSPTmp.obs"
  const tmpFile = await fs.open(tmpName, "w+");
  await tmpFile.write(change.document.getText(), 0);
  tmpFile.close();

  const typecheckingError = /^.*\s(?<line>\d+)\.(?<col>\d+): (?<msg>.*)$/;
  const parsingError = /Parser failure at (?<line>\d+).(?<col>\d+): (?<msg>.*)$/;
  const syntaxError = /(?<msg>.*) at (?<line>\d+):(?<col>\d+)/;

  let stdout: string = "";

  try {
    stdout = process.execSync(`obsidianc "${tmpName}" -L "${path.dirname(change.document.uri)}"`, { 
      encoding: "utf8"
    });
  }
  catch (e) {
    // node throws if a child process exits with nonzero status
    stdout = e.stdout?.toString() || "";
  }
  // Harvest messages from obsidianc
  // Type assertion is required since TypeScript
  // can't figure out that the array elements can never
  // be null 
  const diagnostics: Diagnostic[] = <Diagnostic[]>
    stdout.split("\n")
    .map(line => {
        const parseResult: RegExpExecArray | undefined = <RegExpExecArray | undefined>
          [typecheckingError, parsingError, syntaxError]
          .map(regex => regex.exec(line))
          .find(x => x !== null);
        if (parseResult === undefined) return null;

        // The compiler reports positions where lines/cols
        // start as 1, but vscode needs it starting from 0
        const errorLine = Number(parseResult.groups!.line) - 1;
        const errorColumn = Number(parseResult.groups!.col) - 1;
        const errorMessage = parseResult.groups!.msg;

        const errorPosition = Position.create(errorLine, errorColumn);
        return Diagnostic.create(
          Range.create(errorPosition, errorPosition), 
          errorMessage,
          DiagnosticSeverity.Error);
    })
    .filter((x: Diagnostic | null) => x !== null);

  connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
}

documents.onDidOpen(validateTextDocument);
documents.onDidChangeContent(validateTextDocument);

documents.listen(connection);
connection.listen();