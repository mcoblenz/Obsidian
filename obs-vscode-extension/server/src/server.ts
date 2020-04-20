import { createConnection, ProposedFeatures, TextDocuments, InitializeParams, TextDocumentSyncKind, TextDocumentChangeEvent, Position, Diagnostic, Range, DiagnosticSeverity } from "vscode-languageserver";
import { TextDocument } from "vscode-languageserver-textdocument";

import * as process from "child_process";
import { promises as fs } from "fs";

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

interface CompilerError {
  line: number;
  column: number;
  message: string;
}

async function validateTextDocument(change: TextDocumentChangeEvent<TextDocument>) {
  connection.console.log("Validating document");

  // Create temporary file containing the current set of changes 
  // TODO: Modify obsidianc to accept source code via stdin instead 
  // Note: obsidianc crashes if the source file begins with a dot
  const tmpName = __dirname + "/ObsidianLSPTmp.obs"
  const tmpFile = await fs.open(tmpName, "w+");
  await tmpFile.write(change.document.getText(), 0);
  tmpFile.close();

  const errorRegex = /^.*\s(\d+)\.(\d+): (.*)$/;

  let stdout: string = "";

  try {
    stdout = process.execSync(`obsidianc "${tmpName}"`, { encoding: "utf8"});
  }
  catch (e) {
    stdout = e.stdout.toString();
  }
  // Harvest messages from obsidianc
  // Type assertion is required since TypeScript
  // can't figure out that the array elements can never
  // be null 
  const diagnostics: Diagnostic[] = <Diagnostic[]>
    stdout.split("\n")
    .map((line: string) => {
        const parseResult = errorRegex.exec(line);
        if (parseResult === null) return null;

        const errorLine = Number(parseResult[1]) - 1;
        const errorColumn = Number(parseResult[2]) - 1;
        const errorMessage = parseResult[3];

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