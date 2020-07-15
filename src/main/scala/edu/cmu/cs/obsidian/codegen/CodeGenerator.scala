package edu.cmu.cs.obsidian.codegen

import java.nio.file.Path

import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.parser.SymbolTable

trait CodeGenerator {
    def gen(filename: String, srcDir: Path, outputPath: Path, protoDir: Path,
            options: CompilerOptions, checkedTable: SymbolTable, transformedTable: SymbolTable): Boolean
}