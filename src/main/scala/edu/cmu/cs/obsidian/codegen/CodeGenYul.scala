package edu.cmu.cs.obsidian.codegen

import java.io.{File, PrintWriter, FileReader}
import java.nio.file.{Path, Paths}

import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.Main.{findMainContract, findMainContractName}
import edu.cmu.cs.obsidian.parser.SymbolTable
import com.github.mustachejava.DefaultMustacheFactory
import com.github.mustachejava.Mustache
import com.github.mustachejava.MustacheFactory
import java.io.IOException

object CodeGenYul extends CodeGenerator{
    def gen(filename: String, srcDir: Path, outputPath: Path, protoDir: Path,
            options: CompilerOptions, checkedTable: SymbolTable, transformedTable: SymbolTable): Boolean = {

        val mainName = findMainContractName(checkedTable.ast)
        val finalOutputPath = options.outputPath match {
            case Some(p) =>
                Paths.get(p).resolve(mainName)
            case None =>
                Paths.get(mainName)
        }

        val ast = checkedTable.ast
        val mainContractOption = findMainContract(ast)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val mainContract = mainContractOption.get.name
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"), "example")
        mustache.execute(new PrintWriter(new File(finalOutputPath.toString(),mainContract + ".yul" )), new Scope(mainContract)).flush

        return true
    }
}

class Scope(val mainContractName: String) {
    val creationObject:String = mainContractName
    val runtimeObject:String = mainContractName + "_deployed"
}