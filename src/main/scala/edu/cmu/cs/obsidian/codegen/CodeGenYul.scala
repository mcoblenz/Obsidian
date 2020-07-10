package edu.cmu.cs.obsidian.codegen

import java.io.{File, FileReader, PrintWriter}
import java.nio.file.{Path, Paths}

import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.parser.{JavaFFIContractImpl, ObsidianContractImpl, Program, SymbolTable}
import edu.cmu.cs.obsidian.Main.{findMainContract, findMainContractName}
import com.github.mustachejava.Mustache
import com.github.mustachejava.MustacheFactory

object CodeGenYul extends CodeGenerator{
    def gen(filename: String, srcDir: Path, outputPath: Path, protoDir: Path,
            options: CompilerOptions, checkedTable: SymbolTable, transformedTable: SymbolTable): Boolean = {

        val ast = checkedTable.ast
        val mainContractOption = findMainContract(ast)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }

        val mainName = findMainContractName(ast)

        val finalOutputPath = options.outputPath match {
            case Some(p) =>
                Paths.get(p).resolve(mainName)
            case None =>
                Paths.get(mainName)
        }

        // temporary code, not designed to be recursive
        val mainContract = mainContractOption.get
        mainContract match {
            case obsContract: ObsidianContractImpl => {
                val translated_obj =  translateContract(obsContract)
                genYulCode(translated_obj, finalOutputPath)
            }
            case _ => None
        }
//        val ast_seq =  translateProgram(ast)
        return true
    }

    def translateProgram(program: Program): Unit ={
        // ignore imports for now
        for (c <- program.contracts) {
            c match {
                case obsContract: ObsidianContractImpl => translateContract(obsContract)
                case javaContract: JavaFFIContractImpl => None
            }
        }
    }

    def translateContract(contract: ObsidianContractImpl): Object ={
        // create deploy object
        val runtime_name = contract.name + "_deployed"
        val runtime = Object(runtime_name, Code(Block(Seq())), Seq(), Seq())
        val create = Object(contract.name, Code(Block(Seq())), Seq(runtime), Seq())
        create
    }

    def genYulCode(obj: Object, finalOutputPath: Path)= {
        printf("outputpath: %s, name: %s", finalOutputPath, obj.name)
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/Object.mustache"),"example")
        val scope = new Scope(obj.name)
        mustache.execute(new PrintWriter(new File(finalOutputPath.toString(), obj.name + ".yul")), scope).flush
    }
}

// temporary function, not designed for a full recursive walk through of the object
class Scope(val mainContractName: String) {
    val creationObject:String = mainContractName
    val runtimeObject:String = mainContractName + "_deployed"
}