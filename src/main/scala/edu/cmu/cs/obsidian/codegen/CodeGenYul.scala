package edu.cmu.cs.obsidian.codegen

import java.io.{File, FileReader, PrintWriter}
import java.nio.file.{Files, Path, Paths}

import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.Main.{findMainContract, findMainContractName}
import com.github.mustachejava.Mustache
import com.github.mustachejava.MustacheFactory
import edu.cmu.cs.obsidian.codegen.Code

import scala.collection.immutable.Map

// need some table remembering field index in storage
object CodeGenYul extends CodeGenerator {

    // TODO improve this temporary symbol table
    var temp_symbol_table: Map[String, Int] = Map() // map from field identifiers to index in storage
    var temp_table_idx = 0


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
        // translate from obsidian AST to yul AST
        val translated_obj = translateProgram(ast)
        // generate yul string from yul AST and write to output file
        yulString(translated_obj, finalOutputPath)
        true
    }


    def translateProgram(program: Program): YulObject = {
        // TODO temporary code to generate an empty template for main contract
        // translate main contract
        val mainContractOption = findMainContract(program)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val mainContract = mainContractOption.get

        val main_contract_ast = mainContract match {
            case obsContract: ObsidianContractImpl => translateContract(obsContract)
            case _ => throw new RuntimeException("Java contract not supported in yul translation")
        }

        // TODO ignore imports, data for now
        // translate other contracts (if any) and add them to the subObjects
        var new_subObjects: Seq[YulObject] = main_contract_ast.subObjects
        for (c <- program.contracts) {
            c match {
                case obsContract: ObsidianContractImpl =>
                    if (!c.modifiers.contains(IsMain())) { // if not main contract
                        new_subObjects = main_contract_ast.subObjects :+ translateContract(obsContract)
                    }
                case _: JavaFFIContractImpl =>
                    throw new RuntimeException("Java contract not supported in yul translation")
            }
        }
        YulObject(main_contract_ast.name, main_contract_ast.code,
            new_subObjects, main_contract_ast.data)
    }

    def translateContract(contract: ObsidianContractImpl): YulObject = {
        // create runtime obj
        var subObjects: Seq[YulObject] = Seq()
        val runtime_name = contract.name + "_deployed"
        val runtime_obj = YulObject(runtime_name, Code(Block(Seq())), Seq(), Seq())
        subObjects = runtime_obj +: subObjects

        var statement_seq: Seq[YulStatement] = Seq()
        // TODO initialization

        val create = YulObject(contract.name, Code(Block(statement_seq)), subObjects, Seq())
        return create
    }


    def translateDeclaration(declaration: Declaration): Seq[YulStatement] = {
        declaration match {
            case f: Field => translateField(f)
            case t: Transaction =>
                assert(false, "TODO")
                Seq()
            case s: State =>
                assert(false, "TODO")
                Seq()
            case c: ObsidianContractImpl =>
                assert(false, "TODO")
                Seq()
            case c: JavaFFIContractImpl =>
                assert(false, "TODO")
                Seq()
            case c: Constructor =>
                assert(false, "TODO")
                Seq()
            case t: TypeDecl =>
                assert(false, "TODO")
                Seq()
            // This should never be hit.
            case _ =>
                assert(false, "Translating unexpected declaration: " + declaration)
                Seq()
        }
    }

    def translateField(field: Field): Seq[YulStatement] = {
        // Reserve a slot in the storage by assigning a index in the symbol table
        // since field declaration has not yet be assigned, there is no need to do sstore
        temp_symbol_table += field.name -> temp_table_idx
        temp_table_idx += 1
        Seq()
    }

    def yulString(obj: YulObject, finalOutputPath: Path)= {
        printf("outputpath: %s, name: %s", finalOutputPath, obj.name)
        Files.createDirectories(finalOutputPath)
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"),"example")
        val scope = new Scope(obj.name)
        mustache.execute(new PrintWriter(new File(finalOutputPath.toString(), obj.name + ".yul")), scope).flush
    }
}

// temporary function, not designed for a full recursive walk through of the object
class Scope(val mainContractName: String) {
    val creationObject:String = mainContractName
    val runtimeObject:String = mainContractName + "_deployed"
}