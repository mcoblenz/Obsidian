package edu.cmu.cs.obsidian.codegen

import java.io.{File, FileReader, PrintWriter}
import java.nio.file.{Path, Paths}

import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.codegen.YulAST

import edu.cmu.cs.obsidian.Main.{findMainContract, findMainContractName}
import com.github.mustachejava.Mustache
import com.github.mustachejava.MustacheFactory

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

        // temporary code, not designed to be recursive
        val mainContract = mainContractOption.get
        mainContract match {
            case obsContract: ObsidianContractImpl => {
                val translated_obj =  translateContract(obsContract)
                yulString(translated_obj, finalOutputPath)
            }
            case _ => None
        }
//        val ast_seq =  translateProgram(ast)
        return true
    }

    def translateProgram(program: Program): Unit = {
        // ignore imports for now
        for (c <- program.contracts) {
            c match {
                case obsContract: ObsidianContractImpl => translateContract(obsContract)
                case javaContract: JavaFFIContractImpl => None
            }
        }
    }

    def translateContract(contract: ObsidianContractImpl): Object = {
        // create runtime obj
        var subobjects: Seq[Object] = Seq()
        val runtime_name = contract.name + "_deployed"
        val runtime_obj = Object(runtime_name, Code(Block(Seq())), Seq(), Seq())
        subobjects = runtime_obj +: subobjects

        var statement_seq: Seq[YulStatement] = Seq()
        // initialization
//        statement_seq = statement_seq +: translate_mem_init() // TODO check seq append

        // declaration
        // what to do when instance of constructor
        for (decl <- contract.declarations if !decl.isInstanceOf[Constructor]) {
//            statement_seq = statement_seq +: translateDeclaration(decl)
        }

        val create = Object(contract.name, Code(Block(statement_seq)), subobjects, Seq())
        return create
    }

//    def translate_mem_init(): Seq[Statement]

    def translateDeclaration(declaration: Declaration): Seq[YulStatement] = {
        declaration match {
            case f: Field => translateField(f)
            case t: Transaction =>Seq()
            case s: State =>Seq()
            case c: ObsidianContractImpl =>Seq()
            case c: JavaFFIContractImpl => Seq()
            case c: Constructor =>Seq()
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
        // no need to sstore
        // TODO create symbol table
        temp_symbol_table += field.name -> temp_table_idx
        temp_table_idx += 1
        Seq()
    }

    def yulString(obj: Object, finalOutputPath: Path)= {
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