package CodeGen

import Parser._
import com.sun.codemodel.internal._
import com.sun.tools.javac.code.Flags
import com.sun.tools.javac.tree.JCTree._
import com.sun.tools.javac.tree._
import com.sun.tools.javac.util._

/**
  * Created by mcoblenz on 2/13/17.
  */
class CodeGen {
  /*
  def translateProgram (p : Program) : Seq[JCCompilationUnit] =
    p.contracts.map(translateContract);


  // Contracts translate to compilation units containing one class.
  def translateContract (c : Contract) : JCCompilationUnit = {
    val context : Context = new Context()
    val treeMaker : TreeMaker = TreeMaker.instance(context)
    val names : Names = new Names(context)

    val classModifiers = treeMaker.Modifiers(Flags.PUBLIC)

    val javaClassDecl = treeMaker.ClassDef(classModifiers,
      names.
    )
    val defs = List(classDecl)


    val compilationUnit = new JCCompilationUnit(List.empty, // no package annotations for now
      null,
      defs,
      null,
      null,
      null,
      null)

    compilationUnit
  }
*/

  def translateProgram (program : Program) : JCodeModel = {
    val model : JCodeModel = new JCodeModel()
    val programPackage : JPackage = model._package("edu.cmu.cs.obsidian.generated-code") // Put all generated code in the same package.


    for (aContract : Contract <- program.contracts) {
      translateContract(aContract, programPackage)

    }
  }


  // Contracts translate to compilation units containing one class.
  def translateContract (aContract : Contract, programPackage : JPackage) = {
    val newClass : JDefinedClass = programPackage._class(aContract.name)
    for (decl <- aContract.declarations) {
      translateDeclaration(decl, newClass);
    }
  }

  def translateDeclaration(declaration: Declaration, newClass : JDefinedClass): Unit = {
    declaration match {
      case TypeDecl (name, typ) => () // TODO
      case Field (typ, fieldName) =>
      case Func (name, args, body) => // TODO
      case Transaction (name, args, body) =>
      case State(name, declarations) =>
    }
  }


/*
  def translateStatement(s : Statement) =


    def translateExpression(e : Expression) =
*/
}
