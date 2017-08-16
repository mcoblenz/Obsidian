package edu.cmu.cs.obsidian.tests


import org.junit.Assert.{assertTrue, fail}
import org.junit.Test
import org.scalatest.junit.JUnitSuite

import edu.cmu.cs.obsidian.typecheck._
import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ArrayBuffer

class TypeCheckerTests extends JUnitSuite {
    type LineNumber = Int

    private def runTest(file: String, expectedErrors: Seq[(Error, LineNumber)]): Unit = {
        var prog: Program = null
        try {
            prog = Parser.parseFileAtPath(file, printTokens = false)
        }
        catch {
            case p: Parser.ParseException =>
                val errMsg = p.message
                fail(s"Failed with parser message $errMsg")
        }

        val table = new SymbolTable(prog)
        val checker = new Checker(table)
        val errs = checker.checkProgram()
        var remaining = new ArrayBuffer[(Error, LineNumber)]() ++ expectedErrors
        for (err <- errs) {
            val pred = (expected: (Error, LineNumber)) => {
                expected._1 == err && expected._2 == err.loc.line
            }
            val line = err.loc.line
            assertTrue(s"Nothing matches $err at line $line", remaining.exists(pred))
            val indexToRemove = remaining.indexOf((err, err.loc.line))
            remaining.remove(indexToRemove)
        }
        val msg = s"The following errors weren't found when checking: $remaining"
        assertTrue(msg, remaining.isEmpty)
    }

    @Test def basicTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ExampleTypeFailure.obs",
            (SubTypingError(BoolType(), IntType()), 18)
              ::
              (WrongArityError(1, 0, "createC"), 19)
              ::
              (SubTypingError(BoolType(), IntType()), 20)
              ::
              Nil
        )
    }

    @Test def operationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimpleOperations.obs",
            (SubTypingError(BoolType(), IntType()), 5)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 6)
              ::
              (SubTypingError(
                  BoolType(),
                  IntType()), 7)
              ::
              (SubTypingError(
                  StringType(),
                  BoolType()), 8)
              ::
              (SubTypingError(
                  IntType(),
                  BoolType()), 9)
              ::
              (SubTypingError(
                  IntType(),
                  BoolType()), 9)
              ::
              Nil
        )
    }

    @Test def comparisonTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimpleComparisons.obs",
            (SubTypingError(BoolType(), IntType()), 5)
              ::
              (SubTypingError(
                  BoolType(),
                  IntType()), 6)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 7)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 8)
              ::
              (SubTypingError(
                  BoolType(),
                  IntType()), 9)
              ::
              (SubTypingError(
                  BoolType(),
                  IntType()), 9)
              :: Nil
        )
    }

    @Test def variableTest(): Unit = {
        runTest("resources/tests/type_checker_tests/UndefinedVariable.obs",
            (VariableUndefinedError("x"), 4)
              ::
              (VariableUndefinedError("z"), 5)
              ::
              Nil
        )
    }

    @Test def fieldsTest(): Unit = {
        runTest("resources/tests/type_checker_tests/CheckFields.obs",
            (StateSpecificSharedError(), 16)
              ::
              (StateSpecificReadOnlyError(), 17)
              ::
              (StateSpecificReadOnlyError(), 19)
              ::
              Nil
        )
    }

    @Test def assignmentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Assignment.obs",
            (SubTypingError(BoolType(), IntType()), 22)
              ::
              (SubTypingError(
                  OwnedRef(null, NoPathType(JustContractType("C_Unique"))),
                  SharedRef(null, NoPathType(JustContractType("C_Shared")))),
                24)
              ::
              (FieldUndefinedError(JustContractType("C_Shared"), "f2"), 26)
              ::
              (FieldUndefinedError(JustContractType("C_Shared"), "f3"), 27)
              ::
              (SubTypingError(
                  SharedRef(null, NoPathType(JustContractType("C_Shared"))),
                  SharedRef(null, NoPathType(StateType("C_Shared", "S")))),
                28)
              ::
              (VariableUndefinedError("j"), 32)
              ::
              (AssignmentError(), 33)
              ::
              Nil
        )
    }

    @Test def returnTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Return.obs",
            (CannotReturnError("t_no_ret"), 10)
              ::
              (UnreachableCodeError(), 11)
              ::
              (MustReturnError("t_has_ret"), 16)
              ::
              (UnreachableCodeError(), 16)
              ::
              (SubTypingError(
                  OwnedRef(null, NoPathType(JustContractType("C_Unique"))),
                  OwnedRef(null, NoPathType(StateType("C_Unique", "S")))),
                21)
              ::
              (UnreachableCodeError(), 21)
              ::
              (MustReturnError("t_ret_nonprimitive"), 22)
              ::
              (SubTypingError(IntType(),
                  OwnedRef(null, NoPathType(JustContractType("C_Unique")))), 23)
              ::
              (MustReturnError("no_return"), 28)
              ::
              (UnreachableCodeError(), 37)
              ::
              (MustReturnError("branching_return2"), 46)
              ::
              Nil
        )
    }

    @Test def equalityTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Equality.obs",
            (DifferentTypeError(Variable("a"), IntType(), Variable("b"), StringType()), 9)
              ::
              (DifferentTypeError(
                  TrueLiteral(),
                  BoolType(),
                  NumLiteral(5),
                  IntType()), 10)
              ::
              (DifferentTypeError(
                  NumLiteral(1),
                  IntType(),
                  FalseLiteral(),
                  BoolType()), 11)
              ::
              Nil
        )
    }

    @Test def invocationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Invocation.obs",
            (WrongArityError(1, 0, "a"), 16)
              ::
              (WrongArityError(1, 2, "a"), 17)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 18)
              ::
              (MethodUndefinedError(
                  JustContractType("Invocation"),
                  "otherMethod"), 19)
              ::
              (NonInvokeableError(IntType()), 20)
              ::
              (MethodUndefinedError(
                  JustContractType("OtherContract"),
                  "anotherMethod"), 25)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 26)
              ::
              Nil
        )
    }

    @Test def dereferenceTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Dereference.obs",
            (FieldUndefinedError(JustContractType("Thing"), "w"), 20)
              ::
              (DereferenceError(StringType()), 22)
              ::
              Nil
        )
    }

    @Test def constructionTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Construction.obs",
            (ConstructorNameError("Thing"), 24)
              ::
              (WrongArityError(0, 1, "constructor of Thing"), 33)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 33)
              ::
              (SubTypingError(
                StringType(),
                IntType()), 33)
              ::
              (WrongArityError(3, 1, "constructor of Thing"), 33)
              ::
              (WrongArityError(0, 3, "constructor of Thing"), 34)
              ::
              (WrongArityError(1, 3, "constructor of Thing"), 34)
              ::
              (WrongArityError(1, 3, "constructor of Thing"), 34)
              ::
              (SubTypingError(
                  IntType(),
                  BoolType()), 34)
              ::
              (SubTypingError(
                  IntType(),
                  StringType()), 34)
              ::
              (ContractUndefinedError("Stuff"), 35)
              ::
              (ContractUndefinedError("Stuff"), 35)
              ::
              Nil
        )
    }

    @Test def branchingTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Branching.obs",
            (MergeIncompatibleError("o1",
                OwnedRef(null, NoPathType(JustContractType("Ow"))),
                ReadOnlyRef(null, NoPathType(JustContractType("Ow")))), 12)
              ::
              (UnusedOwnershipError("o2"), 12)
              ::
              (UnusedOwnershipError("o2"), 22)
              ::
              (MergeIncompatibleError("o1",
                OwnedRef(null, NoPathType(JustContractType("Ow"))),
                ReadOnlyRef(null, NoPathType(JustContractType("Ow")))), 31)
              ::
              (UnusedOwnershipError("o2"), 31)
              ::
              (VariableUndefinedError("x"), 42)
              ::
              Nil)
    }

    @Test def sideEffectTest(): Unit = {
      runTest("resources/tests/type_checker_tests/NoSideEffects.obs",
          (NoEffectsError(Variable("x")), 5)
            ::
            (NoEffectsError(
              Add(NumLiteral(1),NumLiteral(3))), 6)
            ::
            (NoEffectsError(
              LessThan(NumLiteral(1),Variable("x"))), 7)
            ::
            (NoEffectsError(
              Disjunction(TrueLiteral(),FalseLiteral())), 8)
            ::
            Nil
      )
    }

    @Test def stateTest(): Unit = {
      runTest("resources/tests/type_checker_tests/States.obs",
          (TransitionUpdateError(Set("x")), 11)
          ::
          (StateUndefinedError("C", "S3"), 12)
          ::
          (FieldUndefinedError(
              StateType("C","S2"), "x"), 14)
          ::
          (TransitionError(), 20)
          ::
          Nil
      )
    }

    @Test def bottomTest(): Unit = {
        runTest("resources/tests/type_checker_tests/BottomTypeNoError.obs",
          (ContractUndefinedError("D"), 3)
          ::
          Nil
        )
    }

    @Test def contractUndefinedTest(): Unit = {
        runTest("resources/tests/type_checker_tests/UndefinedContract.obs",
          (ContractUndefinedError("OtherThing"), 2)
          ::
          (ContractUndefinedError("OtherThing"), 5)
          ::
          Nil
        )
    }

    @Test def simplePathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimplePDT.obs",
                (UnusedOwnershipError("b"), 7)::Nil)
    }

    @Test def implicitPathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ImplicitPDT.obs",
            (UnusedOwnershipError("b"), 7)::Nil)
    }

    @Test def noStartStateTest(): Unit = {
        runTest("resources/tests/type_checker_tests/StartState.obs",
            (NoStartStateError("HasStates"), 12)
                ::
                (NoConstructorError("StatesNoConstr"), 31)
                ::
                Nil
        )
    }

    @Test def parentPathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ParentPDT.obs",
            (NoParentError("UsesC"), 19)::Nil)
    }

    @Test def thisAndParentPathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ThisTypePDT.obs", Nil)
    }

}

