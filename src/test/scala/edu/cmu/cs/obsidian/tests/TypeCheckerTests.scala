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
        val remaining = new ArrayBuffer[(Error, LineNumber)]() ++ expectedErrors
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
            (SubTypingError(BoolType(), IntType()), 19)
              ::
              (WrongArityError(1, 0, "createC"), 21)
              ::
              (SubTypingError(BoolType(), IntType()), 23)
              ::
              Nil
        )
    }

    @Test def operationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimpleOperations.obs",
            (SubTypingError(BoolType(), IntType()), 8)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 10)
              ::
              (SubTypingError(
                  BoolType(),
                  IntType()), 12)
              ::
              (SubTypingError(
                  StringType(),
                  BoolType()), 14)
              ::
              (SubTypingError(
                  IntType(),
                  BoolType()), 16)
              ::
              (SubTypingError(
                  IntType(),
                  BoolType()), 16)
              ::
              Nil
        )
    }

    @Test def comparisonTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimpleComparisons.obs",
            (SubTypingError(BoolType(), IntType()), 8)
              ::
              (SubTypingError(
                  BoolType(),
                  IntType()), 10)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 12)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 14)
              ::
              (SubTypingError(
                  BoolType(),
                  IntType()), 16)
              ::
              (SubTypingError(
                  BoolType(),
                  IntType()), 16)
              :: Nil
        )
    }

    @Test def variableTest(): Unit = {
        runTest("resources/tests/type_checker_tests/UndefinedVariable.obs",
            (VariableUndefinedError("x"), 7)
              ::
              (VariableUndefinedError("z"), 9)
              ::
              Nil
        )
    }

    @Test def fieldsTest(): Unit = {
        runTest("resources/tests/type_checker_tests/CheckFields.obs",
            (StateSpecificSharedError(), 19)
              ::
              (StateSpecificReadOnlyError(), 20)
              ::
              (StateSpecificReadOnlyError(), 21)
              ::
              Nil
        )
    }

    @Test def assignmentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Assignment.obs",
            (SubTypingError(BoolType(), IntType()), 25)
              ::
              (SubTypingError(
                  OwnedRef(null, NoPathType(JustContractType("C_Owned"))),
                  SharedRef(null, NoPathType(JustContractType("C_Shared")))),
                28)
              ::
              (FieldUndefinedError(JustContractType("C_Shared"), "f2"), 31)
              ::
              (FieldUndefinedError(JustContractType("C_Shared"), "f3"), 33)
              ::
              (SubTypingError(
                  SharedRef(null, NoPathType(JustContractType("C_Shared"))),
                  SharedRef(null, NoPathType(StateType("C_Shared", "S")))),
                36)
              ::
              (VariableUndefinedError("j"), 41)
              ::
              (AssignmentError(), 43)
              ::
              Nil
        )
    }

    @Test def returnTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Return.obs",
            (CannotReturnError("t_no_ret"), 14)
              ::
              (UnreachableCodeError(), 16)
              ::
              (MustReturnError("t_has_ret"), 22)
              ::
              (UnreachableCodeError(), 22)
              ::
              (SubTypingError(
                  OwnedRef(null, NoPathType(JustContractType("C_Owned"))),
                  OwnedRef(null, NoPathType(StateType("C_Owned", "S")))),
                28)
              ::
              (UnreachableCodeError(), 28)
              ::
              (MustReturnError("t_ret_nonprimitive"), 30)
              ::
              (SubTypingError(IntType(),
                  OwnedRef(null, NoPathType(JustContractType("C_Owned")))), 32)
              ::
              (MustReturnError("no_return"), 38)
              ::
              (UnreachableCodeError(), 48)
              ::
              (MustReturnError("branching_return2"), 58)
              ::
              Nil
        )
    }

    @Test def equalityTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Equality.obs",
            (DifferentTypeError(Variable("a"), IntType(), Variable("b"), StringType()), 12)
              ::
              (DifferentTypeError(
                  TrueLiteral(),
                  BoolType(),
                  NumLiteral(5),
                  IntType()), 14)
              ::
              (DifferentTypeError(
                  NumLiteral(1),
                  IntType(),
                  FalseLiteral(),
                  BoolType()), 16)
              ::
              Nil
        )
    }

    @Test def invocationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Invocation.obs",
            (WrongArityError(1, 0, "a"), 17)
              ::
              (WrongArityError(1, 2, "a"), 19)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 21)
              ::
              (MethodUndefinedError(
                  JustContractType("Invocation"),
                  "otherMethod"), 23)
              ::
              (NonInvokeableError(IntType()), 25)
              ::
              (MethodUndefinedError(
                  JustContractType("OtherContract"),
                  "anotherMethod"), 31)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 33)
              ::
              Nil
        )
    }

    @Test def dereferenceTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Dereference.obs",
            (FieldUndefinedError(JustContractType("Thing"), "w"), 23)
              ::
              (DereferenceError(StringType()), 25)
              ::
              Nil
        )
    }

    @Test def constructionTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Construction.obs",
            (ConstructorNameError("Thing"), 27)
              ::
              (WrongArityError(0, 1, "constructor of Thing"), 37)
              ::
              (SubTypingError(
                  StringType(),
                  IntType()), 37)
              ::
              (SubTypingError(
                StringType(),
                IntType()), 37)
              ::
              (WrongArityError(3, 1, "constructor of Thing"), 37)
              ::
              (WrongArityError(0, 3, "constructor of Thing"), 39)
              ::
              (WrongArityError(1, 3, "constructor of Thing"), 39)
              ::
              (WrongArityError(1, 3, "constructor of Thing"), 39)
              ::
              (SubTypingError(
                  IntType(),
                  BoolType()), 39)
              ::
              (SubTypingError(
                  IntType(),
                  StringType()), 39)
              ::
              (ContractUndefinedError("Stuff"), 41)
              ::
              (ContractUndefinedError("Stuff"), 41)
              ::
              Nil
        )
    }

    @Test def branchingTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Branching.obs",
            (MergeIncompatibleError("o1",
                OwnedRef(null, NoPathType(JustContractType("Ow"))),
                ReadOnlyRef(null, NoPathType(JustContractType("Ow")))), 16)
              ::
              (UnusedOwnershipError("o2"), 16)
              ::
              (UnusedOwnershipError("o2"), 27)
              ::
              (MergeIncompatibleError("o1",
                OwnedRef(null, NoPathType(JustContractType("Ow"))),
                ReadOnlyRef(null, NoPathType(JustContractType("Ow")))), 36)
              ::
              (UnusedOwnershipError("o2"), 36)
              ::
              (VariableUndefinedError("x"), 48)
              ::
              Nil)
    }

    @Test def sideEffectTest(): Unit = {
      runTest("resources/tests/type_checker_tests/NoSideEffects.obs",
          (NoEffectsError(Variable("x")), 8)
            ::
            (NoEffectsError(
              Add(NumLiteral(1),NumLiteral(3))), 10)
            ::
            (NoEffectsError(
              LessThan(NumLiteral(1),Variable("x"))), 12)
            ::
            (NoEffectsError(
              Disjunction(TrueLiteral(),FalseLiteral())), 14)
            ::
            Nil
      )
    }

    @Test def stateTest(): Unit = {
      runTest("resources/tests/type_checker_tests/States.obs",
          (TransitionUpdateError(Set("x")), 12)
          ::
          (StateUndefinedError("C", "S3"), 14)
          ::
          (FieldUndefinedError(
              StateType("C","S1"), "x"), 17)
          ::
          (TransitionError(), 24)
          ::
          Nil
      )
    }

    @Test def bottomTest(): Unit = {
        runTest("resources/tests/type_checker_tests/BottomTypeNoError.obs",
          (ContractUndefinedError("D"), 9)
          ::
          Nil
        )
    }

    @Test def contractUndefinedTest(): Unit = {
        runTest("resources/tests/type_checker_tests/UndefinedContract.obs",
          (ContractUndefinedError("OtherThing"), 3)
          ::
          (ContractUndefinedError("OtherThing"), 7)
          ::
          Nil
        )
    }

    @Test def simplePathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimplePDT.obs",
                (UnusedOwnershipError("b"), 8)::Nil)
    }

    @Test def implicitPathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ImplicitPDT.obs",
            (UnusedOwnershipError("b"), 10)::Nil)
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
            (NoParentError("UsesC"), 22)::Nil)
    }

    @Test def thisAndParentPathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ThisTypePDT.obs", Nil)
    }

}

