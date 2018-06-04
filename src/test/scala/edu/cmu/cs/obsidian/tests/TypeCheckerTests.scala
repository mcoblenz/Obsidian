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
        val (globalTable: SymbolTable, transformErrors) = AstTransformer.transformProgram(table)

        val checker = new Checker(globalTable)
        val errs = (checker.checkProgram() ++ transformErrors).sorted

        val remaining = new ArrayBuffer[(Error, LineNumber)]() ++ expectedErrors
        for (ErrorRecord(err, loc) <- errs) {
            val pred = (expected: (Error, LineNumber)) => {
                expected._1 == err && expected._2 == loc.line
            }
            val line = loc.line
            val indexOfError = remaining.indexWhere(pred)
            if (indexOfError < 0) {
                assertTrue(s"Unexpected error: $err at line $line", false)
            }
            else {
                remaining.remove(indexOfError)
            }
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
            (VariableUndefinedError("x", null), 7)
                ::
                (VariableUndefinedError("z", null), 9)
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
                    NonPrimitiveType(null, NoPathType(JustContractType("C_Owned")), Set()),
                    NonPrimitiveType(null, NoPathType(JustContractType("C_Shared")), Set())),
                    28)
                ::
                (FieldUndefinedError(JustContractType("C_Shared"), "f2"), 31)
                ::
                (FieldUndefinedError(JustContractType("C_Shared"), "f3"), 33)
                ::
                (SubTypingError(
                    NonPrimitiveType(null, NoPathType(JustContractType("C_Shared")), Set()),
                    NonPrimitiveType(null, NoPathType(StateType("C_Shared", "S")), Set())),
                    36)
                ::
                (VariableUndefinedError("j", null), 41)
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
                    NonPrimitiveType(null, NoPathType(JustContractType("C_Owned")), Set(IsOwned())),
                    NonPrimitiveType(null, NoPathType(StateType("C_Owned", "S")), Set(IsOwned()))),
                    28)
                ::
                (UnreachableCodeError(), 28)
                ::
                (MustReturnError("t_ret_nonprimitive"), 31)
                ::
                (SubTypingError(IntType(),
                    NonPrimitiveType(null, NoPathType(JustContractType("C_Owned")), Set(IsOwned()))), 33)
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
            (DifferentTypeError(ReferenceIdentifier("a"), IntType(), ReferenceIdentifier("b"), StringType()), 12)
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
                (ContractUndefinedError("Bogus"), 36)
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
            // TODO: https://github.com/mcoblenz/Obsidian/issues/56
            //            (MergeIncompatibleError("o1",
            //                NonPrimitiveType(null, NoPathType(JustContractType("Ow")) ,Set(IsOwned())),
            //                NonPrimitiveType(null, NoPathType(JustContractType("Ow")), Set(IsReadOnlyState()))), 16)
            //              ::
            //              (UnusedOwnershipError("o2"), 16)
            //              ::
            //              (UnusedOwnershipError("o2"), 27)
            //              ::
            // TODO: https://github.com/mcoblenz/Obsidian/issues/56
            //              (MergeIncompatibleError("o1",
            //                  NonPrimitiveType(null, NoPathType(JustContractType("Ow")), Set(IsOwned())),
            //                  NonPrimitiveType(null, NoPathType(JustContractType("Ow")), Set(IsReadOnlyState()))), 36)
            //              ::
            //              (UnusedOwnershipError("o2"), 36)
            //              ::
            (VariableUndefinedError("x", null), 48)
                ::
                Nil)
    }

    @Test def sideEffectTest(): Unit = {
        runTest("resources/tests/type_checker_tests/NoSideEffects.obs",
            (NoEffectsError(ReferenceIdentifier("x")), 8)
                ::
                (NoEffectsError(
                    Add(NumLiteral(1), NumLiteral(3))), 10)
                ::
                (NoEffectsError(
                    LessThan(NumLiteral(1), ReferenceIdentifier("x"))), 12)
                ::
                (NoEffectsError(
                    Disjunction(TrueLiteral(), FalseLiteral())), 14)
                ::
                Nil
        )
    }

    @Test def stateTest(): Unit = {
        runTest("resources/tests/type_checker_tests/States.obs",
            (TransitionUpdateError(Set("x")), 13)
                ::
                (StateUndefinedError("C", "S3"), 15)
                ::
                (FieldUndefinedError(
                    StateType("C", "S1"), "x"), 18)
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

    /*
    @Test def simplePathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimplePDT.obs",
                (UnusedOwnershipError("b"), 9)::Nil)
    }

    @Test def implicitPathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ImplicitPDT.obs",
            (UnusedOwnershipError("b"), 11)::Nil)
    }
    */

    @Test def noStartStateTest(): Unit = {
        runTest("resources/tests/type_checker_tests/StartState.obs",
            (NoStartStateError("HasStates"), 12)
                ::
                (NoConstructorError("StatesNoConstr"), 31)
                ::
                Nil
        )
    }

    /*
    @Test def parentPathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ParentPDT.obs",
            (NoParentError("UsesC"), 22)::Nil)
    }

    @Test def thisAndParentPathDependentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ThisTypePDT.obs", Nil)
    }
    */

    @Test def endsInStateTest(): Unit = {
        runTest("resources/tests/type_checker_tests/EndsInState.obs",
            (SubTypingError(
                NonPrimitiveType(null, NoPathType(StateType("C", "S1")), Set(IsOwned())),
                NonPrimitiveType(null, NoPathType(StateType("C", "S2")), Set(IsOwned()))), 3
            )
                ::
                (SubTypingError(
                    NonPrimitiveType(null, NoPathType(StateType("C", "S2")), Set(IsOwned())),
                    NonPrimitiveType(null, NoPathType(StateType("C", "S1")), Set(IsOwned()))), 8
                )
                ::
                Nil
        )
    }

    @Test def endsInStateUnionTest(): Unit = {
        runTest("resources/tests/type_checker_tests/EndsInStateUnion.obs",
            (SubTypingError(
                NonPrimitiveType(null, NoPathType(StateUnionType("C1", Set("S2", "S3"))), Set(IsOwned())),
                NonPrimitiveType(null, NoPathType(StateUnionType("C1", Set("S1", "S2"))), Set(IsOwned()))), 4
            )
                ::
                (StateUndefinedError("C1", "OtherState"), 13)
                ::
                (SubTypingError(
                    NonPrimitiveType(null, NoPathType(StateUnionType("C1", Set("S1", "S2"))), Set(IsOwned())),
                    NonPrimitiveType(null, NoPathType(StateUnionType("C1", Set("S1", "S3"))), Set(IsOwned()))), 19
                )
                ::
                (SubTypingError(
                    NonPrimitiveType(null, NoPathType(StateUnionType("C2", Set("S1", "S2"))), Set(IsOwned())),
                    NonPrimitiveType(null, NoPathType(StateType("C2", "S1")), Set(IsOwned()))), 33
                )
                ::
                (
                    VariableUndefinedError("f2", null), 65
                )
                ::
                Nil
        )
    }

    @Test def multistateFieldsTest(): Unit = {
        runTest("resources/tests/type_checker_tests/MultistateFields.obs",
            (VariableUndefinedError("foo", null), 19)
                ::
                (VariableUndefinedError("foo", null), 24)
                ::
                (VariableUndefinedError("foo", null), 27)
                ::
                Nil
        )
    }

    @Test def resourcesTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Resources.obs",
            (ResourceContractConstructorError("BogusMoney"), 5)
                ::
                (UnusedOwnershipError("m"), 22)
                ::
                (OwnershipSubtypingError(
                    NonPrimitiveType(null, NoPathType(JustContractType("Money")), Set()),
                    NonPrimitiveType(null, NoPathType(JustContractType("Money")), Set(IsOwned()))), 28)
                ::
                (OwnershipSubtypingError(
                    NonPrimitiveType(null, NoPathType(JustContractType("Money")), Set()),
                    NonPrimitiveType(null, NoPathType(JustContractType("Money")), Set(IsOwned()))), 37)
                ::
                (NonResourceOwningResourceError("BadWallet",
                    Field(false,
                        NonPrimitiveType(null,
                            NoPathType(JustContractType("Money")),
                            Set(IsOwned())),
                        "money",
                        None)), 43)
                ::
                (SubTypingError(
                    NonPrimitiveType(null, NoPathType(JustContractType("Money")), Set()),
                    NonPrimitiveType(null, NoPathType(JustContractType("Money")), Set(IsOwned()))),
                    56)
                ::
                (DisownUnowningExpressionError(ReferenceIdentifier("m")), 49)
                ::
                (UnusedOwnershipError("bad"), 52)
                ::
                (OwnershipSubtypingError(
                    NonPrimitiveType(null, NoPathType(JustContractType("Money")), Set()),
                    NonPrimitiveType(null, NoPathType(JustContractType("Money")), Set(IsOwned()))), 59)
                ::
                (UnusedOwnershipError("m"), 75)
                ::
                Nil
        )
    }

    @Test def ownershipTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Ownership.obs",
            (InvalidOwnershipTransfer(ReferenceIdentifier("p"), NonPrimitiveType(null, NoPathType(JustContractType("Prescription")), Set())), 17)
                ::
                (OwnershipSubtypingError(
                    NonPrimitiveType(null, NoPathType(JustContractType("Prescription")), Set()),
                    NonPrimitiveType(null, NoPathType(JustContractType("Prescription")), Set(IsOwned()))), 17)
                ::
                Nil
        )
    }

    @Test def stateInitializationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/StateInitialization.obs",
            (InvalidStateFieldInitialization("S2", "x2"), 22)
                ::
                (TransitionUpdateError(Set("x1", "x2", "shared")), 26)
                ::
                (InvalidStateFieldInitialization("S1", "x1"), 29)
                ::
                (SharedFieldNameError("x2", "S1", 4), 8)
                ::
                Nil
        )
    }

    @Test def droppedResourcesTest(): Unit = {
        runTest("resources/tests/type_checker_tests/MaybeDroppedResource.obs",
            (PotentiallyUnusedOwnershipError("m"), 17)
                ::
                (NoEffectsError(OwnershipTransfer(ReferenceIdentifier("n"))), 21)
                ::
                (UnusedExpressionOwnershipError(OwnershipTransfer(ReferenceIdentifier("n"))), 21)
                ::
                Nil
        )
    }

    @Test def transitionTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Transitions.obs",
                Nil
        )
    }


    @Test def noMainContractTest(): Unit = {
        runTest("resources/tests/type_checker_tests/NoMainContract.obs",
            (NoMainContractError(), 1)
                ::
                Nil
        )
    }


    @Test def shadowingTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ForbiddenShadowing.obs",
            (ShadowingError("x", "S1", 2), 9)
                ::
                Nil
        )
    }


    @Test def sameFieldNameTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SameNameFields.obs",
            (ShadowingError("x", "S1", 24), 8)
                ::
                (SharedFieldNameError("x", "S1", 8), 15)
                ::
                (ShadowingError("x", "S3", 24), 15)
                ::
                (CombineAvailableIns("shared", "S1, S2, S3", 18), 19)
                ::
                (RepeatContractFields("test", 22, 21), 22)
                ::
                Nil
        )
    }

    @Test def argShadowingTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ArgumentShadowing.obs",
            (ArgShadowingError("x", "t", 4), 8)
              ::
              Nil
        )
    }


}
