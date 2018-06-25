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

        val importsProcessedAst = ImportProcessor.processImports(file, prog)
        val fieldsLiftedAst = StateFieldTransformer.transformProgram(importsProcessedAst)

        val table = new SymbolTable(fieldsLiftedAst)
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
            (SubtypingError(BoolType(), IntType()), 19)
                ::
                (WrongArityError(1, 0, "createC"), 21)
                ::
                (SubtypingError(BoolType(), IntType()), 23)
                ::
                Nil
        )
    }

    @Test def operationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimpleOperations.obs",
            (SubtypingError(BoolType(), IntType()), 8)
                ::
                (SubtypingError(
                    StringType(),
                    IntType()), 10)
                ::
                (SubtypingError(
                    BoolType(),
                    IntType()), 12)
                ::
                (SubtypingError(
                    StringType(),
                    BoolType()), 14)
                ::
                (SubtypingError(
                    IntType(),
                    BoolType()), 16)
                ::
                (SubtypingError(
                    IntType(),
                    BoolType()), 16)
                ::
                Nil
        )
    }

    @Test def comparisonTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimpleComparisons.obs",
            (SubtypingError(BoolType(), IntType()), 8)
                ::
                (SubtypingError(
                    BoolType(),
                    IntType()), 10)
                ::
                (SubtypingError(
                    StringType(),
                    IntType()), 12)
                ::
                (SubtypingError(
                    StringType(),
                    IntType()), 14)
                ::
                (SubtypingError(
                    BoolType(),
                    IntType()), 16)
                ::
                (SubtypingError(
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
                Nil
        )
    }

    @Test def assignmentTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Assignment.obs",
            (SubtypingError(BoolType(), IntType()), 26)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("C_Owned"), Owned()),
                    ContractReferenceType(ContractType("C_Shared"), Shared())),
                    29)
                ::
                (FieldUndefinedError(ContractReferenceType(ContractType("C_Shared"), Shared()), "f2"), 32)
                ::
                (FieldUndefinedError(ContractReferenceType(ContractType("C_Shared"), Shared()), "f3"), 34)
                ::
                (VariableUndefinedError("j", null), 41)
                ::
                (AssignmentError(), 43)
                ::
                (DereferenceError(IntType()), 46)
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
                (SubtypingError(
                    ContractReferenceType(ContractType("C_Owned"), Owned()),
                    StateType("C_Owned", "S")),
                    28)
                ::
                (UnreachableCodeError(), 28)
                ::
                (MustReturnError("t_ret_nonprimitive"), 31)
                ::
                (SubtypingError(IntType(),
                    ContractReferenceType(ContractType("C_Owned"), Owned())), 33)
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
                (SubtypingError(
                    StringType(),
                    IntType()), 21)
                ::
                (MethodUndefinedError(
                    ContractReferenceType(ContractType("Invocation"), Shared()),
                    "otherMethod"), 23)
                ::
                (NonInvokeableError(IntType()), 25)
                ::
                (MethodUndefinedError(
                    ContractReferenceType(ContractType("OtherContract"), Shared()),
                    "anotherMethod"), 31)
                ::
                (SubtypingError(
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
            (FieldUndefinedError(ContractReferenceType(ContractType("Thing"), Shared()), "w"), 23)
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
                (SubtypingError(
                    StringType(),
                    IntType()), 37)
                ::
                (SubtypingError(
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
                (SubtypingError(
                    IntType(),
                    BoolType()), 39)
                ::
                (SubtypingError(
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
            //                NonPrimitiveType(null, ContractReferenceType("Ow") ,Set(IsOwned())),
            //                NonPrimitiveType(null, ContractReferenceType("Ow"), Set(IsReadOnlyState()))), 16)
            //              ::
            //              (UnusedOwnershipError("o2"), 16)
            //              ::
            //              (UnusedOwnershipError("o2"), 27)
            //              ::
            // TODO: https://github.com/mcoblenz/Obsidian/issues/56
            //              (MergeIncompatibleError("o1",
            //                  NonPrimitiveType(null, ContractReferenceType("Ow"), Set(IsOwned())),
            //                  NonPrimitiveType(null, ContractReferenceType("Ow"), Set(IsReadOnlyState()))), 36)
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
            (SubtypingError(
                StateType("C", "S1"),
                StateType("C", "S2")), 3
            )
                ::
                (SubtypingError(
                    StateType("C", "S2"),
                    StateType("C", "S1")), 8
                )
                ::
                Nil
        )
    }

    @Test def endsInStateUnionTest(): Unit = {
        runTest("resources/tests/type_checker_tests/EndsInStateUnion.obs",
            (SubtypingError(
                StateType("C1", Set("S2", "S3")),
                StateType("C1", Set("S1", "S2"))), 4
            )
                ::
                (StateUndefinedError("C1", "OtherState"), 13)
                ::
                (SubtypingError(
                    StateType("C1", Set("S1", "S2")),
                    StateType("C1", Set("S1", "S3"))), 19
                )
                ::
                (SubtypingError(
                    StateType("C2", Set("S1", "S2")),
                    StateType("C2", "S1")), 33
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
                (SubtypingError(
                    ContractReferenceType(ContractType("Money"), Unowned()),
                    ContractReferenceType(ContractType("Money"), Owned())), 28)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Money"), Unowned()),
                    ContractReferenceType(ContractType("Money"), Owned())), 37)
                ::
                (NonResourceOwningResourceError("BadWallet",
                    Field(false,
                        ContractReferenceType(ContractType("Money"), Owned()),
                        "money",
                        None)), 43)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Money"), Unowned()),
                    ContractReferenceType(ContractType("Money"), Owned())),
                    56)
                ::
                (DisownUnowningExpressionError(ReferenceIdentifier("m")), 49)
                ::
                (UnusedOwnershipError("bad"), 52)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Money"), Unowned()),
                    ContractReferenceType(ContractType("Money"), Owned())), 59)
                ::
                (UnusedOwnershipError("m"), 75)
                ::
                Nil
        )
    }

    @Test def ownershipTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Ownership.obs",
            (InvalidOwnershipTransfer(ReferenceIdentifier("p"), ContractReferenceType(ContractType("Prescription"), Unowned())), 17)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Prescription"), Unowned()),
                    ContractReferenceType(ContractType("Prescription"), Owned())), 17)
                ::
                Nil
        )
    }

    @Test def stateInitializationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/StateInitialization.obs",
            (InvalidStateFieldInitialization("S2", "x3"), 21)
                ::
                (TransitionUpdateError(Set("x1", "x2", "shared")), 25)
                ::
                (InvalidStateFieldInitialization("S1", "x1"), 28)
                ::
                Nil
        )
    }

    @Test def droppedResourcesTest(): Unit = {
        runTest("resources/tests/type_checker_tests/MaybeDroppedResource.obs",
            (PotentiallyUnusedOwnershipError("m"), 17)
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
                (RepeatContractFields("x", 9, 2), 9)
                    ::
                    Nil
        )
    }


    @Test def sameFieldNameTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SameNameFields.obs",
            (
                (CombineAvailableIns("x", "S1, S3", 8), 15)
                    ::
                    (CombineAvailableIns("shared", "S1, S2, S3", 18), 19)
                    ::
                    (RepeatContractFields("test", 22, 21), 22)
                    ::
                    (RepeatContractFields("x", 15, 15), 24)
                    ::
                    (RepeatContractFields("x", 8, 8), 24)
                    ::
                    Nil
                )
        )
    }

    @Test def argShadowingTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ArgumentShadowing.obs",
            (ArgShadowingError("x", "t", 4), 8)
              ::
              Nil
        )
    }

    @Test def multipleConstructorsTest(): Unit = {
        runTest("resources/tests/type_checker_tests/MultipleConstructors.obs",
            (MultipleConstructorsError("C"), 3)
              ::
              Nil
        )
    }


}
