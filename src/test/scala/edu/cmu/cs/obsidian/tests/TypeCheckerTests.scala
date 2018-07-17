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
            (SubtypingError(BoolType(), IntType()), 38)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("C_Owned"), Owned(), false),
                    ContractReferenceType(ContractType("C_Shared"), Shared(), false)),
                    41)
                ::
                (FieldUndefinedError(ContractReferenceType(ContractType("C_Shared"), Shared(), false), "f2"), 20)
                ::
                (FieldUndefinedError(ContractReferenceType(ContractType("C_Shared"), Shared(), false), "f3"), 22)
                ::
                (VariableUndefinedError("j", null), 49)
                ::
                (AssignmentError(), 51)
                ::
                (InvalidNonThisFieldAssignment(), 54)
                ::
                (DereferenceError(IntType()), 54)
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
                    ContractReferenceType(ContractType("C_Owned"), Owned(), false),
                    StateType("C_Owned", "S", false)),
                    28)
                ::
                (UnreachableCodeError(), 28)
                ::
                (MustReturnError("t_ret_nonprimitive"), 31)
                ::
                (SubtypingError(IntType(),
                    ContractReferenceType(ContractType("C_Owned"), Owned(), false)), 33)
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
                    ContractReferenceType(ContractType("Invocation"), Shared(), false),
                    "otherMethod"), 23)
                ::
                (NonInvokeableError(IntType()), 25)
                ::
                (MethodUndefinedError(
                    ContractReferenceType(ContractType("OtherContract"), Shared(), false),
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
            (FieldUndefinedError(ContractReferenceType(ContractType("Thing"), Shared(), false), "w"), 23)
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
                (RepeatConstructorsError("Thing"), 27)
                ::
                (SubtypingError(ContractReferenceType(ContractType("Thing"), Owned(), false),
                    ContractReferenceType(ContractType("OtherThing"), Inferred(), false)), 27)
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
            (MergeIncompatibleError("o1",
                ContractReferenceType(ContractType("Ow"), Owned(), false),
                ContractReferenceType(ContractType("Ow"), Unowned(), false)), 16)
                ::
                (UnusedOwnershipError("o2"), 16)
                ::
                (UnusedOwnershipError("o2"), 27)
                ::
                (MergeIncompatibleError("o1",
                    ContractReferenceType(ContractType("Ow"), Owned(), false),
                    ContractReferenceType(ContractType("Ow"), Unowned(), false)), 36)
                ::
                (UnusedOwnershipError("o2"), 36)
                ::
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
                    StateType("C", "S1", false), "x"), 18)
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
                StateType("C", "S1", false),
                StateType("C", "S2", false)), 3
            )
                ::
                (SubtypingError(
                    StateType("C", "S2", false),
                    StateType("C", "S1", false)), 8
                )
                ::
                Nil
        )
    }

    @Test def endsInStateUnionTest(): Unit = {
        runTest("resources/tests/type_checker_tests/EndsInStateUnion.obs",
            (StateUndefinedError("C1", "OtherState"), 12)
                ::
                (SubtypingError(
                    StateType("C1", Set("S1", "S2"), false),
                    StateType("C1", Set("S1", "S3"), false)), 17
                )
                ::
                (SubtypingError(
                    StateType("C2", Set("S1", "S2"), false),
                    StateType("C2", "S1", false)), 30
                )
                ::
                (
                    VariableUndefinedError("f2", null), 62
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
                    ContractReferenceType(ContractType("Money"), Unowned(), false),
                    ContractReferenceType(ContractType("Money"), Owned(), false)), 28)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Money"), Unowned(), false),
                    ContractReferenceType(ContractType("Money"), Owned(), false)), 37)
                ::
                (NonResourceOwningResourceError("BadWallet",
                    Field(false,
                        ContractReferenceType(ContractType("Money"), Owned(), false),
                        "money",
                        None)), 43)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Money"), Unowned(), false),
                    ContractReferenceType(ContractType("Money"), Owned(), false)),
                    56)
                ::
                (DisownUnowningExpressionError(ReferenceIdentifier("m")), 49)
                ::
                (UnusedOwnershipError("bad"), 52)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Money"), Unowned(), false),
                    ContractReferenceType(ContractType("Money"), Owned(), false)), 59)
                ::
                (UnusedOwnershipError("m"), 75)
                ::
                Nil
        )
    }

    @Test def ownershipTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Ownership.obs",
            //                (TODO: https://github.com/mcoblenz/Obsidian/issues/134)
            //                (InvalidOwnershipTransfer(ReferenceIdentifier("p"), ContractReferenceType(ContractType("Prescription"), Unowned())), 16)
            //                ::
            (SubtypingError(
                ContractReferenceType(ContractType("Prescription"), Unowned(), false),
                ContractReferenceType(ContractType("Prescription"), Owned(), false)), 16)
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
            (ArgShadowingError("x", "t", 6), 10)
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

    @Test def staticAssertsTest(): Unit = {
        runTest("resources/tests/type_checker_tests/StaticAsserts.obs",
            (StaticAssertInvalidState("C", "S3"), 6)
                ::
                (StaticAssertFailed(This(), Seq("S2"), StateType("C", Set("S1", "S2"), false)), 17)
                ::
                (StaticAssertFailed(ReferenceIdentifier("ow"), Seq("Unowned"), ContractReferenceType(ContractType("C"), Owned(), false)), 24)
                ::
                Nil
        )
    }

    @Test def typeSpecificationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/TypeSpecification.obs",
            (SubtypingError(StateType("C", Set("S3", "S2"), false),
                StateType("C", "S1", false)), 24)
                ::
                (ArgumentSpecificationError("a", "badChangeA",
                    StateType("A", "Unavailable", false),
                    StateType("A", "Available", false)), 51)
                ::
                (ArgumentSpecificationError("a", "badChangeA2",
                    StateType("A", "Available", false),
                    StateType("A", "Unavailable", false)), 56)
                ::
                Nil
        )
    }

    @Test def readOnlyStateTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ReadOnlyState.obs",
            (TransitionNotAllowedError(), 11) ::
                (ReceiverTypeIncompatibleError("changeStateShared",
                    ContractReferenceType(ContractType("C"), ReadOnlyState(), false),
                    ContractReferenceType(ContractType("C"), Shared(), false)), 39) ::
                (ReceiverTypeIncompatibleError("changeStateOwned",
                    ContractReferenceType(ContractType("C"), Shared(), false),
                    ContractReferenceType(ContractType("C"), Owned(), false)), 42) ::
                (ReceiverTypeIncompatibleError("changeStateStateSpecified",
                    ContractReferenceType(ContractType("C"), Owned(), false),
                    StateType("C", Set("S1"), false)), 45) ::
                Nil
        )
    }

    @Test def remoteInstanceInvocationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/RemoteInstanceInvocation.obs",
            (UsedInRemoteInvoke("c"), 22)
                ::
                Nil
        )
    }
}
