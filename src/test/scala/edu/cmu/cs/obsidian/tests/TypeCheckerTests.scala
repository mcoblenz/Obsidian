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

        val (importsProcessedAst, importErrors) = ImportProcessor.processImports(file, prog)
        val fieldsLiftedAst = StateFieldTransformer.transformProgram(importsProcessedAst)

        val table = new SymbolTable(fieldsLiftedAst)
        val (globalTable: SymbolTable, transformErrors) = StateNameValidator.transformProgram(table)

        val checker = new Checker(globalTable)
        val errs = (checker.checkProgram()._1 ++ transformErrors).sorted

        val remaining = new ArrayBuffer[(Error, LineNumber)]() ++ expectedErrors
        for (ErrorRecord(err, loc, _) <- errs) {
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
            (SubtypingError(BoolType(), IntType(), false), 19)
                ::
                (WrongArityError(1, 0, "createC"), 21)
                ::
                (SubtypingError(BoolType(), IntType(), false), 23)
                ::
                Nil
        )
    }

    @Test def operationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimpleOperations.obs",
            (SubtypingError(BoolType(), IntType(), false), 8)
                ::
                (SubtypingError(
                    StringType(),
                    IntType(), false), 10)
                ::
                (SubtypingError(
                    BoolType(),
                    IntType(), false), 12)
                ::
                (SubtypingError(
                    StringType(),
                    BoolType(), false), 14)
                ::
                (SubtypingError(
                    IntType(),
                    BoolType(), false), 16)
                ::
                (SubtypingError(
                    IntType(),
                    BoolType(), false), 16)
                ::
                Nil
        )
    }

    @Test def comparisonTest(): Unit = {
        runTest("resources/tests/type_checker_tests/SimpleComparisons.obs",
            (SubtypingError(BoolType(), IntType(), false), 8)
                ::
                (SubtypingError(
                    BoolType(),
                    IntType(), false), 10)
                ::
                (SubtypingError(
                    StringType(),
                    IntType(), false), 12)
                ::
                (SubtypingError(
                    StringType(),
                    IntType(), false), 14)
                ::
                (SubtypingError(
                    BoolType(),
                    IntType(), false), 16)
                ::
                (SubtypingError(
                    BoolType(),
                    IntType(), false), 16)
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
            (SubtypingError(BoolType(), IntType(), false), 38)
                ::
                (InconsistentContractTypeError("C_Shared", "C_Owned"), 41)
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
                (UnreachableCodeError(), 28)
                ::
                (MustReturnError("t_ret_nonprimitive"), 31)
                ::
                (SubtypingError(IntType(),
                    ContractReferenceType(ContractType("C_Owned"), Owned(), false), false), 33)
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
                (ArgumentSubtypingError("a", "x",
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
                    "anotherMethod"), 32)
                ::
                (ArgumentSubtypingError("otherMethod", "x",
                    StringType(),
                    IntType()), 34)
                ::
                (ContractUndefinedError("Bogus"), 37)
                ::
                Nil
        )
    }

    @Test def dereferenceTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Dereference.obs",
            (FieldUndefinedError(ContractReferenceType(ContractType("Construct"), Shared(), false), "x"), 30)
                ::
                (DereferenceError(StringType()), 22)
                ::
                (InvalidNonThisFieldAccess(), 25)
                ::
                Nil
        )
    }

    /* This test is temporarily disabled until we add proper support for multiple constructors.
    @Test def constructionTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Construction.obs",
            (ConstructorNameError("Thing"), 27)
                ::
                (RepeatConstructorsError("Thing"), 27)
                ::
                (SubtypingError(ContractReferenceType(ContractType("Thing"), Owned(), false),
                    ContractReferenceType(ContractType("OtherThing"), Inferred(), false)), 27)
                ::
                (WrongArityError(0, 1, "constructor of Thing"), 39)
                ::
                (ArgumentSubtypingError("Thing", "x",
                    StringType(),
                    IntType()), 39)
                ::
                (ArgumentSubtypingError("OtherThing", "x",
                    StringType(),
                    IntType()), 39)
                ::
                (WrongArityError(3, 1, "constructor of Thing"), 39)
                ::
                (WrongArityError(0, 3, "constructor of Thing"), 41)
                ::
                (WrongArityError(1, 3, "constructor of Thing"), 41)
                ::
                (WrongArityError(1, 3, "constructor of OtherThing"), 41)
                ::
                (ArgumentSubtypingError("Thing", "z",
                    IntType(),
                    BoolType()), 41)
                ::
                (ArgumentSubtypingError("Thing", "y",
                    IntType(),
                    StringType()), 41)
                ::
                (ContractUndefinedError("Stuff"), 43)
                ::
                (ContractUndefinedError("Stuff"), 43)
                ::
                Nil
        )
    }
    */

    @Test def branchingTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Branching.obs",
            (MergeIncompatibleError("o1",
                ContractReferenceType(ContractType("LinearContract"), Owned(), false),
                ContractReferenceType(ContractType("LinearContract"), Unowned(), false)), 29)
                ::
                (UnusedOwnershipError("o2"), 29)
                ::
                (UnusedOwnershipError("o2"), 40)
                ::
                (MergeIncompatibleError("o1",
                    ContractReferenceType(ContractType("LinearContract"), Owned(), false),
                    ContractReferenceType(ContractType("LinearContract"), Unowned(), false)), 49)
                ::
                (UnusedOwnershipError("o2"), 49)
                ::
                (VariableUndefinedError("x", null), 61)
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
                StateType("C", "S2", false), true), 3
            )
                ::
                (SubtypingError(
                    StateType("C", "S2", false),
                    StateType("C", "S1", false), false), 8
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
                    StateType("C1", Set("S1", "S3"), false), false), 17
                )
                ::
                (SubtypingError(
                    StateType("C2", Set("S1", "S2"), false),
                    StateType("C2", "S1", false), true), 30
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
        runTest("resources/tests/type_checker_tests/Assets.obs",
            (AssetContractConstructorError("BogusMoney"), 5)
                ::
                (InvalidInconsistentFieldType("money",
                    ContractReferenceType(ContractType("Money"), Unowned(), false),
                    ContractReferenceType(ContractType("Money"), Owned(), false)), 26)
                ::
                (OverwrittenOwnershipError("money"), 27)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Money"), Unowned(), false),
                    ContractReferenceType(ContractType("Money"), Owned(), false), false), 37)
                ::
                (NonAssetOwningAssetError("BadWallet",
                    Field(false,
                        ContractReferenceType(ContractType("Money"), Owned(), false),
                        "money",
                        None)), 43)
                ::
                (ArgumentSubtypingError("discardMoney", "m",
                    ContractReferenceType(ContractType("Money"), Shared(), false),
                    ContractReferenceType(ContractType("Money"), Owned(), false)),
                    56)
                ::
                (DisownUnowningExpressionError(ReferenceIdentifier("m")), 49)
                ::
                (UnusedOwnershipError("m"), 72)
                ::
                Nil
        )
    }

    @Test def ownershipTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Ownership.obs",
            (InvalidInconsistentFieldType("prescription",
                ContractReferenceType(ContractType("Prescription"), Unowned(), false),
                ContractReferenceType(ContractType("Prescription"), Owned(), false)), 15)
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

    @Test def droppedAssetsTest(): Unit = {
        runTest("resources/tests/type_checker_tests/MaybeDroppedAsset.obs",
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
                (FieldUndefinedError(StateType("C", Set("S1"), false), "shared"), 4)
                ::
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
                StateType("C", "S1", false), false), 24)
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
                    ContractReferenceType(ContractType("C"), Unowned(), false),
                    ContractReferenceType(ContractType("C"), Shared(), false)), 39) ::
                (ReceiverTypeIncompatibleError("changeStateOwned",
                    ContractReferenceType(ContractType("C"), Shared(), false),
                    ContractReferenceType(ContractType("C"), Owned(), false)), 42) ::
                (ReceiverTypeIncompatibleError("changeStateStateSpecified",
                    ContractReferenceType(ContractType("C"), Owned(), false),
                    StateType("C", Set("S1"), false)), 45) ::
                (InvalidInconsistentFieldType("s1C", StateType("C", Set("S2"), false), StateType("C", Set("S1"), false)), 48) ::
                Nil
        )
    }

    @Test def fieldTypeMismatchTest(): Unit = {
        runTest("resources/tests/type_checker_tests/FieldTypeMismatch.obs",
            (InvalidInconsistentFieldType("c", StateType("C", Set("S2"), false), StateType("C", Set("S1"), false)), 24) ::
            Nil
        )
    }

    @Test def unownedReferenceTest(): Unit = {
        runTest("resources/tests/type_checker_tests/UnownedReference.obs", Nil)
    }

    @Test def typeInferenceTest(): Unit = {
        runTest("resources/tests/type_checker_tests/TypeInference.obs", Nil)
    }

    @Test def privateTransactionsTest(): Unit = {
        runTest("resources/tests/type_checker_tests/PrivateTransactions.obs",
            (InvalidFinalFieldTypeDeclarationError("bogus"), 30)::
                (FieldTypesDeclaredOnPublicTransactionError("t2"), 33)::
                (InvalidInconsistentFieldType("c", StateType("C", "S2", false), StateType("C", "S1", false)), 42)::
                (FieldSubtypingError("c", StateType("C", "S1", false), StateType("C", "S2", false)), 48)::
            Nil)
    }

    @Test def variableDeclarationsTest(): Unit = {
        runTest("resources/tests/type_checker_tests/VariableDeclarations.obs",
            (InvalidLocalVariablePermissionDeclarationError(), 15)::
            Nil)
    }

    @Test def uninitializedFieldTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Uninitialized.obs",
            ((UninitializedFieldError("x"), 7) ::
             (UninitializedFieldError("z"), 7) ::
                Nil))
    }

    @Test def revertTest(): Unit = {
        runTest("resources/tests/type_checker_tests/Revert.obs",
            (SubtypingError(IntType(), StringType(), false), 12) :: Nil)
    }

    @Test def inStateTest(): Unit = {
        runTest("resources/tests/type_checker_tests/InState.obs",
            (ReceiverTypeIncompatibleError("turnOff",
                ContractReferenceType(ContractType("LightSwitch"), Unowned(), false),
                StateType("LightSwitch", "On", false)), 33) ::
            (ReceiverTypeIncompatibleError("turnOn",
                ContractReferenceType(ContractType("LightSwitch"), Unowned(), false),
                StateType("LightSwitch", "Off", false)), 37) ::
            (StateCheckOnPrimitiveError(), 45) ::
              (StateCheckRedundant(), 56) ::
              (StaticAssertFailed(
                ReferenceIdentifier("s"), Seq("Owned"),
                StateType("LightSwitch", "On", false)), 62) ::
              (StateCheckRedundant(), 67) ::
              Nil)

    }

    @Test def constructorPermissionRequired(): Unit = {
        runTest("resources/tests/parser_tests/ConstructorPermissionRequired.obs", Nil)
    }

    @Test def constructorPermissionRequiredMissing(): Unit = {
        runTest("resources/tests/parser_tests/ConstructorPermissionRequiredMissing.obs",
            (ConstructorAnnotationMissingError("A"), 4)::
                Nil)
    }

    @Test def multipleConstructorAmbiguousTest(): Unit = {
        runTest("resources/tests/type_checker_tests/MultiConstrAmbiguous.obs",
            (AmbiguousConstructorError("B",
                List(AmbiguousConstructorExample("C@S2",
                    VariableDeclWithSpec(
                        StateType("C", Set("S1", "S2", "S3"), false),
                        StateType("C", Set("S1", "S2", "S3"), false),
                        "s1c"),
                    VariableDeclWithSpec(
                        StateType("C", "S2", false),
                        StateType("C", "S2", false),
                        "s2c")))
                    ), 3) ::
                (AmbiguousConstructorError("D",
                    List(AmbiguousConstructorExample("C@S2",
                        VariableDeclWithSpec(
                            ContractReferenceType(ContractType("C"), Owned(), false),
                            ContractReferenceType(ContractType("C"), Owned(), false),
                            "s1c"),
                        VariableDeclWithSpec(
                            StateType("C", "S2", false),
                            StateType("C", "S2", false), "s2c")))
                    ), 16) ::
                (AmbiguousConstructorError("E",
                    List(AmbiguousConstructorExample("C@Owned",
                        VariableDeclWithSpec(
                            StateType("C", Set("S1", "S2", "S3"), false),
                            StateType("C", Set("S1", "S2", "S3"), false),
                            "s1c"),
                        VariableDeclWithSpec(
                            ContractReferenceType(ContractType("C"), Unowned(), false),
                            ContractReferenceType(ContractType("C"), Unowned(), false),
                            "s2c"))),
                    ), 21) ::
                (AmbiguousConstructorError("F",
                    List(AmbiguousConstructorExample("C@Owned",
                        VariableDeclWithSpec(
                            ContractReferenceType(ContractType("C"), Owned(), false),
                            ContractReferenceType(ContractType("C"), Owned(), false),
                            "s1c"),
                    VariableDeclWithSpec(
                        ContractReferenceType(ContractType("C"), Shared(), false),
                        ContractReferenceType(ContractType("C"), Shared(), false),
                        "s2c"))),
                    ), 26) ::
        Nil)
    }

    @Test def multipleConstructorDistinguishableTest(): Unit = {
        runTest("resources/tests/type_checker_tests/MultiConstrDistinguishable.obs",
            Nil)
    }

    @Test def interfaceDoesntRequireReturnTest(): Unit = {
        runTest("resources/tests/type_checker_tests/InterfaceWithReturn.obs", Nil)
    }

    @Test def assetStateTrackingOwned(): Unit = {
        runTest("resources/tests/type_checker_tests/AssetStateTracking.obs",
            (UnusedOwnershipError("c"), 10) ::
                Nil)
    }

    @Test def assetStateTrackingOwnedOkay(): Unit = {
        runTest("resources/tests/type_checker_tests/AssetStateTrackingOkay.obs", Nil)
    }
}
