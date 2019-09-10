package edu.cmu.cs.obsidian.tests


import java.io.FileInputStream

import org.junit.Assert.{assertTrue, fail}
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import edu.cmu.cs.obsidian.typecheck._
import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ArrayBuffer

class TypeCheckerTests extends JUnitSuite {
    type LineNumber = Int

    private val topBound = GenericBoundPerm(_: Boolean, _: Boolean, ContractType.topContractType, Unowned())

    private def runTest(file: String, expectedErrors: Seq[(Error, LineNumber)]): Unit = {
        var prog: Program = null
        try {
            prog = Parser.parseFileAtPath(file, new FileInputStream(file), printTokens = false)
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
            (SubtypingError(BoolType(), IntType(), false), 39)
                ::
                (InconsistentContractTypeError(ContractType("C_Shared", Nil), ContractType("C_Owned", Nil)), 42)
                ::
                (FieldUndefinedError(ContractReferenceType(ContractType("C_Shared", Nil), Shared(), NotRemoteReferenceType()), "f2"), 21)
                ::
                (FieldUndefinedError(ContractReferenceType(ContractType("C_Shared", Nil), Shared(), NotRemoteReferenceType()), "f3"), 23)
                ::
                (VariableUndefinedError("j", null), 50)
                ::
                (AssignmentError(), 52)
                ::
                (InvalidNonThisFieldAssignment(), 55)
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
                    ContractReferenceType(ContractType("C_Owned", Nil), Owned(), NotRemoteReferenceType()), false), 33)
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
                    ContractReferenceType(ContractType("Invocation", Nil), Shared(), NotRemoteReferenceType()),
                    "otherMethod"), 23)
                ::
                (NonInvokeableError(IntType()), 25)
                ::
                (MethodUndefinedError(
                    ContractReferenceType(ContractType("OtherContract", Nil), Shared(), NotRemoteReferenceType()),
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
            (FieldUndefinedError(ContractReferenceType(ContractType("Construct", Nil), Shared(), NotRemoteReferenceType()), "x"), 30)
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
                (SubtypingError(ContractReferenceType(ContractType("Thing", Nil), Owned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("OtherThing", Nil), Inferred(), NotRemoteReferenceType())), 27)
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
                ContractReferenceType(ContractType("LinearContract", Nil), Unowned(), NotRemoteReferenceType()),
                ContractReferenceType(ContractType("LinearContract", Nil), Owned(), NotRemoteReferenceType())), 29)
                ::
                (UnusedOwnershipError("o2"), 29)
                ::
                (UnusedOwnershipError("o2"), 40)
                ::
                (MergeIncompatibleError("o1",
                    ContractReferenceType(ContractType("LinearContract", Nil), Owned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("LinearContract", Nil), Unowned(), NotRemoteReferenceType())), 49)
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
                    StateType(ContractType("C", Nil), "S1", NotRemoteReferenceType()), "x"), 18)
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
            (ContractUndefinedError("OtherThing"), 3) ::
                (ContractUndefinedError("OtherThing"), 7) ::
                (ContractUndefinedError("OtherThing"), 7) ::
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
                StateType(ContractType("C", Nil), "S1", NotRemoteReferenceType()),
                StateType(ContractType("C", Nil), "S2", NotRemoteReferenceType()), true), 3
            )
                ::
                (SubtypingError(
                    StateType(ContractType("C", Nil), "S2", NotRemoteReferenceType()),
                    StateType(ContractType("C", Nil), "S1", NotRemoteReferenceType()), false), 8
                )
                ::
                Nil
        )
    }

    @Test def endsInStateUnionTest(): Unit = {
        runTest("resources/tests/type_checker_tests/EndsInStateUnion.obs",
            (StateUndefinedError("C1", "OtherState"), 12)
                ::
                (NonPrimitiveTypeTransformError(StateType(ContractType("C1", Nil), Set("S2", "OtherState"), NotRemoteReferenceType()),
                    BottomType()), 12)
                ::
                (SubtypingError(
                    StateType(ContractType("C1", Nil), Set("S1", "S2"), NotRemoteReferenceType()),
                    StateType(ContractType("C1", Nil), Set("S1", "S3"), NotRemoteReferenceType()), false), 17
                )
                ::
                (SubtypingError(
                    StateType(ContractType("C2", Nil), Set("S1", "S2"), NotRemoteReferenceType()),
                    StateType(ContractType("C2", Nil), "S1", NotRemoteReferenceType()), true), 30
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
                    ContractReferenceType(ContractType("Money", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("Money", Nil), Owned(), NotRemoteReferenceType())), 26)
                ::
                (OverwrittenOwnershipError("money"), 27)
                ::
                (SubtypingError(
                    ContractReferenceType(ContractType("Money", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("Money", Nil), Owned(), NotRemoteReferenceType()), false), 37)
                ::
                (NonAssetOwningAssetError("BadWallet",
                    Field(false,
                        ContractReferenceType(ContractType("Money", Nil), Owned(), NotRemoteReferenceType()),
                        "money",
                        None)), 43)
                ::
                (ArgumentSubtypingError("discardMoney", "m",
                    ContractReferenceType(ContractType("Money", Nil), Shared(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("Money", Nil), Owned(), NotRemoteReferenceType())),
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
                ContractReferenceType(ContractType("Prescription", Nil), Unowned(), NotRemoteReferenceType()),
                ContractReferenceType(ContractType("Prescription", Nil), Owned(), NotRemoteReferenceType())), 15)
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
            (OverwrittenOwnershipError("c"), 46)::
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
                (FieldUndefinedError(StateType(ContractType("C", Nil), Set("S1"), NotRemoteReferenceType()), "shared"), 4)
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
                (StaticAssertFailed(This(), States(Set("S2")), StateType(ContractType("C", Nil), Set("S1", "S2"), NotRemoteReferenceType())), 17)
                ::
                (StaticAssertFailed(ReferenceIdentifier("ow"), Unowned(), ContractReferenceType(ContractType("C", Nil), Owned(), NotRemoteReferenceType())), 24)
                ::
                Nil
        )
    }

    @Test def typeSpecificationTest(): Unit = {
        runTest("resources/tests/type_checker_tests/TypeSpecification.obs",
            (SubtypingError(StateType(ContractType("C", Nil), Set("S3", "S2"), NotRemoteReferenceType()),
                StateType(ContractType("C", Nil), "S1", NotRemoteReferenceType()), false), 24)
                ::
                (ArgumentSpecificationError("a", "badChangeA",
                    StateType(ContractType("A", Nil), "Unavailable", NotRemoteReferenceType()),
                    StateType(ContractType("A", Nil), "Available", NotRemoteReferenceType())), 51)
                ::
                (ArgumentSpecificationError("a", "badChangeA2",
                    StateType(ContractType("A", Nil), "Available", NotRemoteReferenceType()),
                    StateType(ContractType("A", Nil), "Unavailable", NotRemoteReferenceType())), 56)
                ::
                Nil
        )
    }

    @Test def readOnlyStateTest(): Unit = {
        runTest("resources/tests/type_checker_tests/ReadOnlyState.obs",
            (TransitionNotAllowedError(), 11) ::
                (ReceiverTypeIncompatibleError("changeStateShared",
                    ContractReferenceType(ContractType("C", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("C", Nil), Shared(), NotRemoteReferenceType())), 39) ::
                (ReceiverTypeIncompatibleError("changeStateOwned",
                    ContractReferenceType(ContractType("C", Nil), Shared(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("C", Nil), Owned(), NotRemoteReferenceType())), 42) ::
                (ReceiverTypeIncompatibleError("changeStateStateSpecified",
                    ContractReferenceType(ContractType("C", Nil), Owned(), NotRemoteReferenceType()),
                    StateType(ContractType("C", Nil), Set("S1"), NotRemoteReferenceType())), 45) ::
                (InvalidInconsistentFieldType("s1C", StateType(ContractType("C", Nil), Set("S2"), NotRemoteReferenceType()), StateType(ContractType("C", Nil), Set("S1"), NotRemoteReferenceType())), 48) ::
                Nil
        )
    }

    @Test def fieldTypeMismatchTest(): Unit = {
        runTest("resources/tests/type_checker_tests/FieldTypeMismatch.obs",
            (InvalidInconsistentFieldType("c", StateType(ContractType("C", Nil), Set("S2"), NotRemoteReferenceType()), StateType(ContractType("C", Nil), Set("S1"), NotRemoteReferenceType())), 24) ::
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
                (InvalidInconsistentFieldType("c", StateType(ContractType("C", Nil), "S2", NotRemoteReferenceType()), StateType(ContractType("C", Nil), "S1", NotRemoteReferenceType())), 42)::
                (FieldSubtypingError("c", StateType(ContractType("C", Nil), "S1", NotRemoteReferenceType()), StateType(ContractType("C", Nil), "S2", NotRemoteReferenceType())), 48)::
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
                ContractReferenceType(ContractType("LightSwitch", Nil), Unowned(), NotRemoteReferenceType()),
                StateType(ContractType("LightSwitch", Nil), "On", NotRemoteReferenceType())), 33) ::
            (ReceiverTypeIncompatibleError("turnOn",
                ContractReferenceType(ContractType("LightSwitch", Nil), Unowned(), NotRemoteReferenceType()),
                StateType(ContractType("LightSwitch", Nil), "Off", NotRemoteReferenceType())), 37) ::
            (StateCheckOnPrimitiveError(), 45) ::
              (StateCheckRedundant(), 56) ::
              (StaticAssertFailed(
                ReferenceIdentifier("s"), Owned(),
                StateType(ContractType("LightSwitch", Nil), "On", NotRemoteReferenceType())), 62) ::
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
                        StateType(ContractType("C", Nil), Set("S1", "S2", "S3"), NotRemoteReferenceType()),
                        StateType(ContractType("C", Nil), Set("S1", "S2", "S3"), NotRemoteReferenceType()),
                        "s1c"),
                    VariableDeclWithSpec(
                        StateType(ContractType("C", Nil), "S2", NotRemoteReferenceType()),
                        StateType(ContractType("C", Nil), "S2", NotRemoteReferenceType()),
                        "s2c")))
                    ), 3) ::
                (AmbiguousConstructorError("D",
                    List(AmbiguousConstructorExample("C@S2",
                        VariableDeclWithSpec(
                            ContractReferenceType(ContractType("C", Nil), Owned(), NotRemoteReferenceType()),
                            ContractReferenceType(ContractType("C", Nil), Owned(), NotRemoteReferenceType()),
                            "s1c"),
                        VariableDeclWithSpec(
                            StateType(ContractType("C", Nil), "S2", NotRemoteReferenceType()),
                            StateType(ContractType("C", Nil), "S2", NotRemoteReferenceType()), "s2c")))
                    ), 16) ::
                (AmbiguousConstructorError("E",
                    List(AmbiguousConstructorExample("C@Owned",
                        VariableDeclWithSpec(
                            StateType(ContractType("C", Nil), Set("S1", "S2", "S3"), NotRemoteReferenceType()),
                            StateType(ContractType("C", Nil), Set("S1", "S2", "S3"), NotRemoteReferenceType()),
                            "s1c"),
                        VariableDeclWithSpec(
                            ContractReferenceType(ContractType("C", Nil), Unowned(), NotRemoteReferenceType()),
                            ContractReferenceType(ContractType("C", Nil), Unowned(), NotRemoteReferenceType()),
                            "s2c"))),
                    ), 21) ::
                (AmbiguousConstructorError("F",
                    List(AmbiguousConstructorExample("C@Owned",
                        VariableDeclWithSpec(
                            ContractReferenceType(ContractType("C", Nil), Owned(), NotRemoteReferenceType()),
                            ContractReferenceType(ContractType("C", Nil), Owned(), NotRemoteReferenceType()),
                            "s1c"),
                    VariableDeclWithSpec(
                        ContractReferenceType(ContractType("C", Nil), Shared(), NotRemoteReferenceType()),
                        ContractReferenceType(ContractType("C", Nil), Shared(), NotRemoteReferenceType()),
                        "s2c"))),
                    ), 26) ::
        Nil)
    }

    @Test def multipleConstructorDistinguishableTest(): Unit = {
        runTest("resources/tests/type_checker_tests/MultiConstrDistinguishable.obs",
            Nil)
    }

    @Test def transactionParamShadowTest(): Unit = {
        val contractParam = GenericType(GenericVar(isAsset = false, "X", Some("s")), topBound(false, false))
        val fParam = GenericType(GenericVar(isAsset = false, "X", Some("t")), topBound(false, false))
        val gParam = GenericType(GenericVar(isAsset = false, "Y", Some("s")), topBound(false, false))

        runTest("resources/tests/type_checker_tests/TransactionParamShadow.obs",
            (GenericParamShadowError("f", fParam, "TransactionParamShadow", contractParam), 3) ::
            (GenericParamShadowError("f", gParam, "TransactionParamShadow", contractParam), 6) ::
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

    @Test def okayBasicGenerics(): Unit = {
        runTest("resources/tests/type_checker_tests/BasicGenerics.obs", Nil)
    }

    @Test def genericsMismatchIntString(): Unit = {
        runTest("resources/tests/type_checker_tests/GenericsIntStringMismatch.obs",
            (SubtypingError(IntType(), StringType(), isThis = false), 18) :: Nil)
    }

    @Test def genericsOwnership(): Unit = {
        runTest("resources/tests/type_checker_tests/GenericsOwnership.obs",
            (InvalidInconsistentFieldType("x",
                GenericType(GenericVar(isAsset = false,"T",None),GenericBoundPerm(false, false, ContractType.topContractType, Unowned())),
                GenericType(GenericVar(isAsset = false,"T",None),GenericBoundPerm(false, false, ContractType.topContractType, Owned()))), 12) ::
            Nil)
    }

    @Test def genericsInterfaceBasicSwitch(): Unit = {
        runTest("resources/tests/type_checker_tests/GenericInterfaceSwitch.obs",
            (InterfaceInstantiationError("Switch"), 52) ::
            (ReceiverTypeIncompatibleError("turnOff",
                 StateType(ContractType("Switch", Nil), "Off", NotRemoteReferenceType()),
                 StateType(ContractType("Switch", Nil), "On", NotRemoteReferenceType())), 38) ::
            (MissingStateImplError("NoImplSwitch", "Switch", "On"), 29) ::
            (MissingStateImplError("NoImplSwitch", "Switch", "Off"), 29) ::
            (MissingTransactionImplError("NoImplSwitch", "Switch", "turnOn"), 29) ::
            (MissingTransactionImplError("NoImplSwitch", "Switch", "turnOff"), 29) ::
                Nil)
    }

    @Test def genericsInterfaceImplementsBound(): Unit = {
        runTest("resources/tests/type_checker_tests/GenericInterfaceBounds.obs",
            (MethodUndefinedError(GenericType(GenericVar(isAsset = false,"T",None),
                GenericBoundPerm(interfaceSpecified = false, permSpecified = false, ContractType.topContractType,Owned())),"validate"), 29) ::
            (ArgumentSubtypingError("store", "a",
                StateType(ContractType("NoImplValidatable", Nil), "Invalid", NotRemoteReferenceType()),
                StateType(ContractType("DummyValidatable", Nil), "Invalid", NotRemoteReferenceType())), 67) ::
            (GenericParameterError(GenericType(GenericVar(isAsset = false,"A",None),
                GenericBoundPerm(interfaceSpecified = true, permSpecified = false, ContractType("Validatable",Nil),Unowned())),
                ContractReferenceType(ContractType("NoImplValidatable", Nil), Inferred(), NotRemoteReferenceType())), 73) ::
            Nil)
    }

    @Test def genericsInterfaceWithParameters(): Unit = {
        val consumerBound =
            GenericBoundPerm(interfaceSpecified = true, permSpecified = false, ContractType("Consumer",
                List(GenericType(GenericVar(isAsset = false, "U", None),
                    GenericBoundPerm(interfaceSpecified = false, permSpecified = false, ContractType.topContractType, Inferred())))), Unowned())

        runTest("resources/tests/type_checker_tests/GenericInterfaceParameters.obs",
            (ArgumentSubtypingError("consume", "x", StringType(), IntType()), 49) ::
            (ArgumentSubtypingError("Store", "t",
                ContractReferenceType(ContractType("NopConsumer", List(StringType())), Owned(), NotRemoteReferenceType()),
                ContractReferenceType(ContractType("Consumer", List(IntType())), Owned(), NotRemoteReferenceType())), 59) ::
            (ArgumentSubtypingError("consume", "u",
                ContractReferenceType(ContractType("A", Nil), Owned(), NotRemoteReferenceType()),
                IntType()), 65) ::
            (GenericParameterError(
                GenericType(GenericVar(isAsset = false, "T", None), consumerBound),
                ContractReferenceType(ContractType("A", Nil), Inferred(), NotRemoteReferenceType())
            ), 70) ::
            (GenericParameterError(
                GenericType(GenericVar(isAsset = false,"T",None), consumerBound),
                ContractReferenceType(ContractType("NopConsumer", List(StringType())), Inferred(), NotRemoteReferenceType())), 73) ::
            (GenericParameterError(
                GenericType(GenericVar(isAsset = false,"T",None), consumerBound),
                ContractReferenceType(ContractType("NopConsumer", List()), Inferred(), NotRemoteReferenceType())), 76) ::
            // Intentionally duplicated error: one for the type in the generic, one for the actual construction
            (GenericParameterListError(1, 0), 76) ::
            (GenericParameterListError(1, 0), 76) ::
            Nil)
    }

    @Test def genericsTransactionParams(): Unit = {
        runTest("resources/tests/type_checker_tests/GenericsTransactionParams.obs", Nil)
    }

    @Test def genericsStateVariables(): Unit = {
        runTest("resources/tests/type_checker_tests/GenericsStateVariables.obs",
            (InvalidInconsistentFieldType("val",
                GenericType(GenericVar(isAsset = false,"X",None),
                    GenericBoundPerm(interfaceSpecified = false, permSpecified = false, ContractType.topContractType, Unowned())),
                GenericType(GenericVar(isAsset = false,"X",Some("s")),
                    GenericBoundPerm(interfaceSpecified = false, permSpecified = false, ContractType.topContractType, Unowned()))), 16) ::
            (ReceiverTypeIncompatibleError("getX",
                StateType(ContractType("A", Nil), "S2", NotRemoteReferenceType()),
                StateType(ContractType("A", Nil), "S1", NotRemoteReferenceType())), 66) ::
            (ReceiverTypeIncompatibleError("getX",
                ContractReferenceType(ContractType("A", Nil), Unowned(), NotRemoteReferenceType()),
                StateType(ContractType("A", Nil), "S1", NotRemoteReferenceType())), 72) ::
                Nil)
    }

    @Test def genericsAssets(): Unit = {
        runTest("resources/tests/type_checker_tests/GenericsAssets.obs",
            (UnusedOwnershipError("x"), 37) ::
            (UnusedOwnershipError("val"), 48) ::
            (GenericParameterAssetError("X", "C"), 63) ::
            (ReceiverTypeIncompatibleError("go",
                GenericType(GenericVar(isAsset = false, "X", Some("s")),
                    GenericBoundPerm(interfaceSpecified = true, permSpecified = true, ContractType("Go", Nil), Unowned())),
                ContractReferenceType(ContractType("Go", Nil), Owned(), NotRemoteReferenceType())), 55) ::
                Nil)
    }

    @Test def fieldOwnership(): Unit = {
        runTest("resources/tests/type_checker_tests/FieldOwnershipDiscrepancy.obs",
            (InvalidInconsistentFieldType("c",
                ContractReferenceType(ContractType("C", Nil), Owned(), NotRemoteReferenceType()),
                ContractReferenceType(ContractType("C", Nil), Unowned(), NotRemoteReferenceType()))
            , 8) :: Nil)
    }

    @Test def genericsLinkedList(): Unit = {
        runTest("resources/tests/type_checker_tests/GenericLinkedList.obs", Nil)
    }

    @Test def permissionPassing(): Unit = {
        runTest("resources/tests/type_checker_tests/PermissionPassing.obs",
            (ReceiverTypeIncompatibleError("t5",
                ContractReferenceType(ContractType("PermissionPassing", Nil), Unowned(), NotRemoteReferenceType()),
                ContractReferenceType(ContractType("PermissionPassing", Nil), Owned(), NotRemoteReferenceType())), 47) ::
                (UnusedExpressionArgumentOwnershipError(LocalInvocation("returnOwnedAsset", Nil, Nil, Nil)), 58) ::
                (LostOwnershipErrorDueToSharing(LocalInvocation("returnOwnedAsset", Nil, Nil, Nil)), 65) ::
                Nil)
    }

    @Test def allPermissions(): Unit = {
        runTest("resources/tests/type_checker_tests/AllPermissions.obs",
            (SubtypingError(ContractReferenceType(ContractType("AllPermissions", Nil), Shared(), NotRemoteReferenceType()),
                ContractReferenceType(ContractType("AllPermissions", Nil), Owned(), NotRemoteReferenceType()), false), 19) ::
                (SubtypingError(ContractReferenceType(ContractType("AllPermissions", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Owned(), NotRemoteReferenceType()), false), 25) ::
                (SubtypingError(ContractReferenceType(ContractType("AllPermissions", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Shared(), NotRemoteReferenceType()), false), 29) ::
                (LostOwnershipErrorDueToSharing(ReferenceIdentifier("x2")), 41) ::
                (LostOwnershipErrorDueToSharing(ReferenceIdentifier("x7")), 63) ::
                (ReceiverTypeIncompatibleError("t1",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Shared(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Owned(), NotRemoteReferenceType())), 81) ::
                (ReceiverTypeIncompatibleError("t4",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Shared(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Owned(), NotRemoteReferenceType())), 96) ::
                (ReceiverTypeIncompatibleError("t5",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Shared(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Owned(), NotRemoteReferenceType())), 101) ::
                (ReceiverTypeIncompatibleError("t1",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Owned(), NotRemoteReferenceType())), 130) ::
                (ReceiverTypeIncompatibleError("t2",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Shared(), NotRemoteReferenceType())), 135) ::
                (ReceiverTypeIncompatibleError("t4",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Owned(), NotRemoteReferenceType())), 144) ::
                (ReceiverTypeIncompatibleError("t5",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Owned(), NotRemoteReferenceType())), 148) ::
                (ReceiverTypeIncompatibleError("t6",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Shared(), NotRemoteReferenceType())), 152) ::
                (ReceiverTypeIncompatibleError("t7",
                    ContractReferenceType(ContractType("AllPermissions", Nil), Unowned(), NotRemoteReferenceType()),
                    ContractReferenceType(ContractType("AllPermissions", Nil), Shared(), NotRemoteReferenceType())), 158) ::
                (UnusedOwnershipError("this"), 172) ::
                Nil)
    }

    @Test def duplicateContractDecl(): Unit = {
        runTest("resources/tests/type_checker_tests/DuplicateContractDecl.obs",
            (DuplicateContractError("Contract"), 2) :: Nil)
    }

    @Test def constructorFieldTypes(): Unit = {
        runTest("resources/tests/type_checker_tests/ConstructorFieldTypes.obs",
            (InvalidInconsistentFieldType("seller",
                StateType(ContractType("Seller", Nil),  "InAuction", NotRemoteReferenceType()),
                StateType(ContractType("Seller", Nil),  "Unsold", NotRemoteReferenceType())), 11) :: Nil)
    }
}
