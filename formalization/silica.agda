module silica where

-- open import Data.AVL.Sets using (⟨Set⟩)

open import Agda.Builtin.Bool
open import Data.Bool
open import Prelude
open import Data.Nat
open import Data.List
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open import Context

-------------- Syntax ------------
eq : zero ≡ zero
eq = refl

Id : Set
Id = ℕ

-- State sets
σ : Set
σ = List Id

data Tst : Set where
  Owned : Tst
  Unowned : Tst
  Shared : Tst
  S : σ → Tst

record Tc : Set where
  field
    contractName : Id
    tst : Tst

data Tbase : Set where
  Void : Tbase
  Boolean : Tbase

data Type : Set where
  base : Tbase -> Type
  contractType : Tc -> Type


isShared : Type → Bool
isShared (contractType (record {contractName = _ ; tst = Shared})) = true
isShared _ = false

data Field : Set where

data State : Set where
  state : Id -> List Field -> State

data Targ : Set where
  arg-trans : Tc -> Tst -> Targ
  base : Tbase -> Targ

data Expr : Set where
  var : Id -> Expr
  -- TODO: add the rest of the expressions

record PublicTransaction : Set where
  constructor publicTransaction
  field
    retType : Type
    name : Id
    argType : Targ
    argName : Id
    initialState : Tst
    finalState : Tst
    expr : Expr

-- TODO: add private transactions

record Contract : Set where
  constructor contract
  field
    isAsset : Bool
    name : Id
    states : List State
    fields : List Field
    transactions : List PublicTransaction


data Program : Set where
  program : List Contract -> Expr -> Program

-------------- Static Semantics ------------------
-- A ContractEnv (written Γ) maps from Ids to contract definitions.
module ContractEnv = Context Contract

-- Helper judgments --

--data _⊢_NotAsset : ContractEnv.·ctx → Id → Set where
data NotAsset : ContractEnv.·ctx → Id → Set where
  inContext :
    {Γ : ContractEnv.·ctx}
    → {id : Id}
    → (contr : Contract)
    → (p : Contract.isAsset contr ≡ false)
    → (q : (ContractEnv._∈_) (id , contr) Γ)
    -------------
    → NotAsset Γ id

-- Splitting --
data _⊢_⇛_/_ : ContractEnv.·ctx -> Type -> Type -> Type -> Set where
  void : ∀ {Γ : ContractEnv.·ctx}
        ---------------
        → Γ ⊢ (base Void) ⇛ (base Void) / (base Void)

  boolean : ∀ {Γ : ContractEnv.·ctx}
        --------------
        → Γ ⊢ base Boolean ⇛ base Boolean / base Boolean
        
  unowned : ∀ {Γ : ContractEnv.·ctx}
          → ∀ {t1 t2 t3 : Tc}
          → (Tc.contractName t1) ≡ (Tc.contractName t2)
          → (Tc.tst t1) ≡ (Tc.tst t2)
          → Tc.tst t3 ≡ Unowned
        --------------
        → Γ ⊢ contractType t1 ⇛ contractType t2 / contractType t3

  shared-shared-shared :
    ∀ {c : Id}
    → ∀ {Γ : ContractEnv.·ctx}
    → Γ ⊢ contractType ( record {tst = Shared ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )

  owned-shared :
   ∀ {c : Id}
   → ∀ {Γ : ContractEnv.·ctx}
   → NotAsset Γ c
   --------------
    → Γ ⊢  contractType ( record {tst = Owned ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )

  states-shared :
    ∀ {s : σ}
    → ∀ {c : Id}
    → ∀ {Γ : ContractEnv.·ctx}
    → NotAsset Γ c
    --------------
    → Γ ⊢  contractType ( record {tst = S s ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )


----------- PROPERTIES OF SPLITTING -----------
-- The results of splitting are always compatible.
-- TODO



-- Some tests to see if I know what I'm doing.

owned1 : Type
owned1 = contractType  (record {contractName = zero ; tst = Owned })
unowned1 : Type
unowned1 = contractType (record {contractName = zero ; tst = Unowned })
shared1 : Type
shared1 = contractType (record {contractName = zero ; tst = Shared })

test1 : ContractEnv.∅ ⊢ owned1 ⇛ owned1 / unowned1
test1 = unowned refl refl refl

isSharedTest1 = isShared shared1
isSharedTest2 = isShared owned1

open import IO

main = run (putStrLn (if isSharedTest2 then "true" else "false"))
