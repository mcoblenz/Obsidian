module silica where

-- open import Data.AVL.Sets using (⟨Set⟩)

open import Agda.Builtin.Bool
--open import Nat
--open import Prelude
open import Data.Nat
open import Data.List

-------------- Syntax ------------

Id : Set
Id = ℕ

-- State sets
σ : Set
σ = List Id

data Tst : Set where
  Owned : Tst
  Unowned : Tst
  Shared : Tst
  states : σ → Tst

data Tc : Set where
  tc : Id -> Tst -> Tc

contractName : Tc → Id
contractName (tc id _) = id

data Tbase : Set where
  Void : Tbase
  Boolean : Tbase

data Type : Set where
  base : Tbase -> Type
  contractType : Tc -> Type

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


data Contract : Set where
  contract : Bool -> Id -> List State -> List Field -> List PublicTransaction -> Contract




data Program : Set where
  program : List Contract -> Expr -> Program

-------------- Static Semantics ------------------
-- Splitting --
data _⇛_/_ : Type -> Type -> Type -> Set where
  void :
        ---------------
        base Void ⇛ base Void / base Void

  boolean :
        --------------
        base Boolean ⇛ base Boolean / base Boolean
        
  unowned :
          ∀ {T1 T2 T3 : Type}
          → (contractName T1) ≡ (contractName T2)
    -- also contractName tc1 == contractName tc2
        --------------
        → T1 ⇛ T2 / T3 -- contractType (tc (contractName tc1) Unowned)
