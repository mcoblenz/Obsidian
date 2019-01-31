module silica where

-- open import Data.AVL.Sets using (⟨Set⟩)

open import Agda.Builtin.Bool
open import Data.Bool
open import Prelude
open import Data.Nat
open import Data.List
open import Data.Nat.Properties
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Data.Maybe using (just)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning
open import Data.Product as Prod using (∃; ∃-syntax)
import Context
open import Data.List.Membership.DecSetoid ≡-decSetoid
open import Data.List.Any

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

IndirectRef : Set
IndirectRef = Id
  
data SimpleExpression : Set where
  var : Id -> SimpleExpression
  loc : IndirectRef -> SimpleExpression

record Object : Set where
  field
    contractName : Id
    stateName : Id
    contractFields : List SimpleExpression
    stateFields : List SimpleExpression

data Expr : Set where
  bool : Bool → Expr
  var : Id -> Expr
  void : Expr
  obj : Object → Expr
  loc : IndirectRef → Expr
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

--============= Utilities ================
data FreeLocations : Expr → List IndirectRef → Set where
  boolFL : ∀ {b : Bool} → FreeLocations (bool b) []
  varFL : ∀ {x : Id} → FreeLocations (var x) []
  void : FreeLocations (void) []
  obj : ∀ {o : Object} → FreeLocations (obj o) []
  loc : ∀ {l : IndirectRef} → FreeLocations (loc l) [ l ]

zeroList = [ zero ]
zeroInList : zero ∈ zeroList
zeroInList = Any.here refl




--=============== Static Semantics ================
-- A ContractEnv (written Γ) maps from Ids to contract definitions.
module ContractEnv = Context Contract

module TypeEnvContext = Context Type
TypeEnv = TypeEnvContext.·ctx
open TypeEnvContext

-- Helper judgments --

--data _⊢_NotAsset : ContractEnv.·ctx → Id → Set where
data NotAsset : ContractEnv.·ctx → Id → Set where
  inContext :
    {Γ : ContractEnv.·ctx}
    → {id : Id}
    → (contr : Contract)
    → (p : Contract.isAsset contr ≡ false)
    → (q : (ContractEnv._∈̇_) (id , contr) Γ)
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

------------ Type judgments ----------------
data _⊢_⦂_⊣_ : TypeEnv → Expr → Type → TypeEnv → Set where
  Var : ∀ {Γ : ContractEnv.·ctx}
      → ∀ {Δ : TypeEnv}
      → ∀ {T₁ T₂ T₃ : Type}
      → ∀ {x : Id}
      → Γ ⊢ T₁ ⇛ T₂ / T₃
      ------------------------------------
      → (Δ ,, (x , T₁)) ⊢ (var x) ⦂ T₂ ⊣ (Δ ,, (x , T₃))

------------ DYNAMIC SEMANTICS --------------
data Value : Expr → Set where
  bool : ∀ {b : Bool}
         ------------
         → Value (bool b)

  void : Value (void)

  obj : ∀ (o : Object)
        --------------
        → Value (obj o)


-- μ
module ObjectRefContext = Context Object
ObjectRefEnv = ObjectRefContext.·ctx

-- ρ
module IndirectRefContext = Context Expr -- TODO: require that these are all values
IndirectRefEnv = IndirectRefContext.·ctx

-- φ
module StateLockingContext = Context Bool
StateLockingEnv = StateLockingContext.·ctx

-- ψ
module ReentrancyContext = Context Bool
ReentrancyEnv = ReentrancyContext.·ctx


record RuntimeEnv : Set where
  field
    μ : ObjectRefEnv
    ρ : IndirectRefEnv
    φ : StateLockingEnv
    ψ : ReentrancyEnv


--============= Consistency ==============

------------ Global Consistency -----------
data ReferenceConsistency : RuntimeEnv → TypeEnv → Set where
  -- TODO
  
data _&_ok : RuntimeEnv → TypeEnv → Set where
  ok : ∀ {Σ : RuntimeEnv}
       → {Δ : TypeEnv}
         → ∀ {e : Expr} → ∀ {l : IndirectRef} → ∀ {T : Type}
         → (l , T) ∈̇ Δ
         → ∃[ fl ] ((FreeLocations e fl) → (l ∈ fl))
         → ∃[ v ] (RuntimeEnv.ρ Σ l ≡ just v)
       → ReferenceConsistency Σ Δ
     

       -- TODO: add remaining antecedents
       ---------------------------
       → Σ & Δ ok

-- Inversion for global consistency
refConsistency : ∀ {Σ : RuntimeEnv}
                 → ∀ {Δ : TypeEnv}                 
                 → Σ & Δ ok → ReferenceConsistency Σ Δ
refConsistency (ok _ _ _ rc) =  rc

------------ Lemmas --------------
-- If an expression is well-typed in Δ and Σ, Δ ok, then all locations in the expression are in the domain of the context.
{- TODO 
locationsOK : ∀ {Δ Δ' : TypeEnv}
              → ∀ {Σ : RuntimeEnv}
              → ∀ {e : Expr}
              → ∀ {T : Type}
              → ∀ {l : IndirectRef}
              → Δ ⊢ e ⦂ T ⊣ Δ'
              → Σ & Δ ok
              -- TODO!
--              → {!Data.List.Membership.Setoid._∈_ l ?!}
--              → l ∈ (fl (e)) -- (fl e) is a list of locations, so shouldn't this be right?
              ------------------------
              → ∃[ v ] ((RuntimeEnv.ρ Σ l) ≡ just v)

locationsOK ty consis =  let refConsis = (refConsistency consis)
                          in {!!}
-}
----------- Reduction Rules ------------
{-
data _,_⟶_,_ : RuntimeEnv → Expr → RuntimeEnv → Expr → Set where
  SElookup :
    ∀ {Σ : RuntimeEnv}
    → ∀ {l : IndirectRef}
    → Σ , (loc l) ⟶ Σ , (loc (RuntimeEnv.ρ Σ l))
-}
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
