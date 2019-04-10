module silica where

-- open import Data.AVL.Sets using (⟨Set⟩)

open import Agda.Builtin.Bool
open import Data.Bool using (true; false)
open import Prelude
open import Data.Nat
open import Data.List
open import Data.Nat.Properties
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Relation.Binary using (module StrictTotalOrder)
open import Data.Maybe using (just)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; _≢_; refl; cong; sym)
open Eq.≡-Reasoning
open import Data.Product using (_×_; proj₁; proj₂; ∃-syntax; ∃) renaming (_,_ to ⟨_,_⟩)
import Context
open import Data.List.Membership.DecSetoid ≡-decSetoid
open import Data.List.Any
open import Data.List.All
open import Data.Empty
open import Data.Sum

import Data.AVL.Sets
module StateSet = Data.AVL.Sets Data.Nat.Properties.<-strictTotalOrder
open StateSet

-------------- Syntax ------------
Id : Set
Id = ℕ

-- State sets
σ : Set
σ = StateSet.⟨Set⟩

_⊆_ : σ → σ → Set
s₁ ⊆ s₂ = (x : ℕ) → (x StateSet.∈? s₁) ≡ true → (x StateSet.∈? s₂) ≡ true

data Perm : Set where
  Owned : Perm
  Unowned : Perm
  Shared : Perm
  S : σ → Perm

record Tc : Set where
  constructor tc
  field
    contractName : Id
    perm : Perm

data Tbase : Set where
  Void : Tbase
  Boolean : Tbase

data Type : Set where
  base : Tbase -> Type
  contractType : Tc -> Type


isShared : Type → Bool
isShared (contractType (record {contractName = _ ; perm = Shared})) = true
isShared _ = false

eqContractTypes : ∀ {t₁ : Tc}
                  → ∀ {t₂ : Tc}
                  → Tc.perm t₁ ≡ Tc.perm t₂
                  → Tc.contractName t₁ ≡ Tc.contractName t₂
                  → t₁ ≡ t₂
eqContractTypes {t₁} {t₂} refl refl = refl

data Field : Set where

data State : Set where
  state : Id -> List Field -> State

data Targ : Set where
  arg-trans : Tc -> Perm -> Targ
  base : Tbase -> Targ

IndirectRef : Set
IndirectRef = Id

ObjectRef : Set
ObjectRef = Id
  
data SimpleExpr : Set where
  var : Id -> SimpleExpr
  loc : IndirectRef -> SimpleExpr

record Object : Set where
  field
    contractName : Id
    stateName : Id
    contractFields : List SimpleExpr
    stateFields : List SimpleExpr

data Expr : Set where
  simpleExpr : SimpleExpr → Expr
  boolExpr : Bool → Expr
  voidExpr : Expr
  objRef : ObjectRef → Expr
  fieldAccess : Id → Expr  -- All field accesses are to 'this', so the field name suffices.
  assertₓ : Id → StateSet.⟨Set⟩ → Expr
  assertₗ : IndirectRef → StateSet.⟨Set⟩ → Expr
  -- TODO: add the rest of the expressions



record PublicTransaction : Set where
  constructor publicTransaction
  field
    retType : Type
    name : Id
    argType : Targ
    argName : Id
    initialState : Perm
    finalState : Perm
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
  boolFL : ∀ {b : Bool} → FreeLocations (boolExpr b) []
  varFL : ∀ {x : Id} → FreeLocations (simpleExpr (var x)) []
  voidFL : FreeLocations (voidExpr) []
  objRefFL : ∀ {o : ObjectRef} → FreeLocations (objRef o) []
  locFL : ∀ (l : IndirectRef) → FreeLocations (simpleExpr (loc l)) [ l ]


freeLocations : Expr → List IndirectRef
freeLocations (boolExpr b) = []
freeLocations (simpleExpr (var x)) = []
freeLocations (voidExpr) = []
freeLocations (objRef o) = []
freeLocations (simpleExpr (loc l)) = [ l ]
freeLocations (fieldAccess x) = []
freeLocations (assertₓ x x₁) = []
freeLocations (assertₗ l x₁) = [ l ]

data FreeVariables : Expr → List Id → Set where
  boolFL : ∀ {b : Bool} → FreeVariables (boolExpr b) []
  varFL : ∀ {x : Id} → FreeVariables (simpleExpr (var x)) [ x ]
  voidFL : FreeVariables (voidExpr) []
  objRefFL : ∀ {o : ObjectRef} → FreeVariables (objRef o) []
  locFL : ∀ (l : IndirectRef) → FreeVariables (simpleExpr (loc l)) []

data Closed : Expr → Set where
  closed : ∀ (e : Expr)
           → FreeVariables e []
           --------------------
           → Closed e

freeVariables : Expr → List IndirectRef
freeVariables (boolExpr b) = []
freeVariables (simpleExpr (var x)) = [ x ]
freeVariables (voidExpr) = []
freeVariables (objRef o) = []
freeVariables (simpleExpr (loc l)) = []
freeVariables (fieldAccess x) = []
freeVariables (assertₓ x x₁) = [ x ]
freeVariables (assertₗ l x₁) =  []


--=============== Static Semantics ================
-- A ContractEnv (written Γ) maps from Ids to contract definitions.
module ContractEnv = Context Contract

module TypeEnvContext = Context Type
TypeEnv = TypeEnvContext.ctx
open TypeEnvContext

record StaticEnv : Set where
  constructor se
  field
    varEnv : TypeEnv
    locEnv : TypeEnv
    objEnv : TypeEnv

_,ₓ_⦂_ : StaticEnv → Id → Type → StaticEnv
Δ ,ₓ x ⦂ T = record Δ {varEnv = (StaticEnv.varEnv Δ) , x ⦂ T}

_,ₗ_⦂_ : StaticEnv → Id → Type → StaticEnv
Δ ,ₗ l ⦂ T = record Δ {locEnv = (StaticEnv.locEnv Δ) , l ⦂ T}

_,ₒ_⦂_ : StaticEnv → Id → Type → StaticEnv
Δ ,ₒ o ⦂ T = record Δ {objEnv = (StaticEnv.objEnv Δ) , o ⦂ T}




-- Subtyping --
data _<:_ : Type → Type → Set where
  <:-refl : ∀ {T T' : Type}
         ----------------
         → T <: T
  -- TODO: add more subtyping judgments

-- Helper judgments --

--data _⊢_NotAsset : ContractEnv.ctx → Id → Set where
data NotAsset : ContractEnv.ctx → Id → Set where
  inContext :
    {Γ : ContractEnv.ctx}
    → {id : Id}
    → (contr : Contract)
    → (p : Contract.isAsset contr ≡ false)
    → (q : (Γ ContractEnv.∋ id ⦂ contr))
    -------------
    → NotAsset Γ id

-- Context strength --
data _<ₗ_ : TypeEnv → TypeEnv → Set where
  empty< : ∀ { Δ Δ' : TypeEnv}
        → (Δ' ≡ ∅)
        --------------
         → Δ <ₗ Δ'

  nonempty< : ∀ {Δ Δ' Δ'' Δ''' : TypeEnv}
    → ∀ {l : ℕ}
    → ∀ {T T' : Type}
    → Δ' ≡ (Δ'' , l ⦂ T')
    → Δ ≡ (Δ''' , l ⦂ T)
    → T <: T'
    → Δ''' <ₗ Δ''
    -------------
    → Δ <ₗ Δ'

data _<*_ : StaticEnv → StaticEnv → Set where
  * : ∀ {Δ Δ'}
        → (StaticEnv.varEnv Δ) ≡ (StaticEnv.varEnv Δ')
        → (StaticEnv.locEnv Δ) <ₗ (StaticEnv.locEnv Δ')
        → (StaticEnv.objEnv Δ) ≡ (StaticEnv.objEnv Δ')
        -----------------------------------------------
        → Δ <* Δ'

<ₗ-refl : ∀ {Δ : TypeEnv}
          → Δ <ₗ Δ
<ₗ-refl {Context.∅} = empty< refl
<ₗ-refl {Δ , x ⦂ x₁} = nonempty< refl refl <:-refl (<ₗ-refl {Δ})

<*-refl : ∀ {Δ : StaticEnv}
          → Δ <* Δ

<*-refl =
  let
    lt = <ₗ-refl
  in 
  * refl lt refl
  
-- Splitting --
record SplitType : Set where
  constructor _⇛_/_
  field
    t₁ : Type
    t₂ : Type
    t₃ : Type

infix 4 _⊢_
data _⊢_ : ContractEnv.ctx -> SplitType -> Set where
  voidSplit : ∀ {Γ : ContractEnv.ctx}
              ---------------
              → Γ ⊢ (base Void) ⇛ (base Void) / (base Void)

  booleanSplit : ∀ {Γ : ContractEnv.ctx}
                 --------------
                 → Γ ⊢ base Boolean ⇛ base Boolean / base Boolean

  -- split Unowned off of anything.
  unownedSplit : ∀ {Γ : ContractEnv.ctx}
                 → ∀ {t1 t2 t3 : Tc}
                 → (Tc.contractName t1) ≡ (Tc.contractName t2)
                 → Tc.contractName t1 ≡ Tc.contractName t3
                 → (Tc.perm t1) ≡ (Tc.perm t2)
                 → Tc.perm t3 ≡ Unowned
                 --------------
                 → Γ ⊢ contractType t1 ⇛ contractType t2 / contractType t3

  shared-shared-shared : ∀ {Γ : ContractEnv.ctx}
                         → ∀ {t : Tc}
                         → Tc.perm t ≡ Shared
                         --------------------------------------------------------
                         → Γ ⊢ contractType t  ⇛ contractType t / contractType t

  owned-shared :
   ∀ {c : Id}
   → ∀ {Γ : ContractEnv.ctx}
   → NotAsset Γ c
   --------------
    → Γ ⊢ contractType (tc c Owned)  ⇛ contractType (tc c Shared) / contractType (tc c Shared)

  states-shared :
    ∀ {s : σ}
    → ∀ {c : Id}
    → ∀ {Γ : ContractEnv.ctx}
    → NotAsset Γ c
    --------------
    → Γ ⊢ contractType ( record {perm = S s ; contractName = c} )  ⇛ contractType ( record {perm = Shared ; contractName = c} ) / contractType ( record {perm = Shared ; contractName = c} )

splitType : ∀ {Γ : ContractEnv.ctx}
          → ∀ {t1 t2 t3 : Type}
          → Γ ⊢ t1 ⇛ t2 / t3
          → SplitType

splitType voidSplit = (base Void) ⇛ (base Void) / (base Void)
splitType booleanSplit =  base Boolean ⇛ base Boolean / base Boolean
splitType (unownedSplit {Γ} {t1} {t2} {t3} eqNames1 eqNames2 eqPerms eqUnownedPerm) = contractType t1 ⇛ contractType t2 / contractType t3
splitType (shared-shared-shared {Γ} {t} _) =  contractType t  ⇛ contractType t / contractType t
splitType (owned-shared {c} x) = contractType ( record {perm = Owned ; contractName = c} )  ⇛ contractType ( record {perm = Shared ; contractName = c} ) / contractType ( record {perm = Shared ; contractName = c} )
splitType (states-shared {s} {c} x) = contractType ( record {perm = S s ; contractName = c} )  ⇛ contractType ( record {perm = Shared ; contractName = c} ) / contractType ( record {perm = Shared ; contractName = c} )

splitTypeCorrect :  ∀ {Γ}
                    → ∀ {t1 t2 t3 : Type}
                    → ∀ (p : Γ ⊢ t1 ⇛ t2 / t3)
                    → splitType p ≡ t1 ⇛ t2 / t3
splitTypeCorrect voidSplit = refl
splitTypeCorrect booleanSplit = refl
splitTypeCorrect (unownedSplit x _ _ _) = refl
splitTypeCorrect (shared-shared-shared _) = refl
splitTypeCorrect (owned-shared x) = refl
splitTypeCorrect (states-shared x) = refl



------------ Type judgments ----------------
data _⊢_⦂_⊣_ : StaticEnv → Expr → Type → StaticEnv → Set where
  varTy : ∀ {Γ : ContractEnv.ctx}
      → ∀ {Δ : StaticEnv}
      → ∀ {T₁ T₂ T₃ : Type}
      → ∀ (x : Id)
      → Γ ⊢ T₁ ⇛ T₂ / T₃
      -----------------------------------
      → (Δ ,ₓ x ⦂ T₁) ⊢ (simpleExpr (var x)) ⦂ T₂ ⊣ (Δ ,ₓ x ⦂ T₃)

  locTy :  ∀ {Γ : ContractEnv.ctx}
      → ∀ {Δ : StaticEnv}
      → ∀ {T₁ T₂ T₃ : Type}
      → ∀ (l : IndirectRef)
      → Γ ⊢ T₁ ⇛ T₂ / T₃
      ------------------------------------
      → (Δ ,ₗ l ⦂ T₁) ⊢ (simpleExpr (loc l)) ⦂ T₂ ⊣ (Δ ,ₗ l ⦂ T₃)

  objTy :  ∀ {Γ : ContractEnv.ctx}
      → ∀ {Δ : StaticEnv}
      → ∀ {T₁ T₂ T₃ : Type}
      → ∀ (o : ObjectRef)
      → Γ ⊢ T₁ ⇛ T₂ / T₃
      ------------------------------------
      → (Δ ,ₒ o ⦂ T₁) ⊢ (objRef o) ⦂ T₂ ⊣ (Δ ,ₒ o ⦂ T₃)

  boolTy : ∀ {Γ : ContractEnv.ctx}
         → ∀ {Δ : StaticEnv}
         → ∀ (b : Bool)
         ------------------------------------
         → Δ ⊢ (boolExpr b) ⦂ (base Boolean) ⊣ Δ

  voidTy : ∀ {Γ : ContractEnv.ctx}
         → ∀ {Δ : StaticEnv}
         --------------------
          → Δ ⊢ voidExpr ⦂ base Void ⊣ Δ


  assertTyₓ : ∀ {Γ : ContractEnv.ctx}
           → ∀ {Δ : StaticEnv}
           → ∀ {s₁ s₂ : StateSet.⟨Set⟩}
           → ∀ {tc : Tc}
           → ∀ {x : Id}
           → Tc.perm tc ≡ S s₁
           → s₁ ⊆ s₂
           --------------------------
           → (Δ ,ₓ x ⦂ (contractType tc)) ⊢ assertₓ x s₁ ⦂ base Void ⊣ (Δ ,ₓ x ⦂ (contractType tc))

  assertTyₗ : ∀ {Γ : ContractEnv.ctx}
           → ∀ {Δ : StaticEnv}
           → ∀ {s₁ s₂ : StateSet.⟨Set⟩}
           → ∀ {tc : Tc}
           → ∀ {l : IndirectRef}
           → Tc.perm tc ≡ S s₁
           → s₁ ⊆ s₂
           --------------------------
           → (Δ ,ₗ l ⦂ (contractType tc)) ⊢ assertₗ l s₁ ⦂ base Void ⊣ (Δ ,ₗ l ⦂ (contractType tc))

------------ DYNAMIC SEMANTICS --------------
data Value : Expr → Set where
  boolVal : ∀ (b : Bool)
          ------------
          → Value (boolExpr b)

  voidVal : Value (voidExpr)

  objVal : ∀ (o : ObjectRef)
           --------------
           → Value (objRef o)


-- μ
module ObjectRefContext = Context Object
ObjectRefEnv = ObjectRefContext.ctx

-- ρ
module IndirectRefContext = Context Expr -- TODO: require that these are all values
IndirectRefEnv = IndirectRefContext.ctx

-- φ
module StateLockingContext = Context Bool
StateLockingEnv = StateLockingContext.ctx

-- ψ
module ReentrancyContext = Context Bool
ReentrancyEnv = ReentrancyContext.ctx


record RuntimeEnv : Set where
  field
    μ : ObjectRefEnv
    ρ : IndirectRefEnv
    φ : StateLockingEnv
    ψ : ReentrancyEnv


----------- Reduction Rules ------------
data _,_⟶_,_ : RuntimeEnv → Expr → RuntimeEnv → Expr → Set where
  SElookup : -- of locations (i.e. let-bound variables)
    ∀ {Σ : RuntimeEnv}
    → ∀ {Δ Δ' : StaticEnv}
    → ∀ {T : Type}
    → ∀ {l : IndirectRef}
    → ∀ {v : Expr}
    → Δ ⊢ (simpleExpr (loc l)) ⦂ T ⊣ Δ'
    → (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just v)
    -----------------------------------------------------------
    → (Σ , (simpleExpr (loc l)) ⟶ Σ , v)

  SEassertₓ :
    ∀ {Σ : RuntimeEnv}
    → ∀ (x : Id)
    → ∀ (s : StateSet.⟨Set⟩)
    --------------
    → (Σ , assertₓ x s ⟶ Σ , voidExpr)

  SEassertₗ :
    ∀ {Σ : RuntimeEnv}
    → ∀ (l : IndirectRef)
    → ∀ (s : StateSet.⟨Set⟩)
    --------------
    → (Σ , assertₗ l s ⟶ Σ , voidExpr)

--============= Consistency ==============
-- Relates typing environments and object references to lists of all types of possible references.
-- For now, this is ordered; perhaps that is too strong and I should use <Set> instead.

data _⟷_ : Type → Type → Set where
  symCompat : ∀ {T₁ T₂ : Type}
              → T₁ ⟷ T₂
              ---------
              → T₂ ⟷ T₁
              
  unownedCompat :  ∀ {C C' : Id}
                  → ∀ {perm perm' : Perm}
                  → perm ≡ Unowned
                  → C ≡ C'
                  --------------------------------------------------------------
                  → contractType (tc C perm) ⟷ contractType (tc C' perm')

  sharedCompat : ∀ {t t' : Tc}
                 → Tc.perm t ≡ Shared
                 → Tc.perm t' ≡ Shared
                 → Tc.contractName t ≡ Tc.contractName t'
                 ----------------
                 → contractType t ⟷ contractType t'

  voidCompat : ----------------------
               base Void ⟷ base Void

  booleanCompat : ----------------------
                  base Boolean ⟷ base Boolean
  

----------- PROPERTIES OF SPLITTING -----------
-- The results of splitting are always compatible.
splitCompatibility : ∀ {Γ}
                     → ∀ {t₁ t₂ t₃ : Type}
                     → Γ ⊢ t₁ ⇛ t₂ / t₃
                     → t₂ ⟷ t₃
splitCompatibility voidSplit = voidCompat
splitCompatibility booleanSplit = booleanCompat
splitCompatibility (unownedSplit {Γ} {t₁} {t₂} {t₃} eqNames1 eqNames2 eqPerms t3IsUnowned) = symCompat (unownedCompat t3IsUnowned (Eq.trans (Eq.sym eqNames2) eqNames1))
splitCompatibility (shared-shared-shared {Γ} {t} eq)  = sharedCompat eq eq refl
splitCompatibility (owned-shared notAsset) = sharedCompat refl refl refl
splitCompatibility (states-shared x) = sharedCompat refl refl refl

compatibleContractsHaveSameNames : ∀ {T : Type}
                                   → ∀ {t : Tc}
                                   → ∀ {t' : Tc}
                                   → contractType t ⟷ T
                                   → T ≡ contractType t'
                                   → Tc.contractName t ≡ Tc.contractName t'
compatibleContractsHaveSameNames {T} {t} {t'} (symCompat compat) refl = sym (compatibleContractsHaveSameNames compat refl)
compatibleContractsHaveSameNames {T} {t} {t'} (unownedCompat x x₁) refl = x₁
compatibleContractsHaveSameNames {T} {t} {t'} (sharedCompat x x₁ x₂) refl = x₂          

typesCompatibleWithContractsAreContracts : ∀ {T : Type}
                                           → ∀ {t : Tc}
                                           → contractType t ⟷ T
                                           → ∃[ t' ] (T ≡ contractType t')

typesCompatibleWithContractsAreContracts {T} {t} (symCompat (symCompat compat)) = typesCompatibleWithContractsAreContracts compat
typesCompatibleWithContractsAreContracts {(contractType (tc _ _))} {(tc _ _)} (symCompat (unownedCompat {C} {C'} {perm} {perm'} x x₁)) = ⟨ tc C perm , refl ⟩
typesCompatibleWithContractsAreContracts {(contractType (tc _ _))} {(tc _ _)} (symCompat (sharedCompat {t} {t'} x x₁ x₂)) = ⟨ t , refl ⟩
typesCompatibleWithContractsAreContracts {T} {t} (unownedCompat {C} {C'} {perm} {perm'} refl refl) = ⟨ tc C perm' , refl ⟩
typesCompatibleWithContractsAreContracts {T} {t} (sharedCompat {tc C Shared} {tc C' Shared} refl refl refl) = ⟨ tc C Shared , refl ⟩ 

splittingRespectsHeap : ∀ {Γ}
                        → ∀ {T t₁ t₂ t₃ : Type}
                        → Γ ⊢ t₁ ⇛ t₂ / t₃
                        → T ⟷ t₁
                        → (T ⟷ t₂) × (T ⟷ t₃)

{-
splittingRespectsHeap {Γ} {base Void} {t₁} {t₂} {t₃} voidSplit consis = ⟨ consis , consis ⟩
splittingRespectsHeap {Γ} {base Boolean} {t₁} {t₂} {t₃} spl consis = {!!}
splittingRespectsHeap {Γ} {contractType x} {t₁} {t₂} {t₃} spl consis = {!!}
-}

splittingRespectsHeap {Γ} {T} {(base Void)} {(base Void)} {(base Void)} voidSplit consis = ⟨ consis , consis ⟩
splittingRespectsHeap {Γ} {T} {.(base Boolean)} {.(base Boolean)} {.(base Boolean)} booleanSplit consis = ⟨ consis , consis ⟩

-- t₁ => t₁ / t₃ because t₃ is Unowned.
splittingRespectsHeap {Γ} {T} {contractType t₁} {contractType t₂} {contractType t₃} (unownedSplit {Γ} {t₁} {t₂} {t₃} x x₁ x₂ x₃) (symCompat consis) rewrite (eqContractTypes x₂ x) | (proj₂ (typesCompatibleWithContractsAreContracts {T} {t₂} consis))  =
  let
    TIsContractTypeEx = typesCompatibleWithContractsAreContracts {T} consis -- T is a contractType.
    TContractType = proj₁ TIsContractTypeEx
    TIsContractType = proj₂ TIsContractTypeEx
    compatTypes = (compatibleContractsHaveSameNames consis TIsContractType)
    C = Tc.contractName t₃
    C' = Tc.contractName TContractType
  in 
    ⟨ symCompat ( Eq.subst (λ a → contractType t₂ ⟷ a) TIsContractType consis) , symCompat (unownedCompat x₃ (Eq.trans (Eq.sym x₁) compatTypes)) ⟩

-- t1 => t1 / Unowned
splittingRespectsHeap {Γ} {(contractType (tc C perm))} {contractType (tc C' perm')} {contractType t₂} {contractType t₃} (unownedSplit x x₁ x₂ x₃) (unownedCompat x₄ x₅) =
  ⟨ unownedCompat x₄ (Eq.trans x₅ x), unownedCompat x₄ (Eq.trans x₅ x₁) ⟩
splittingRespectsHeap {Γ} {(contractType t)} {contractType t₁} {contractType t₂} {contractType t₃} (unownedSplit x x₁ x₂ x₃) (sharedCompat t₁Shared t₂Shared t₁t₂Names) rewrite x₂ =
  ⟨ sharedCompat t₁Shared t₂Shared (Eq.trans t₁t₂Names x) , symCompat (unownedCompat x₃ (Eq.trans (Eq.sym x₁) (Eq.sym t₁t₂Names))) ⟩

splittingRespectsHeap {Γ} {T} {.(contractType _)} {.(contractType _)} {.(contractType _)} (shared-shared-shared x) consis = ⟨ consis , consis ⟩

-- Owned => Shared / Shared. Requires that T be Unowned.
splittingRespectsHeap {Γ} {T} {contractType (tc _ Owned)} {contractType (tc _ Shared)} {contractType (tc _ Shared)} (owned-shared x) (symCompat (symCompat consis)) = splittingRespectsHeap (owned-shared x) consis
splittingRespectsHeap {Γ} {.(contractType (tc _ _))} {contractType (tc _ Owned)} {contractType (tc _ Shared)} {contractType (tc _ Shared)} (owned-shared x) (symCompat (unownedCompat () x₂))
splittingRespectsHeap {Γ} {.(contractType _)} {contractType (tc _ Owned)} {contractType (tc _ Shared)} {contractType (tc _ Shared)} (owned-shared x) (symCompat (sharedCompat () x₂ x₃))
splittingRespectsHeap {Γ} {.(contractType (tc _ _))} {.(contractType (tc _ Owned))} {.(contractType (tc _ Shared))} {.(contractType (tc _ Shared))} (owned-shared x) (unownedCompat x₁ x₂) = ⟨ unownedCompat x₁ x₂ , unownedCompat x₁ x₂ ⟩
splittingRespectsHeap {Γ} {.(contractType _)} {.(contractType (tc _ Owned))} {.(contractType (tc _ Shared))} {.(contractType (tc _ Shared))} (owned-shared x) (sharedCompat x₁ () x₃)

-- States => Shared / Shared. Requires that T be Unowned.
splittingRespectsHeap {Γ} {T} {contractType (tc _ (S _))} {contractType (tc _ Shared)} {contractType (tc _ Shared)} (states-shared x) (symCompat (symCompat consis)) = splittingRespectsHeap (states-shared x) consis
splittingRespectsHeap {Γ} {.(contractType (tc _ _))} {contractType (tc _ (S _))} {contractType (tc _ Shared)} {contractType (tc _ Shared)} (states-shared x) (symCompat (unownedCompat () x₃))
splittingRespectsHeap {Γ} {.(contractType _)} {contractType (tc _ (S _))} {contractType (tc _ Shared)} {contractType (tc _ Shared)} (states-shared x) (symCompat (sharedCompat () x₃ x₄))
splittingRespectsHeap {Γ} {.(contractType (tc _ _))} {contractType (tc _ (S _))} {contractType (tc _ Shared)} {contractType (tc _ Shared)} (states-shared x) (unownedCompat x₁ x₂) =  ⟨ unownedCompat x₁ x₂ , unownedCompat x₁ x₂ ⟩
splittingRespectsHeap {Γ} {.(contractType _)} {contractType (tc _ (S _))} {contractType (tc _ Shared)} {contractType (tc _ Shared)} (states-shared x) (sharedCompat x₂ () x₄)


record RefTypes : Set where
  constructor rt
  field
    ctxTypes : List Type -- Corresponds to types from Δ
    envTypes : List Type -- Corresponds to types from ρ
    fieldTypes : List Type -- Corresponds to types from fields inside μ

data IsConnectedTypeList : List Type → Set where
  emptyTypeList : ∀ {D}
                  → D ≡ []
                  ----------
                  → IsConnectedTypeList D

  consTypeList : ∀ {T D}
                 → All (λ T' → (T ⟷ T')) D
                 → IsConnectedTypeList D
                 ------------------------
                 → IsConnectedTypeList (T ∷ D)

data IsConnectedEnvAndField : RefTypes → Set where
  emptyEnvTypes : ∀ {R}
                  → RefTypes.envTypes R ≡ []
                  → IsConnectedTypeList (RefTypes.fieldTypes R)
                  ----------------------------------------------
                  → IsConnectedEnvAndField R

  consEnvTypes : ∀ {R D T}
                 → RefTypes.envTypes R ≡ T ∷ D
                 → All (λ T' → T ⟷ T') D
                 → All (λ T' → T ⟷ T') (RefTypes.fieldTypes R)
                 → IsConnectedTypeList (RefTypes.fieldTypes R)
                 ----------------------------------------------
                 → IsConnectedEnvAndField R

data IsConnected : RefTypes → Set where              
  emptyCtxTypes : ∀ {R}
                  → RefTypes.ctxTypes R ≡ []
                  → IsConnectedEnvAndField R
                  ----------------------------
                  → IsConnected R

  nonEmptyCtxTypes : ∀ {R D T T'}
                   → RefTypes.ctxTypes R ≡ (T ∷ D)
                   → All (λ T' → T ⟷ T') D
                   → All (λ T' → T ⟷ T') (RefTypes.envTypes R)
                   → All (λ T' → T ⟷ T') (RefTypes.fieldTypes R)
                   → IsConnected (record {ctxTypes = D ; envTypes = RefTypes.envTypes R ; fieldTypes = RefTypes.fieldTypes R})
                   ----------------------------------------------
                   → IsConnected R
                       

objTypes : TypeEnv → ObjectRef → List Type
objTypes ∅ _ = []
objTypes (Δ , o' ⦂ T) o with o ≟ o'
...                    | yes eq = T ∷ (objTypes Δ o) 
...                    | no nEq = objTypes Δ o

envTypesHelper : IndirectRefEnv → TypeEnv → ObjectRef → List Type
envTypesHelper IndirectRefContext.∅  Δ o = []
envTypesHelper (IndirectRefContext._,_⦂_ ρ l (objRef o')) Δ o with (o' ≟ o) | (TypeEnvContext.lookup Δ l)
...                                             | yes _ | just T =  (T ∷ (envTypesHelper ρ Δ o))
...                                             | _ | _ = envTypesHelper ρ Δ o
envTypesHelper (IndirectRefContext._,_⦂_ ρ l v) Δ o = envTypesHelper ρ Δ o

envTypes : RuntimeEnv → StaticEnv → ObjectRef → List Type
envTypes Σ Δ o = envTypesHelper (RuntimeEnv.ρ Σ) (StaticEnv.locEnv Δ) o 

refFieldTypesHelper : ObjectRefEnv → StaticEnv → ObjectRef → List Type
refFieldTypesHelper ObjectRefContext.∅ Δ o = []
refFieldTypesHelper (ObjectRefContext._,_⦂_ μ o' obj) Δ o = refFieldTypesHelper μ Δ o  -- TODO; this is bogus!

refFieldTypes : RuntimeEnv → StaticEnv → ObjectRef → List Type
refFieldTypes Σ Δ o = refFieldTypesHelper (RuntimeEnv.μ Σ) Δ o

refTypes : RuntimeEnv → StaticEnv → ObjectRef → RefTypes
refTypes Σ Δ o = record {ctxTypes = (objTypes (StaticEnv.objEnv Δ) o) ; envTypes = (envTypes Σ Δ o) ; fieldTypes = (refFieldTypes Σ Δ o)}

envTypesExtendingEnv : ∀ {Σ Δ} → ∀ {o : ObjectRef} → ∀ {l T}
                       → let
                         E = envTypes Σ Δ o
                         E' = envTypes Σ (Δ ,ₗ l ⦂ T) o
                         in
                         (E' ≡ E) ⊎ (E' ≡ T ∷ E)
envTypesExtendingEnv{Σ} {Δ} {l} {T} {o} with (TypeEnvContext.lookup (StaticEnv.locEnv Δ) l)
...                                           | just T' = inj₂ {!!}
...                                           | _ =  inj₁ {!refl!}

refTypesExtendingEnv : ∀ {Σ Δ l T o}
                       → let
                         R = refTypes Σ Δ o
                         R' = refTypes Σ (Δ ,ₗ l ⦂ T) o
                         in
                           (R' ≡ R) ⊎ (R' ≡ rt (RefTypes.ctxTypes R) (T ∷ (RefTypes.envTypes R)) (RefTypes.fieldTypes R))
refTypesExtendingEnv {Σ} {Δ} {l} {T} {o} with (TypeEnvContext.lookup (StaticEnv.locEnv Δ) l)
...                                           | just T' = inj₂ {!!}
...                                           | _ =  inj₁ {!refl!}

{-
compatibleStaticExtensionsOK : ∀ {Σ Δ o l T}
                               → IsConnected (refTypes Σ Δ o)
                               → (∀ T' → ∀ l' → (StaticEnv.locEnv Δ) ∋ l' ⦂ T' → T' ⟷ T)
                               → IsConnected (refTypes Σ (Δ ,ₗ l ⦂ T) o)
compatibleStaticExtensionsOK (emptyCtxTypes eq (emptyEnvTypes eq' conn)) compat = {!!}
compatibleStaticExtensionsOK (emptyCtxTypes eq (consEnvTypes x x₁ x₂ x₃)) compat = {!!}
compatibleStaticExtensionsOK (nonEmptyCtxTypes x x₁ x₂ x₃ conn) compat = {!!}

compatibleDynamicExtensionsOK : ∀ {Σ Δ o o' T}
                                → IsConnected (refTypes Σ Δ o)
                                → (∀ T' → ∀ o'' → (StaticEnv.objEnv Δ) ∋ o'' ⦂ T' → T' ⟷ T)
                                → IsConnected (refTypes Σ (Δ ,ₒ o' ⦂ T) o)

compatibleDynamicExtensionsOK conn compat = {!!}
-}

splitReplacementEnvFieldOK : ∀ {Γ Σ Δ o l T₁ T₂ T₃}
                             → IsConnectedEnvAndField (refTypes Σ (Δ ,ₗ l ⦂ T₁) o)
                             → Γ ⊢ T₁ ⇛ T₂ / T₃
                             → IsConnectedEnvAndField (refTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₃) o)
splitReplacementEnvFieldOK = {!!}

-- The idea here is that the expression in question is of type T₂, so the reference left in the heap is of type T₃.
splitReplacementOK : ∀ {Γ Σ Δ o l T₁ T₂ T₃}
                     → IsConnected (refTypes Σ (Δ ,ₗ l ⦂ T₁) o)
                     → Γ ⊢ T₁ ⇛ T₂ / T₃
                     → IsConnected (refTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₃) o)
splitReplacementOK {Γ} {Σ} {Δ} {o} {l} {T₁} {T₂} {T₃} rConnected@(emptyCtxTypes {R} eq envFieldConnected) spl =
  let
    R' = (refTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₃) o) -- objTypes (StaticEnv.objEnv (Δ ,ₗ l ⦂ T₃) Context., o ⦂ T₃) o
  in
  -- The new obj context includes o, so it is no longer empty.
  nonEmptyCtxTypes {R'}
    {!refl!} 
    [] -- o is the only thing in the object context, so the rest of the list is empty and trivially is connected.
    {!!} -- show that o is connected to all the prior env types
    {!
      -- show that o is connected to all prior field types.
      -- Plan: all prior field types were compatible with T₁, so they must still be compatible with both T₁ and T₃.

!} 
    {!emptyCtxTypes refl envFieldConnected!} -- show that the rest of the context is all connected
splitReplacementOK (nonEmptyCtxTypes eq withinContext withEnv withField restConnected) spl = nonEmptyCtxTypes {!!} withinContext {!!} withField {!!}

data ReferenceConsistency : RuntimeEnv → StaticEnv → ObjectRef → Set where
  referencesConsistent : ∀ {Σ : RuntimeEnv}
                       → ∀ {Δ : StaticEnv}
                       → ∀ {o : ObjectRef}
                       → IsConnected (refTypes Σ Δ o)
                       -- TODO: add subtype constraint: C <: (refTypes Σ Δ o)
                       ---------------------------
                       → ReferenceConsistency Σ Δ o

-- Inversion for reference consistency: connectivity
referencesConsistentImpliesConnectivity : ∀ {Σ Δ o}
                                          → ReferenceConsistency Σ Δ o
                                          → IsConnected (refTypes Σ Δ o)
                                          
referencesConsistentImpliesConnectivity (referencesConsistent ic) = ic

------------ Global Consistency -----------
-- I'm going to need the fact that if an expression typechecks, and I find a location in it, then the location can be looked
-- up in the runtime environment. But every location in the expression has to also be in the typing context, so I can state this
-- without talking about expressions at all.
data _&_ok : RuntimeEnv → StaticEnv → Set where
  ok : ∀ {Σ : RuntimeEnv}
       → ∀ (Δ : StaticEnv)
       → (∀ (l : IndirectRef) → ((StaticEnv.locEnv Δ) ∋ l ⦂ base Void → (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just voidExpr)))
       → (∀ (l : IndirectRef) → ((StaticEnv.locEnv Δ) ∋ l ⦂ base Boolean → ∃[ b ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just (boolExpr b))))
       → (∀ (l : IndirectRef)
         → ∀ (T : Tc)
         → (StaticEnv.locEnv Δ) ∋ l ⦂ (contractType T)         -- If a location is in Δ and has contract reference type...
         → ∃[ o ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just (objRef o)) -- then location can be looked up in Σ...
         )
       → (∀ o → o ObjectRefContext.∈dom (RuntimeEnv.μ Σ) → ReferenceConsistency Σ Δ o)
       -- TODO: add remaining antecedents
       ---------------------------
       → Σ & Δ ok

-- Inversion for global consistency: reference consistency
refConsistency : ∀ {Σ : RuntimeEnv}
                 → ∀ {Δ : StaticEnv}
                 → ∀ {o : ObjectRef}
                 → ∀ {l : IndirectRef}
                 → Σ & Δ ok
                 → (∀ o → o ObjectRefContext.∈dom RuntimeEnv.μ Σ → ReferenceConsistency Σ Δ o)
refConsistency (ok Δ _ _ _ rc) =  rc


-- Inversion for global consistency : location lookup for a particular location
-- If l is in Δ and Σ & Δ ok, then l can be found in Σ.ρ.
locLookup : ∀ {Σ : RuntimeEnv}
            → ∀ {Δ : StaticEnv}
            → ∀ {l : IndirectRef}
            → ∀ {T : Tc}
            → Σ & Δ ok
            → (StaticEnv.locEnv Δ) ∋ l ⦂ (contractType T)
            → ∃[ o ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just (objRef o))

locLookup (ok Δ _ _ lContainment rc) lInDelta@(Z {Δ'} {x} {contractType a}) = lContainment x a lInDelta
locLookup (ok Δ _ _ lContainment rc) lInDelta@(S {Δ'} {x} {y} {contractType a} {b} nEq xInRestOfDelta) = lContainment x a lInDelta

voidLookup : ∀ {Σ : RuntimeEnv}
            → ∀ {Δ : StaticEnv}
            → ∀ {l : IndirectRef}
            → Σ & Δ ok
            → (StaticEnv.locEnv Δ) ∋ l ⦂ base Void
            → IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just voidExpr
voidLookup (ok Δ voidContainment _ _ _) voidType@(Z {Δ'} {l} {a}) = voidContainment l voidType
voidLookup (ok Δ voidContainment _ _ _) voidType@(S {Δ'} {l} {y} {a} {b} nEq lInRestOfDelta) = voidContainment l voidType

boolLookup : ∀ {Σ : RuntimeEnv}
            → ∀ {Δ : StaticEnv}
            → ∀ {l : IndirectRef}
            → Σ & Δ ok
            → (StaticEnv.locEnv Δ) ∋ l ⦂ base Boolean
            → ∃[ b ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just (boolExpr b))
boolLookup (ok Δ _ boolContainment _ _) boolType@(Z {Δ'} {l} {a}) = boolContainment l boolType
boolLookup (ok Δ _ boolContainment _ _) boolType@(S {Δ'} {l} {y} {a} {b} nEq lInRestOfDelta) = boolContainment l boolType 

------------ Lemmas --------------
-- If an expression is well-typed in Δ, then all locations in the expression are in Δ.
locationsInExprAreInContext : ∀ {Δ Δ' e T fl}
                              → ∀ {l : IndirectRef}
                              → Δ ⊢ e ⦂ T ⊣ Δ'
                              → FreeLocations e fl
                              → l ∈ fl
                              ----------------
                              → ∃[ T' ] ((StaticEnv.locEnv Δ) ∋ l ⦂ T')

-- fl is empty, so l is in fl leads to a contradiction.
locationsInExprAreInContext (varTy x spl) varFL ()
-- l is related to e, so therefore we can point to where l is in Δ.
locationsInExprAreInContext (locTy {Δ = Δ''} {T₁ = T₁} l spl) (locFL l) (here refl) =  ⟨ T₁ , Z ⟩
locationsInExprAreInContext (locTy {Δ = Δ''} {T₁} l spl) (locFL l) (there ())
locationsInExprAreInContext (objTy o spl) objRefFL ()
locationsInExprAreInContext (boolTy b) boolFL ()
locationsInExprAreInContext ty voidFL ()


-- TODO: relax progress a little per statement of Theorem 5.1.
data Progress : Expr → Set where
  step : ∀ (Σ Σ' : RuntimeEnv)
         → ∀ (e e' : Expr)       
         → (Σ , e ⟶ Σ' , e')
         -------------
         → Progress e

  done : ∀ {e : Expr}
         → Value e
         ---------
         → Progress e
progress : ∀ {e T Δ Δ'}
           → ∀ (Σ : RuntimeEnv)
           → Closed e
           → Σ & Δ ok
           → Δ ⊢ e ⦂ T ⊣ Δ'
           ---------------
           → Progress e

progress Σ (closed (simpleExpr (var x)) ()) consis (varTy x split) -- Contradiction: var x has free variables, but we assumed e was closed.
-- TODO: Refactor these cases to avoid duplication!
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l voidSplit) = 
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = voidLookup consis lInDelta
  in
    step Σ Σ (simpleExpr (loc l)) voidExpr (SElookup ty heapLookupResult)
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l booleanSplit) = 
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = boolLookup consis lInDelta
  in
    step Σ Σ (simpleExpr (loc l)) (boolExpr (proj₁ heapLookupResult)) (SElookup ty (proj₂ heapLookupResult))
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l (unownedSplit _ _ _ _)) = 
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₂ heapLookupResult
    o = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) (objRef o) (SElookup ty heapLookupFound)
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l (shared-shared-shared _)) =
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₂ heapLookupResult
    o = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) (objRef o) (SElookup ty heapLookupFound)
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l (owned-shared _)) =
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₂ heapLookupResult
    o = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) (objRef o) (SElookup ty heapLookupFound)
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l (states-shared _)) =
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₂ heapLookupResult
    o = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) (objRef o) (SElookup ty heapLookupFound)

progress Σ cl consis (objTy o split) =  done (objVal o)
progress Σ cl consis (boolTy b) = done (boolVal b)
progress Σ cl consis (voidTy) = done (voidVal)
progress Σ cl consis (assertTyₗ {s₁ = s} {l = l} tcEq subset) = step Σ Σ (assertₗ l s) (voidExpr) (SEassertₗ {Σ} l s)
progress Σ cl consis (assertTyₓ {s₁ = s} {x = x} tcEq subset) = step Σ Σ (assertₓ x s) (voidExpr) (SEassertₓ {Σ} x s)

data Preservation : Expr → Set where
  pres : ∀ {e e' : Expr}
       → ∀ {T T' : Type}
       → ∀ {Δ Δ'' Δ''' : StaticEnv}
       → ∀ {Σ Σ' : RuntimeEnv }
       → Δ ⊢ e ⦂ T ⊣ Δ'' 
       → Σ & Δ ok        
       -- TODO: hdref(e)
       → Σ , e ⟶ Σ' , e'
       → ∀ Δ'
       → (Δ' ⊢ e' ⦂ T' ⊣ Δ''') 
       → (Σ' & Δ' ok)  -- 
       → (Δ''' <* Δ'')
       -----------------
       → Preservation e

preservation : ∀ {e e' : Expr}
               → ∀ {T : Type}
               → ∀ {Δ Δ'' : StaticEnv}
               → ∀ {Σ Σ' : RuntimeEnv}
               → Δ ⊢ e ⦂ T ⊣ Δ'' 
               → Σ & Δ ok   
               → Σ , e ⟶ Σ' , e'
               -----------------------
               → Preservation e

-- Proof proceeds by induction on the dynamic semantics.

preservation ty@(locTy {Γ} {Δ₀} l voidSplit) consis st@(SElookup {l = l} {v = voidExpr} _ lookupL) =
  let
    Δ = (Δ₀ ,ₗ l ⦂ base Void)
    Δ' = Δ
    e'TypingJudgment = voidTy {Γ} {Δ = Δ'}
  in 
    pres ty consis st Δ' e'TypingJudgment consis <*-refl
preservation ty@(locTy {Γ} {Δ₀} l booleanSplit) consis st@(SElookup {l = l} {v = boolExpr b} _ lookupL) = 
 let
    Δ = (Δ₀ ,ₗ l ⦂ base Boolean)
    Δ' = Δ
    e'TypingJudgment = boolTy {Γ} {Δ = Δ'} b
  in 
    pres {Δ = Δ} ty consis st Δ' e'TypingJudgment consis <*-refl
preservation ty@(locTy {Γ} {Δ₀} {T₁ = contractType t₁} {T₃ = contractType t₃} l spl@(unownedSplit _ _ _ _))
                       consis@(ok Δ voidLookup boolLookup objLookup refConsistencyFunc)
                       st@(SElookup {Σ} {l = l} {v = objRef o} _ lookupL) = 
  let
    e'TypingJudgment = objTy {Γ} {Δ = Δ''} o spl
   
    consis' = ok Δ' voidLookup' boolLookup' objLookup' refConsistency'
  in
    pres {Δ = Δ} {Δ'' = Δ''} ty consis st Δ' e'TypingJudgment consis' {!!}
  where
    splT = splitType spl
    Δ'' = Δ₀ ,ₗ l ⦂ (SplitType.t₃ splT)
    Δ' = Δ'' ,ₒ o ⦂ (SplitType.t₁ splT)
    --Δ = Δ₀ ,ₗ l ⦂ (SplitType.t₁ splT)
    -- Show that if you look up l in the new context, you get the same type as before.
    voidLookup' : (∀ (l' : IndirectRef)
                  → ((StaticEnv.locEnv Δ') ∋ l' ⦂ base Void
                  → (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l' ≡ just voidExpr)))
    voidLookup' l' l'InΔ' with l' Data.Nat.≟ l
    voidLookup' l' (Context.S l'NeqL l'VoidType) | yes eq = ⊥-elim (l'NeqL eq)
    voidLookup' l' l'InΔ' | no nEq = voidLookup l' (S nEq (irrelevantReductionsOK l'InΔ' nEq))

    boolLookup' : (∀ (l' : IndirectRef)
                  → ((StaticEnv.locEnv Δ') ∋ l' ⦂ base Boolean
                  → ∃[ b ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l' ≡ just (boolExpr b))))
    boolLookup' l' l'InΔ' with l' Data.Nat.≟ l
    boolLookup' l' (Context.S l'NeqL l'BoolType) | yes eq = ⊥-elim (l'NeqL eq)
    boolLookup' l' l'InΔ' | no nEq = boolLookup l' (S nEq (irrelevantReductionsOK l'InΔ' nEq))
    
    objLookup' : (∀ (l' : IndirectRef) → ∀ (T : Tc)
                  → ((StaticEnv.locEnv Δ') ∋ l' ⦂ (contractType T)
                  → ∃[ o ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l' ≡ just (objRef o))))
    objLookup' l' _ l'InΔ' with l' Data.Nat.≟ l
    objLookup' l' _ (Context.S l'NeqL l'ObjType) | yes eq = ⊥-elim (l'NeqL eq)
    objLookup' l' t l'InΔ'@(Z {a = contractType t₃}) | yes eq = objLookup l' t₁ Z
    objLookup' l' t l'InΔ' | no nEq = objLookup l' t (S nEq (irrelevantReductionsOK l'InΔ' nEq))

    -- Show that all location-based aliases from the previous environment are compatible with the new alias.
    refConsistency' = λ o → λ oInμ →
      let
        origRefConsistency = refConsistencyFunc o oInμ
        origConnected = referencesConsistentImpliesConnectivity {Σ} {Δ} origRefConsistency
      in 
        referencesConsistent (splitReplacementOK origConnected spl)


preservation ty consis st@(SEassertₓ x s) = pres ty consis st {!!} {!!} {!!} {!!}
preservation ty consis st@(SEassertₗ l s) = pres ty consis st {!!} {!!} {!!} {!!}
