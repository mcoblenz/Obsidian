module silica where

-- open import Data.AVL.Sets using (⟨Set⟩)

open import Agda.Builtin.Bool
open import Data.Bool
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

data Tst : Set where
  Owned : Tst
  Unowned : Tst
  Shared : Tst
  S : σ → Tst

record Tc : Set where
  constructor tc
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
        
  unownedSplit : ∀ {Γ : ContractEnv.ctx}
                 → ∀ {t1 t2 t3 : Tc}
                 → (Tc.contractName t1) ≡ (Tc.contractName t2)
                 → (Tc.tst t1) ≡ (Tc.tst t2)
                 → Tc.tst t3 ≡ Unowned
                 --------------
                 → Γ ⊢ contractType t1 ⇛ contractType t2 / contractType t3

  shared-shared-shared :
    ∀ {c : Id}
    → ∀ {Γ : ContractEnv.ctx}
    → Γ ⊢ contractType ( record {tst = Shared ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )

  owned-shared :
   ∀ {c : Id}
   → ∀ {Γ : ContractEnv.ctx}
   → NotAsset Γ c
   --------------
    → Γ ⊢ contractType ( record {tst = Owned ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )

  states-shared :
    ∀ {s : σ}
    → ∀ {c : Id}
    → ∀ {Γ : ContractEnv.ctx}
    → NotAsset Γ c
    --------------
    → Γ ⊢ contractType ( record {tst = S s ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )

splitType : ∀ {Γ : ContractEnv.ctx}
          → ∀ {t1 t2 t3 : Type}
          → Γ ⊢ t1 ⇛ t2 / t3
          → SplitType

splitType voidSplit = (base Void) ⇛ (base Void) / (base Void)
splitType booleanSplit =  base Boolean ⇛ base Boolean / base Boolean
splitType (unownedSplit {Γ} {t1} {t2} {t3} x x₁ x₂) = contractType t1 ⇛ contractType t2 / contractType t3
splitType (shared-shared-shared {c}) =  contractType ( record {tst = Shared ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c})
splitType (owned-shared {c} x) = contractType ( record {tst = Owned ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )
splitType (states-shared {s} {c} x) = contractType ( record {tst = S s ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )

splitTypeCorrect :  ∀ {Γ}
                    → ∀ {t1 t2 t3 : Type}
                    → ∀ (p : Γ ⊢ t1 ⇛ t2 / t3)
                    → splitType p ≡ t1 ⇛ t2 / t3
splitTypeCorrect voidSplit = refl
splitTypeCorrect booleanSplit = refl
splitTypeCorrect (unownedSplit x x₁ x₂) = refl
splitTypeCorrect shared-shared-shared = refl
splitTypeCorrect (owned-shared x) = refl
splitTypeCorrect (states-shared x) = refl


----------- PROPERTIES OF SPLITTING -----------
-- The results of splitting are always compatible.
-- TODO

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
           → Tc.tst tc ≡ S s₁
           → s₁ ⊆ s₂
           --------------------------
           → (Δ ,ₓ x ⦂ (contractType tc)) ⊢ assertₓ x s₁ ⦂ base Void ⊣ (Δ ,ₓ x ⦂ (contractType tc))

  assertTyₗ : ∀ {Γ : ContractEnv.ctx}
           → ∀ {Δ : StaticEnv}
           → ∀ {s₁ s₂ : StateSet.⟨Set⟩}
           → ∀ {tc : Tc}
           → ∀ {l : IndirectRef}
           → Tc.tst tc ≡ S s₁
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

  unownedCompat : ∀ {T : Type}
                  → ∀ (C : Id)
                  --------------------------------------------------------------
                  → (contractType (record {contractName = C ; tst = Unowned})) ⟷ T

  sharedCompat : ∀ (C C' : Id)
                 → C ≡ C'
                 ----------------
                 → (contractType (record {contractName = C ; tst = Shared})) ⟷ (contractType (record {contractName = C' ; tst = Shared}))

data IsConnected : List Type → Set where
  emptyConnected : ∀ {D}
                 → D ≡ []
                 -----------
                 → IsConnected D

  inductiveConnected : ∀ {T : Type}
                       → ∀ {D : List Type}
                       → All (λ T' → T ⟷ T') D
                       → IsConnected D
                       ---------------------
                       → IsConnected (T ∷ D)                    

contextTypes : TypeEnv → ObjectRef → List Type
contextTypes ∅ _ = []
contextTypes (Δ , o' ⦂ T) o = [] -- if o Data.Nat.≟ o' then T ∷ (contextTypes Δ o) else contextTypes Δ o

envTypesHelper : IndirectRefEnv → TypeEnv → ObjectRef → List Type
envTypesHelper IndirectRefContext.∅  Δ o = []
envTypesHelper (IndirectRefContext._,_⦂_ ρ l v) Δ o with (TypeEnvContext.lookup Δ l)
...                                               | just T =  if true {- v =̂ o -} then (T ∷ envTypesHelper ρ Δ o) else envTypesHelper ρ Δ o  
...                                               | Data.Maybe.nothing = envTypesHelper ρ Δ o

envTypes : RuntimeEnv → StaticEnv → ObjectRef → List Type
envTypes Σ Δ o = envTypesHelper (RuntimeEnv.ρ Σ) (StaticEnv.locEnv Δ) o

refFieldTypesHelper : ObjectRefEnv → StaticEnv → ObjectRef → List Type
refFieldTypesHelper ObjectRefContext.∅ Δ o = []
refFieldTypesHelper (ObjectRefContext._,_⦂_ μ o' obj) Δ o = refFieldTypesHelper μ Δ o  -- TODO; this is bogus!

refFieldTypes : RuntimeEnv → StaticEnv → ObjectRef → List Type
refFieldTypes Σ Δ o = refFieldTypesHelper (RuntimeEnv.μ Σ) Δ o

refTypes : RuntimeEnv → StaticEnv → ObjectRef → List Type
refTypes Σ Δ o = (contextTypes (StaticEnv.objEnv Δ) o) ++ (envTypes Σ Δ o) ++ (refFieldTypes Σ Δ o)

data ReferenceConsistency : RuntimeEnv → StaticEnv → Set where
  referencesConsistent : ∀ {Σ : RuntimeEnv}
                       → ∀ {Δ : StaticEnv}
                       → ∀ {o : ObjectRef}
                       → IsConnected (refTypes Σ Δ o)
                       -- TODO: add subtype constraint: C <: (refTypes Σ Δ o)
                       ---------------------------
                       → ReferenceConsistency Σ Δ

------------ Global Consistency -----------
-- I'm going to need the fact that if an expression typechecks, and I find a location in it, then the location can be looked
-- up in the runtime environment. But every location in the expression has to also be in the typing context, so I can state this
-- without talking about expressions at all.
data _&_ok : RuntimeEnv → StaticEnv → Set where
  ok : ∀ {Σ : RuntimeEnv}
       → ∀ (Δ : StaticEnv)
       → (∀ (l : IndirectRef) → ((StaticEnv.locEnv Δ) ∋ l ⦂ base Void → (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just voidExpr)))
       → (∀ (l : IndirectRef) → ((StaticEnv.locEnv Δ) ∋ l ⦂ base Boolean → ∃[ b ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just (boolExpr b))))
       → (∀ (l : IndirectRef) → ∀ (T : Type)
         → (StaticEnv.locEnv Δ) ∋ l ⦂ T         -- If a location is in Δ...
         → ∃[ v ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just v) -- then location can be looked up in Σ...
         )
       → ReferenceConsistency Σ Δ                    -- and Σ and Δ have the ReferenceConsistency property...
       -- TODO: add remaining antecedents
       ---------------------------
       → Σ & Δ ok

-- Inversion for global consistency: reference consistency
refConsistency : ∀ {Σ : RuntimeEnv}
                 → ∀ {Δ : StaticEnv}
                 → ∀ {o : ObjectRef}
                 → ∀ {l : IndirectRef}
                 → Σ & Δ ok
                 → ReferenceConsistency Σ Δ
refConsistency (ok Δ _ _ _ rc) =  rc


-- Inversion for global consistency : location lookup for a particular location
-- If l is in Δ and Σ & Δ ok, then l can be found in Σ.ρ.
locLookup : ∀ {Σ : RuntimeEnv}
            → ∀ {Δ : StaticEnv}
            → ∀ {l : IndirectRef}
            → ∀ {T : Type}
            → Σ & Δ ok
            → (StaticEnv.locEnv Δ) ∋ l ⦂ T
            → ∃[ v ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just v)

locLookup (ok Δ _ _ lContainment rc) lInDelta@(Z {Δ'} {x} {a}) = lContainment x a lInDelta
locLookup (ok Δ _ _ lContainment rc) lInDelta@(S {Δ'} {x} {y} {a} {b} nEq xInRestOfDelta) = lContainment x a lInDelta

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
locationsInExprAreInContext (locTy {Δ = Δ''} {T₁ = T₁} l spl) (locFL l) (here refl) =  ⟨ T₁ , obviousContainment (StaticEnv.locEnv Δ'') ⟩
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
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l split) =
  let
    fl = freeLocations (simpleExpr (loc l))
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₂ heapLookupResult
    v = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) v (SElookup ty heapLookupFound)
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

preservation ty@(locTy {Γ} {Δ} l voidSplit) consis st@(SElookup {l = l} {v = voidExpr} _ lookupL) =
  let
    ΔInitial = (Δ ,ₗ l ⦂ base Void)
    Δ' = ΔInitial
    -- Show that e' ≡ void so that we can show that e' : Void.
    e'IsVoid = voidLookup consis Z
    e'TypingJudgment = voidTy {Δ = Δ'}
  in 
    pres ty consis st Δ' e'TypingJudgment consis <*-refl
preservation ty@(locTy {Γ} {Δ} l booleanSplit) consis st@(SElookup {l = l} {v = boolExpr b} _ lookupL) = 
 let
    ΔInitial = (Δ ,ₗ l ⦂ base Boolean)
    Δ' = ΔInitial
    -- Show that e' ≡ void so that we can show that e' : Void.
    e'IsBool = boolLookup consis Z
    e'TypingJudgment = boolTy {Δ = Δ'}
  in 
    pres ty consis st Δ' (e'TypingJudgment b) consis <*-refl
preservation ty@(locTy {Γ} {Δ} l spl) consis st@(SElookup {l = l} {v = v} _ lookupL) = {!!}
preservation ty consis st@(SEassertₓ x s) = pres ty consis st {!!} {!!} {!!} {!!}
preservation ty consis st@(SEassertₗ l s) = pres ty consis st {!!} {!!} {!!} {!!}

{-
-- ty is the initial typing judgment, which we get to assume.
-- ty' is the proof of well-typedness

-- CASE: the typing judgment used was Loc, in which case the expression must be of the form (loc l).
        -- and the step must have been taken via SElookup, which is the only rule that applies for exprs of the form (loc l).
preservation  (Loc spl) consis (SElookup ty' r) = 
  -- Case-analyze on the type of the expression.
  {-
  with TSS
  ...   base Void | ?
  ...   base Boolean | ?
  ...   contractType tc | ?
  -- By global consistency, in particular we must have consistency for l.
-}
  let ty = Loc spl
      T₁ = initialSplitType spl
    
  in
  {! 
  !}
  where
    helper : Type → (Δ' ⊢ e' ⦂ T' ⊣ Δ''')
                 × (Σ' & Δ' ok o , l)
                 × (Δ''' <ₗ Δ'')
    helper T = ?
    helper _ = ?

-}
