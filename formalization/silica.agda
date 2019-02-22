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
  bool : Bool → Expr
  void : Expr
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
  boolFL : ∀ {b : Bool} → FreeLocations (bool b) []
  varFL : ∀ {x : Id} → FreeLocations (simpleExpr (var x)) []
  voidFL : FreeLocations (void) []
  objRefFL : ∀ {o : ObjectRef} → FreeLocations (objRef o) []
  locFL : ∀ (l : IndirectRef) → FreeLocations (simpleExpr (loc l)) [ l ]

freeLocationsExact : ∀ {l l'} → FreeLocations (simpleExpr (loc l)) [ l' ] → l ≡ l'
freeLocationsExact (locFL l)  =  refl


freeLocations : Expr → List IndirectRef
freeLocations (bool b) = []
freeLocations (simpleExpr (var x)) = []
freeLocations (void) = []
freeLocations (objRef o) = []
freeLocations (simpleExpr (loc l)) = [ l ]
freeLocations (fieldAccess x) = []
freeLocations (assertₓ x x₁) = []
freeLocations (assertₗ l x₁) = [ l ]

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
  refl : ∀ {T T' : Type}
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

  nonempty< : ∀ {Δ Δ' Δ'' : TypeEnv}
    → ∀ {x : ℕ}
    → ∀ {T T' : Type}
    → Δ' ≡ (Δ'' , x ⦂ T)
    → Δ ∋ x ⦂ T'
    → T <: T'
    → Δ <ₗ Δ''
    -------------
    → Δ <ₗ Δ'
  

-- Splitting --
data _⊢_⇛_/_ : ContractEnv.ctx -> Type -> Type -> Type -> Set where
  void : ∀ {Γ : ContractEnv.ctx}
        ---------------
        → Γ ⊢ (base Void) ⇛ (base Void) / (base Void)

  boolean : ∀ {Γ : ContractEnv.ctx}
        --------------
        → Γ ⊢ base Boolean ⇛ base Boolean / base Boolean
        
  unowned : ∀ {Γ : ContractEnv.ctx}
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
    → Γ ⊢  contractType ( record {tst = Owned ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )

  states-shared :
    ∀ {s : σ}
    → ∀ {c : Id}
    → ∀ {Γ : ContractEnv.ctx}
    → NotAsset Γ c
    --------------
    → Γ ⊢  contractType ( record {tst = S s ; contractName = c} )  ⇛ contractType ( record {tst = Shared ; contractName = c} ) / contractType ( record {tst = Shared ; contractName = c} )

initialSplitType : ∀ {Γ : ContractEnv.ctx}
                   → ∀ {t1 t2 t3 : Type}
                   → Γ ⊢ t1 ⇛ t2 / t3 → Type
initialSplitType (void) = base Void
initialSplitType boolean = base Boolean
initialSplitType (unowned {t1 = T} x x₁ x₂) =  contractType T
initialSplitType (shared-shared-shared {c = Contr}) = contractType ( record {tst = Shared ; contractName = Contr } )
initialSplitType (owned-shared {c = Contr} x) = contractType ( record {tst = Owned ; contractName = Contr })
initialSplitType (states-shared {s = sts} {c = Contr} x) = contractType ( record {tst = S sts ; contractName = Contr })

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
         → Δ ⊢ (bool b) ⦂ (base Boolean) ⊣ Δ

  voidTy : ∀ {Γ : ContractEnv.ctx}
         → ∀ {Δ : StaticEnv}
         --------------------
          → Δ ⊢ void ⦂ base Void ⊣ Δ


  assertTyₓ : ∀ {Γ : ContractEnv.ctx}
           → ∀ {Δ : StaticEnv}
           → ∀ {s₁ s₂ : StateSet.⟨Set⟩}
           → ∀ {tc : Tc}
           → ∀ {x : IndirectRef}
           → Tc.tst tc ≡ S s₁
           → s₁ ⊆ s₂
           --------------------------
           → (Δ ,ₓ x ⦂ (contractType tc)) ⊢ assertₓ x s₁ ⦂ base Void ⊣ (Δ ,ₓ x ⦂ (contractType tc))

  assertTyₗ : ∀ {Γ : ContractEnv.ctx}
           → ∀ {Δ : StaticEnv}
           → ∀ {s₁ s₂ : StateSet.⟨Set⟩}
           → ∀ {tc : Tc}
           → ∀ {l : Id}
           → Tc.tst tc ≡ S s₁
           → s₁ ⊆ s₂
           --------------------------
           → (Δ ,ₗ l ⦂ (contractType tc)) ⊢ assertₗ l s₁ ⦂ base Void ⊣ (Δ ,ₗ l ⦂ (contractType tc))

------------ DYNAMIC SEMANTICS --------------
data Value : Expr → Set where
  boolVal : ∀ (b : Bool)
          ------------
          → Value (bool b)

  voidVal : Value (void)

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
    → (Σ , (simpleExpr (loc l)) ⟶ Σ , v)

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
refConsistency (ok Δ _ rc) =  rc


-- Inversion for global consistency : location lookup for a particular location
-- If l is in Δ and Σ & Δ ok, then l can be found in Σ.ρ.
locLookup : ∀ {Σ : RuntimeEnv}
            → ∀ {Δ : StaticEnv}
            → ∀ {l : IndirectRef}
            → ∀ {T : Type}
            → Σ & Δ ok
            → (StaticEnv.locEnv Δ) ∋ l ⦂ T
            → ∃[ v ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l ≡ just v)

locLookup (ok Δ lContainment rc) lInDelta@(Z {Δ'} {x} {a}) = lContainment x a lInDelta
locLookup (ok Δ lContainment rc) lInDelta@(S {Δ'} {x} {y} {a} {b} nEq xInRestOfDelta) = lContainment x a lInDelta



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
  step : ∀ {e e' : Expr}
         → ∀ {Σ Σ' : RuntimeEnv}
         → (Σ , e ⟶ Σ' , e')
         -------------
         → Progress e

  done : ∀ {e : Expr}
         → Value e
         ---------
         → Progress e
         
progress : ∀ {e T Δ Δ' Σ}
           → Σ & Δ ok
           → Δ ⊢ e ⦂ T ⊣ Δ'
           ---------------
           → Progress e

progress consis (varTy x split) =  step {!!}
progress consis@(ok {Σ} _ _ _) ty@(locTy l split) =
  let
    fl = freeLocations (simpleExpr (loc l))
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₂ heapLookupResult
    v = proj₁ heapLookupResult
  in
    step {simpleExpr (loc l)} {v} {Σ} {Σ} (SElookup ty heapLookupFound)
progress consis (objTy o split) =  step {!!}
progress consis (boolTy b) = done (boolVal b)
progress consis (voidTy) = done (voidVal)
progress consis (assertTyₗ tcEq subset) = {!!}
progress consis (assertTyₓ tcEq subset) = {!!}

{-
preservation : ∀ {Δ Δ' Δ'' Δ''' : TypeEnv}
               → ∀ {e e' : Expr}
               → ∀ {Σ Σ' : RuntimeEnv}
               → ∀ {T T' : Type}
               → ∀ {o : ObjectRef}
               → ∀ {l : IndirectRef} -- Ugh, do I really have to refer to l here?            
               → Δ ⊢ e ⦂ T ⊣ Δ'
               → Σ & Δ ok
               -- TODO: hdref(e)
               → Σ , e ⟶ Σ' , e'
               -----------------------
               →  (Δ' ⊢ e' ⦂ T' ⊣ Δ''')
                 × (Σ' & Δ' ok o , l)
                 × (Δ''' <ₗ Δ'')

-- ty is the initial typing judgment, which we get to assume.
-- ty' is the proof of well-typedness

-- CASE: the typing judgment used was Loc, in which case the expression must be of the form (loc l).
        -- and the step must have been taken via SElookup, which is the only rule that applies for exprs of the form (loc l).
preservation  (Loc spl) consis (SElookup ty' r) = 
  -- Case-analyze on the type of the expression.
  {-
  with T
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
