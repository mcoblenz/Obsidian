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
open Eq using (_≡_; _≢_; refl; cong; sym)
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

ObjectRef : Set
ObjectRef = Id
  
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
  objRef : ObjectRef → Expr
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
  objRef : ∀ {o : ObjectRef} → FreeLocations (objRef o) []
  loc : ∀ {l : IndirectRef} → FreeLocations (loc l) [ l ]

--=============== Static Semantics ================
-- A ContractEnv (written Γ) maps from Ids to contract definitions.
module ContractEnv = Context Contract

module TypeEnvContext = Context Type
TypeEnv = TypeEnvContext.·ctx
open TypeEnvContext


-- Subtyping --
data _<:_ : Type → Type → Set where
  refl : ∀ {T T' : Type}
         ----------------
         → T <: T
  -- TODO: add more subtyping judgments

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

-- Context strength --
data _<ₗ_ : TypeEnv → TypeEnv → Set where
  empty : ∀ { Δ Δ' : TypeEnv}
        → (Δ' ≡ ∅)
        --------------
         → Δ <ₗ Δ'

  nonempty : ∀ {Δ Δ' Δ'' : TypeEnv}
    → ∀ {x : ℕ}
    → ∀ {T T' : Type}
    → Δ' ≡ (Δ'' ,, (x , T))
    → (x , T') ∈̇ Δ
    → T <: T'
    → Δ <ₗ Δ''
    -------------
    → Δ <ₗ Δ'
  

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

initialSplitType : ∀ {Γ : ContractEnv.·ctx}
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
data _⊢_⦂_⊣_ : TypeEnv → Expr → Type → TypeEnv → Set where
  Var : ∀ {Γ : ContractEnv.·ctx}
      → ∀ {Δ : TypeEnv}
      → ∀ {T₁ T₂ T₃ : Type}
      → ∀ {x : Id}
      → Γ ⊢ T₁ ⇛ T₂ / T₃
      ------------------------------------
      → (Δ ,, (x , T₁)) ⊢ (var x) ⦂ T₂ ⊣ (Δ ,, (x , T₃))

  Loc :  ∀ {Γ : ContractEnv.·ctx}
      → ∀ {Δ : TypeEnv}
      → ∀ {T₁ T₂ T₃ : Type}
      → ∀ {l : IndirectRef}
      → Γ ⊢ T₁ ⇛ T₂ / T₃
      ------------------------------------
      → (Δ ,, (l , T₁)) ⊢ (loc l) ⦂ T₂ ⊣ (Δ ,, (l , T₃))

  Obj :  ∀ {Γ : ContractEnv.·ctx}
      → ∀ {Δ : TypeEnv}
      → ∀ {T₁ T₂ T₃ : Type}
      → ∀ {o : ObjectRef}
      → Γ ⊢ T₁ ⇛ T₂ / T₃
      ------------------------------------
      → (Δ ,, (o , T₁)) ⊢ (objRef o) ⦂ T₂ ⊣ (Δ ,, (o , T₃))


------------ DYNAMIC SEMANTICS --------------
data Value : Expr → Set where
  bool : ∀ {b : Bool}
         ------------
         → Value (bool b)

  void : Value (void)

  obj : ∀ (o : ObjectRef)
        --------------
        → Value (objRef o)


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

----------- Reduction Rules ------------
data _,_⟶_,_ : RuntimeEnv → Expr → RuntimeEnv → Expr → Set where
  SElookup : -- of locations (i.e. let-bound variables)
    ∀ {Σ : RuntimeEnv}
    → ∀ {Δ Δ' : TypeEnv}
    → ∀ {T : Type}
    → ∀ {l : IndirectRef}
    → ∀ {v : Expr}
    → Δ ⊢ (loc l) ⦂ T ⊣ Δ'
    → (RuntimeEnv.ρ Σ l ≡ just v)
    → (Σ , (loc l) ⟶ Σ , v)

--============= Consistency ==============
-- Relates typing environments and object references to lists of all types of possible references.
-- For now, this is ordered; perhaps that is too strong and I should use <Set> instead.
data CtxTypes : TypeEnv → ObjectRef → List Type → Set where
  ctxTypesDeltaEmpty : ∀ {Δ : TypeEnv}
                      → ∀ (o : ObjectRef)
                      → ∀ (D : List Type)
                      → Δ ≡ ∅
                      → D ≡ []
                      -----------------
                      → CtxTypes Δ o D

  ctxTypesDeltaMatchNonEmpty : ∀ {Δ Δ' : TypeEnv}
                             → ∀ (o o' : ObjectRef)
                             → ∀ (D D' : List Type)
                             → ∀ {T : Type}
                             → Δ ≡ (Δ' ,, (o' , T))
                             → o' ≡ o
                             → CtxTypes Δ' o D'
                             → D ≡ T ∷ D'
                             --------------------
                             → CtxTypes Δ o D

  ctxTypesDeltaNoMatchNonEmpty : ∀ {Δ Δ' : TypeEnv}
                               → ∀ (o o' : ObjectRef)
                               → ∀ (D : List Type)
                               → ∀ {T : Type}
                               → Δ ≡ (Δ' ,, (o' , T))
                               → o' ≢ o
                               → CtxTypes Δ' o D
                               --------------------
                               → CtxTypes Δ o D

data EnvTypes : RuntimeEnv → ObjectRef → List Type → Set where
  envTypes : ∀ {Σ : RuntimeEnv}
           → ∀ (o : ObjectRef)
           → ∀ (D : List Type)
           -- TODO: add actual antecedents
           ---------------------
           → EnvTypes Σ o D


data RefFieldTypes : RuntimeEnv → ObjectRef → List Type → Set where
  refFieldTypes :  ∀ {Σ : RuntimeEnv}
                   → ∀ (o : ObjectRef)
                   → ∀ (D : List Type)
                   -- TODO: add actual antecedents
                   ---------------------
                   → RefFieldTypes Σ o D

-- Relates an object reference in a context to a list of types that may reference the object.
data RefTypes : RuntimeEnv → TypeEnv → ObjectRef → List Type → Set where
  refTypes : ∀ {Σ : RuntimeEnv}
             → ∀ {Δ : TypeEnv}
             → ∀ (o : ObjectRef)
             → ∀ (D D₁ D₂ D₃ : List Type)
             → CtxTypes Δ o D₁
             → EnvTypes Σ o D₂
             → RefFieldTypes Σ o D₃
             → D ≡ D₁ ++ D₂ ++ D₃
             -------------------
             → RefTypes Σ Δ o D


data ReferenceConsistency : RuntimeEnv → TypeEnv → ObjectRef → Set where
  referencesConsistent : ∀ {Σ : RuntimeEnv}
                       → ∀ {Δ : TypeEnv}
                       → ∀ (o : ObjectRef)
                       → ∀ (D : List Type)
                       → RefTypes Σ Δ o D
                       ---------------------------
                       → ReferenceConsistency Σ Δ o



------------ Global Consistency -----------
-- I feel kind of bad about the fact that l is an input here! Is that really necessary?
data _&_ok_,_ : RuntimeEnv → TypeEnv → ObjectRef → IndirectRef → Set where
  ok : ∀ {Σ : RuntimeEnv}
       → {Δ Δ' : TypeEnv}
         → ∀ {e : Expr}
         → ∀ {T : Type}
         → ∀ (o : ObjectRef)
         → ∀ (l : IndirectRef) -- If you give me any particular location...
         → Δ ⊢ e ⦂ T ⊣ Δ'
         → (l , T) ∈̇ Δ         -- and that location is in Δ...
         → ∃[ fl ] ((FreeLocations e fl) → (l ∈ fl)) -- and that location is in the free locations of e ...
         → ∃[ v ] (RuntimeEnv.ρ Σ l ≡ just v)        -- and that location can be looked up in Σ...
       → ReferenceConsistency Σ Δ o                  -- and Σ and Δ have the ReferenceConsistency property...
     

       -- TODO: add remaining antecedents
       ---------------------------
       → Σ & Δ ok o , l

-- Inversion for global consistency: reference consistency
refConsistency : ∀ {Σ : RuntimeEnv}
                 → ∀ {Δ : TypeEnv}
                 → ∀ {o : ObjectRef}
                 → ∀ {l : IndirectRef}
                 → Σ & Δ ok o , l
                 → ReferenceConsistency Σ Δ o
refConsistency (ok o l _ _ _ _ rc) =  rc

-- Inversion for global consistency : location lookup for a particular location
-- If an expression is well-typed in Δ and (Σ & Δ ok), then all locations in the expression are in the domain of the context.
locLookup : ∀ {Σ : RuntimeEnv}
            → ∀ {Δ : TypeEnv}
            → ∀ {o : ObjectRef}
            → ∀ {l : IndirectRef}
            → Σ & Δ ok o , l
            → ∃[ v ] (RuntimeEnv.ρ Σ l ≡ just v)
locLookup (ok l o et ltd fl re _) =  re
            



------------ Lemmas --------------
-- TODO: relax progress a little per statement of Theorem 5.1.
data Progress (e : Expr) : Set where
  step : ∀ {e'}
         → ∀ {Σ Σ' : RuntimeEnv}
         → ∀ {Δ Δ' : TypeEnv}
         → (Σ , e ⟶ Σ' , e')
         -------------
         → Progress e

  done :
       Value e
       ---------
       → Progress e

progress : ∀ {e T Δ Δ' Σ o l}
           → Σ & Δ ok o , l
           → Δ ⊢ e ⦂ T ⊣ Δ'
           ---------------
           → Progress e

progress consis (Var split) =  step {!!}
progress consis ty@(Loc {l = l} split) =  step ( SElookup ty {!locLookup consis!})
progress consis (Obj split) =  step {!!}

{-
preservation : ∀ {Δ Δ' Δ'' Δ''' : TypeEnv}
               → ∀ {e e' : Expr}
               → ∀ {Σ Σ' : RuntimeEnv}
               → ∀ {T T' : Type}
               → ∀ {o : ObjectRef}
               → ∀ {l : IndirectRef} -- Ugh, do I really have to refer to l here?            
               → Δ ⊢ e ⦂ T ⊣ Δ'
               → Σ & Δ ok o , l
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
