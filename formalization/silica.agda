module Silica where

  -- open import Data.AVL.Sets using (⟨Set⟩)

  open import Agda.Builtin.Bool public
  open import Data.Bool using (true; false) public
  open import Prelude public
  open import Data.Nat public
  open import Data.List public
  open import Data.Nat.Properties public
  open import Relation.Nullary using (¬_; Dec; yes; no) public
  open import Relation.Binary using (module StrictTotalOrder; DecSetoid; IsDecEquivalence) public
  open import Data.Maybe using (just) public
  import Relation.Binary.PropositionalEquality as Eq
  import Relation.Binary.Core
  import Relation.Binary.HeterogeneousEquality
  open Eq using (_≡_; _≢_; refl; cong; sym) public
  open Eq.≡-Reasoning public
  open import Data.Product using (_×_; proj₁; proj₂; ∃-syntax; ∃) renaming (_,_ to ⟨_,_⟩) public
  import Context
  open import Data.List.Membership.DecSetoid ≡-decSetoid
  import Data.List.Membership.DecSetoid
  import Data.List.Relation.Binary.Subset.Setoid using (_⊆_)
  import Data.List.Relation.Binary.Equality.DecPropositional using (_≡?_)
  open import Data.List.Relation.Unary.Any
  open import Data.List.All
  open import Data.Empty
  open import Data.Sum
  open import Level


  -------------- Syntax ------------
  Id : Set
  Id = ℕ

  -- State sets
  σ : Set
  σ = List ℕ

  -- List of ℕ subset
  _⊆_ : σ → σ → Set
  _⊆_ = Data.List.Relation.Binary.Subset.Setoid._⊆_ (DecSetoid.setoid ≡-decSetoid)

  -- List of ℕ decidable equality
  _≟l_ : Relation.Binary.Core.Decidable {A = σ} _≡_
  _≟l_ = Data.List.Relation.Binary.Equality.DecPropositional._≡?_ _≟_
  

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

  SInjective : (l₁ l₂ : σ)
               → l₁ ≢ l₂
               → S l₁ ≢ S l₂
  SInjective l₁ .l₁ lsEq refl = lsEq refl

  TCInjectiveContractName : ∀ {p₁ p₂}
                            → (cn₁ cn₂ : Id)
                            → cn₁ ≢ cn₂
                            → tc cn₁ p₁ ≢ tc cn₂ p₂
  TCInjectiveContractName cn₁ cn₂ nEq refl = nEq refl

  TCInjectivePermission : ∀ {cn₁ cn₂}
                            → (p₁ p₂ : Perm)
                            → p₁ ≢ p₂
                            → tc cn₁ p₁ ≢ tc cn₂ p₂
  TCInjectivePermission cn₁ cn₂ nEq refl = nEq refl     

  contractTypeInjective : ∀ {tc₁ tc₂}
    → tc₁ ≢ tc₂
    → contractType tc₁ ≢ contractType tc₂

  contractTypeInjective nEq refl = nEq refl

  -- Decidable equality for permissions
  infix 4 _≟p_
  _≟p_ : Relation.Binary.Core.Decidable {A = Perm} _≡_
  Owned ≟p Owned = yes refl
  Owned ≟p Unowned =  no (λ ())
  Owned ≟p Shared =  no (λ ())
  Owned ≟p S x =  no (λ ())
  Unowned ≟p Owned =  no (λ ())
  Unowned ≟p Unowned = yes refl
  Unowned ≟p Shared =  no (λ ())
  Unowned ≟p S x =  no (λ ())
  Shared ≟p Owned =  no (λ ())
  Shared ≟p Unowned =  no (λ ())
  Shared ≟p Shared = yes refl
  Shared ≟p S x =  no (λ ())
  S x ≟p Owned =  no (λ ())
  S x ≟p Unowned =  no (λ ())
  S x ≟p Shared =  no (λ ())
  S s1 ≟p S s2 with s1 ≟l s2
  ... | yes eq = yes (Eq.cong S eq)
  ... | no nEq = no λ sEq → SInjective s1 s2 nEq sEq 

  -- Decidable equality for types
  infix 4 _≟t_
  _≟t_ : Relation.Binary.Core.Decidable {A = Type} _≡_
  base Void ≟t base Void = yes refl
  base Void ≟t base Boolean = no (λ ())
  base Boolean ≟t base Void = no (λ ())
  base Boolean ≟t base Boolean = yes refl
  base x ≟t contractType x₁ = no (λ ())
  contractType x ≟t base x₁ = no (λ ())
  contractType (tc contractName p₁) ≟t contractType (tc contractName₁ p₂) with contractName ≟ contractName₁ | p₁ ≟p p₂
  ... | yes eqNames | yes eqPerms = yes (Eq.cong₂ (λ a → λ b → contractType (tc a b)) eqNames eqPerms)
  ... | no nEqNames | _ = no (contractTypeInjective (TCInjectiveContractName contractName contractName₁ nEqNames))
  ... | _ | no nEqPerms =  no (contractTypeInjective (TCInjectivePermission p₁ p₂ nEqPerms))

  ≟tIsEquivalence : Relation.Binary.IsEquivalence {A = Type} _≡_ -- _≟t_
  ≟tIsEquivalence = Eq.isEquivalence 

  ≡t-isDecEquivalence : IsDecEquivalence (_≡_ {A = Type})
  ≡t-isDecEquivalence = record
    { isEquivalence = ≟tIsEquivalence
    ; _≟_           = _≟t_
    }

  ≡t-decSetoid : DecSetoid 0ℓ 0ℓ
  ≡t-decSetoid = record
    { Carrier          = Type
    ; _≈_              = _≡_
    ; isDecEquivalence = ≡t-isDecEquivalence
    }


  module tDecSetoid = Data.List.Membership.DecSetoid ≡t-decSetoid
  _∈ₜ_ : Type → List Type → Set
  _∈ₜ_ = tDecSetoid._∈_

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
    assertₓ : Id → σ → Expr
    assertₗ : IndirectRef → σ → Expr
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
    <:-refl : ∀ {T : Type}
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
  <ₗ-refl {Δ , x ⦂ x₁} = nonempty< refl refl (<:-refl) (<ₗ-refl {Δ})

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
             → ∀ {s₁ s₂ : σ}
             → ∀ {tc : Tc}
             → ∀ {x : Id}
             → Tc.perm tc ≡ S s₁
             → s₁ ⊆ s₂
             --------------------------
             → (Δ ,ₓ x ⦂ (contractType tc)) ⊢ assertₓ x s₁ ⦂ base Void ⊣ (Δ ,ₓ x ⦂ (contractType tc))

    assertTyₗ : ∀ {Γ : ContractEnv.ctx}
             → ∀ {Δ : StaticEnv}
             → ∀ {s₁ s₂ : σ}
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
    constructor re
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
      → RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ v
      -----------------------------------------------------------
      → (Σ , (simpleExpr (loc l)) ⟶ Σ , v)

    SEassertₓ :
      ∀ {Σ : RuntimeEnv}
      → ∀ (x : Id)
      → ∀ (s : σ)
      --------------
      → (Σ , assertₓ x s ⟶ Σ , voidExpr)

    SEassertₗ :
      ∀ {Σ : RuntimeEnv}
      → ∀ (l : IndirectRef)
      → ∀ (s : σ)
      --------------
      → (Σ , assertₗ l s ⟶ Σ , voidExpr)


