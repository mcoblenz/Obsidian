-- {-# OPTIONS --allow-unsolved-metas #-}
{-# OPTIONS --show-implicit #-}

module HeapProperties where

  open import Silica

  import Relation.Binary.PropositionalEquality as Eq
  import Context


  import Data.AVL.Sets
  open import Data.List.All
  open import Data.Sum
  open import Data.Maybe

  open import Data.List.Membership.DecSetoid ≡-decSetoid 
  open import Data.List.Relation.Unary.Any

  open import Data.Empty

  open TypeEnvContext

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

    nonEmptyCtxTypes : ∀ {R D T}
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
  ...                    | yes eq = [ T ]
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

  objTypesExtension : ∀ {Δ o T}
                      → objTypes (Δ , o ⦂ T) o ≡ [ T ]
  objTypesExtension {Δ} {o} {T} with o ≟ o
  ... | yes oEq = refl
  ... | no oNeq = ⊥-elim (oNeq refl)

-- Need to prove this by induction over ρ!
  envTypesWithEmptyListGivesEmpty : ∀ {Δ M Φ Ψ o}
                                    → envTypes (re M IndirectRefContext.∅ Φ Ψ) Δ o ≡ []
  envTypesWithEmptyListGivesEmpty = refl

  envTypesWithOneItemMismatch : ∀ {M Φ Ψ Δ l o o'}
                              → ¬ (o ≡ o')
                              → envTypes (re M (IndirectRefContext.∅ Context., l ⦂ objRef o') Φ Ψ) Δ o ≡ []
                              
  envTypesWithOneItemMismatch {M} {Φ} {Ψ} {Δ} {l} {o} {o'} nEq with (o' ≟ o)
  envTypesWithOneItemMismatch {M} {Φ} {Ψ} {Δ} {l} {o} {o'} nEq | yes p = ⊥-elim (nEq (Eq.sym p))
  envTypesWithOneItemMismatch {M} {Φ} {Ψ} {Δ} {l} {o} {o'} nEq | no ¬p = refl



  envTypesExtensionExtendingΔ : ∀ {Σ Δ l T o R}
                              → envTypes Σ Δ o ≡ R
                              → (envTypes Σ ((Δ ,ₗ l ⦂ T) ,ₒ o ⦂ T) o ≡ R) ⊎  (envTypes Σ ((Δ ,ₗ l ⦂ T) ,ₒ o ⦂ T) o ≡ T ∷ R)
  envTypesExtensionExtendingΔ {re _ IndirectRefContext.∅ _ _} {Δ} {l} {T} {o} {R} eq = inj₁ eq

{-
  envTypesExtensionExtendingΔ {re Μ (Ρ Context., l' ⦂ (objRef o')) Φ Ψ} {Δ} {l} {T} {o} {R} eq with l ≟ l'
  ... | yes lEq =
                let
                  rest = envTypesExtensionExtendingΔ {re Μ Ρ Φ Ψ} {Δ} {l} {T} {o} {!!}
                in 
                  inj₂ {!!}
  ... | no lNeq = envTypesExtensionExtendingΔ eq
  envTypesExtensionExtendingΔ {re Μ (Ρ Context., l' ⦂ _) Φ Ψ} {Δ} {l} {T} {o} {R} eq = envTypesExtensionExtendingΔ {re Μ (Ρ Context., l' ⦂ _) Φ Ψ} eq 
 -}


  consListEq : ∀ {A : Set}
               → ∀ {R R' : List A }
               → ∀ {T : A}
               → R ≡ R'
               → _≡_ {_} {List A} (T ∷ R) (T ∷ R')
  consListEq {A} {[]} {[]} {T₁} refl = refl
  consListEq {A} {[]} {x ∷ R'} {T₁} ()
  consListEq {A} {x ∷ R} {x' ∷ R'} {T} refl = refl


  envTypesExtendingρ : ∀ {Μ Ρ Φ Ψ Δ l o o'} {R : List Type}
                                    → envTypes (re Μ Ρ Φ Ψ) Δ o ≡ R
                                    → (envTypes (re Μ (Ρ Context., l ⦂ (objRef o')) Φ Ψ) Δ o ≡ R) ⊎
                                                 (∃[ T ] (envTypes  (re Μ (Ρ Context., l ⦂ (objRef o')) Φ Ψ) Δ o ≡ T ∷ R))
  envTypesExtendingρ {M} {Ρ} {Φ} {Ψ} {Δ} {l} {o} {o'} {R} eqR with (o' ≟ o) 
  ... | no oNeq = inj₁ eqR
  ... | yes oEq with (TypeEnvContext.lookup (StaticEnv.locEnv Δ) l)
  envTypesExtendingρ {M} {Ρ} {Φ} {Ψ} {Δ} {l} {o} {o'} {R} eqR  | yes oEq | just T =
    let
      substFun : List Type → Set
      substFun a = _≡_ {_} {List Type} (T ∷ a) (T ∷ R)
      substResult = Eq.subst substFun (Eq.sym eqR) refl
    in
      inj₂ ⟨ T , substResult ⟩
  envTypesExtendingρ {M} {Ρ} {Φ} {Ψ} {Δ} {l} {o} {o'} {R} eqR | yes oEq | nothing = inj₁ eqR
 

  -- If we extend the location environment
  {-
  envTypesExtendingEnv : ∀ {Σ Δ} → ∀ {o : ObjectRef} → ∀ {l} → ∀ {T : Type}
                         → let
                           E = envTypes Σ Δ o
                           E' = envTypes Σ (Δ ,ₗ l ⦂ T) o
                           in
                           (E' ≡ E) ⊎ (E' ≡ T ∷ E)
  envTypesExtendingEnv {re _ Context.∅ _ _} {Δ} {o} {l} {T} = inj₁ refl
  envTypesExtendingEnv {re _ (qq Context., o' ⦂ T') _ _} {Δ} {o} {l} {T} with (o' ≟ o) | (TypeEnvContext.lookup (StaticEnv.locEnv Δ) l)
  ... | yes _ | just T'' = inj₂ {!!}
  ... | no _ | _ = envTypesExtendingEnv
  

  refFieldTypesExtendingEnv :  ∀ {Σ Δ} → ∀ {o : ObjectRef} → ∀ {l} → ∀ {T : Type}
                         → let
                           F = refFieldTypes Σ Δ o
                           F' = refFieldTypes Σ (Δ ,ₗ l ⦂ T) o
                          in
                            F ≡ F'
  refFieldTypesExtendingEnv = refl  

  refTypesExtendingEnv : ∀ {Σ Δ l T o}
                         → let
                           R = refTypes Σ Δ o
                           R' = refTypes Σ (Δ ,ₗ l ⦂ T) o
                           in
                             (R' ≡ R) ⊎ (R' ≡ rt (RefTypes.ctxTypes R) (T ∷ (RefTypes.envTypes R)) (RefTypes.fieldTypes R))
  refTypesExtendingEnv {Σ} {Δ} {l} {T} {o}  = {!envTypesExtendingEnv!}
-}

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

  -- Previously, all the types aliasing o were connected. Now, we've extended the context due to a split, and we need to show we still have connectivity.
  -- The idea here is that the expression in question is of type T₂, so the reference left in the heap is of type T₃.
  splitReplacementOK : ∀ {Γ Σ Δ o l T₁ T₂ T₃}
                       → IsConnected (refTypes Σ (Δ ,ₗ l ⦂ T₁) o)
                       → Γ ⊢ T₁ ⇛ T₂ / T₃
                       → IsConnected (refTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₃) o)
  splitReplacementOK {Γ} {Σ} {Δ} {o} {l} {T₁} {T₂} {T₃} rConnected@(emptyCtxTypes {R} eq envFieldConnected) spl =
    let
      Δ' = ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₃)
      R' = (refTypes Σ Δ' o) 
      ot = objTypes (StaticEnv.objEnv Δ) o
      otIsEmpty : ot ≡ []
      otIsEmpty = eq
      ot' = objTypes (StaticEnv.objEnv Δ') o
      otRelationship = ot' ≡ [ T₃ ]
      otRelationship = refl {_} {_} {_}


      
    in
    -- The new obj context includes o, so it is no longer empty.
     
    nonEmptyCtxTypes {R = R'} {D = []} {T = T₃} -- T
    
      (objTypesExtension {_} {_} {_}) -- 
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





