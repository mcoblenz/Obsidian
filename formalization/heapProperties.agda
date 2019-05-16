{-# OPTIONS --allow-unsolved-metas #-}
{-# OPTIONS --show-implicit #-}

module HeapProperties where

  open import Silica

  import Relation.Binary.PropositionalEquality as Eq
  import Context

  open import Data.List.Relation.Unary.All
  import Relation.Unary
  import Data.List.Properties
  import Data.List
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


-- ============= DEFINITIONS OF HEAP CONSISTENCY ===============

 

 
                  
  ctxTypes : TypeEnv → ObjectRef → List Type
  ctxTypes ∅ _ = []
  ctxTypes (Δ , o' ⦂ T) o with o ≟ o'
  ...                    | yes eq = [ T ]
  ...                    | no nEq = ctxTypes Δ o

  envTypesHelper : IndirectRefEnv → TypeEnv → ObjectRef → List Type
  envTypesHelper IndirectRefContext.∅  Δ o = []


  envTypesHelper (IndirectRefContext._,_⦂_ ρ l (objRef o')) Δ o with (o' ≟ o) | (TypeEnvContext.lookup Δ l)
  ...                                             | yes _ | just T =  (T ∷ (envTypesHelper ρ Δ o))
  ...                                             | _ | _ = envTypesHelper ρ Δ o

  envTypesHelper (IndirectRefContext._,_⦂_ ρ l v) Δ o = envTypesHelper ρ Δ o

  makeEnvTypesList : RuntimeEnv → StaticEnv → ObjectRef → List Type
  makeEnvTypesList Σ Δ o = envTypesHelper (RuntimeEnv.ρ Σ) (StaticEnv.locEnv Δ) o

 
  -- The List IndirectRef is a list of locations that have already been used in previous calls (and are forbidden to be used again).
  data EnvTypes : RuntimeEnv → StaticEnv → ObjectRef → List IndirectRef → List Type → Set where
    envTypesConcatMatchFound : ∀ {R l T μ ρ φ ψ forbiddenRefs}
                               → (Δ : StaticEnv)
                               → (o : ObjectRef)
                               → EnvTypes (re μ ρ φ ψ) Δ o (l ∷ forbiddenRefs) R
                               → (StaticEnv.locEnv Δ) ∋ l ⦂ T
                               → l ∉ forbiddenRefs 
                               → EnvTypes (re μ (ρ IndirectRefContext., l ⦂ (objRef o)) φ ψ) Δ o forbiddenRefs (T ∷ R)

    envTypesConcatMatchNotFound : ∀ {R l μ ρ φ ψ forbiddenRefs}
                                  → (Δ : StaticEnv)
                                  → (o : ObjectRef)
                                  → EnvTypes (re μ ρ φ ψ) Δ o forbiddenRefs R
                                  → l ∌dom (StaticEnv.locEnv Δ)
                                  → EnvTypes (re μ (ρ IndirectRefContext., l ⦂ (objRef o)) φ ψ) Δ o forbiddenRefs R
 
    envTypesConcatMismatch : ∀ {R l μ ρ φ ψ forbiddenRefs}
                             → (Δ : StaticEnv)
                             → (o o' : ObjectRef)
                             → o ≢ o'
                             → EnvTypes (re μ ρ φ ψ) Δ o forbiddenRefs R -- Mismatch in ρ, so keep looking in the rest of ρ
                             → EnvTypes (re μ (ρ IndirectRefContext., l ⦂ (objRef o')) φ ψ) Δ o forbiddenRefs R

    envTypesEmpty : ∀ {μ φ ψ Δ o forbiddenRefs}
                    → EnvTypes  (re μ Context.∅ φ ψ) Δ o forbiddenRefs []

  record RefTypes (Σ : RuntimeEnv) (Δ : StaticEnv) (o : ObjectRef) : Set where
    field
      oTypesList : List Type -- Corresponds to types from the o's in Δ.
      oTypes : ctxTypes (StaticEnv.objEnv Δ) o ≡ oTypesList
      envTypesList : List Type
      envTypes : EnvTypes Σ Δ o [] envTypesList
      fieldTypesList : List Type -- Corresponds to types from fields inside μ

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

  data IsConnectedEnvAndField  (Σ : RuntimeEnv) (Δ : StaticEnv) (o : ObjectRef) : RefTypes Σ Δ o → Set where
    envTypesConnected : (R : RefTypes Σ Δ o)
                        → IsConnectedTypeList (RefTypes.envTypesList R)
                        → IsConnectedTypeList (RefTypes.fieldTypesList R)
                        → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList R)) (RefTypes.envTypesList R) -- all of the l types are connected to all of the field types
                        ----------------------------------------------
                        → IsConnectedEnvAndField Σ Δ o R  

  envFieldInversion1 : ∀ {Σ Δ o R}
                       → IsConnectedEnvAndField Σ Δ o R
                       → IsConnectedTypeList (RefTypes.envTypesList R)
  envFieldInversion1 (envTypesConnected R env f envField) = env      

  envFieldInversion2 :  ∀ {Σ Δ o R}
                        → IsConnectedEnvAndField Σ Δ o R
                        → IsConnectedTypeList (RefTypes.fieldTypesList R)
  envFieldInversion2 (envTypesConnected R env f envField) = f

  envFieldInversion3 :  ∀ {Σ Δ o R}
                        → IsConnectedEnvAndField Σ Δ o R
                        → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList R)) (RefTypes.envTypesList R)
  envFieldInversion3 (envTypesConnected R env f envField) = envField

  data IsConnected (Σ : RuntimeEnv) (Δ : StaticEnv) (o : ObjectRef) : RefTypes Σ Δ o → Set where              
    isConnected : (R : RefTypes Σ Δ o)
                  → IsConnectedTypeList (RefTypes.oTypesList R)
                  → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList R)) (RefTypes.oTypesList R) -- all of the o types are connected to all of the field types
                  → All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList R)) (RefTypes.oTypesList R) -- all of the o types are connected to all of the l types
                  → IsConnectedEnvAndField Σ Δ o R
                  ----------------------------------------------
                  → IsConnected Σ Δ o R




  refFieldTypesHelper : ObjectRefEnv → StaticEnv → ObjectRef → List Type
  refFieldTypesHelper ObjectRefContext.∅ Δ o = []
  refFieldTypesHelper (ObjectRefContext._,_⦂_ μ o' obj) Δ o = refFieldTypesHelper μ Δ o  -- TODO; this is bogus!

  refFieldTypes : RuntimeEnv → StaticEnv → ObjectRef → List Type
  refFieldTypes Σ Δ o = refFieldTypesHelper (RuntimeEnv.μ Σ) Δ o

--  refTypes : RuntimeEnv → StaticEnv → ObjectRef → RefTypes
--  refTypes Σ Δ o = record {oTypesList = (ctxTypes (StaticEnv.objEnv Δ) o) ; lTypes = (envTypes Σ Δ o) ; fieldTypesList = (refFieldTypes Σ Δ o)}



-- ================================ OVERALL HEAP CONSISTENCY ===========================

  data ReferenceConsistency : RuntimeEnv → StaticEnv → ObjectRef → Set where
    referencesConsistent : ∀ {Σ : RuntimeEnv}
                         → ∀ {Δ : StaticEnv}
                         → ∀ {o : ObjectRef}
                         → ∃[ RT ] (IsConnected Σ Δ o RT)
                           -- TODO: add subtype constraint: C <: (refTypes Σ Δ o)
                         ---------------------------
                         → ReferenceConsistency Σ Δ o

  -- Inversion for reference consistency: connectivity
  referencesConsistentImpliesConnectivity : ∀ {Σ Δ o}
                                            → ReferenceConsistency Σ Δ o
                                            → ∃[ RT ] (IsConnected Σ Δ o RT)

  referencesConsistentImpliesConnectivity (referencesConsistent ic) = {!!} -- ic

  ------------ Global Consistency -----------
  -- I'm going to need the fact that if an expression typechecks, and I find a location in it, then the location can be looked
  -- up in the runtime environment. But every location in the expression has to also be in the typing context, so I can state this
  -- without talking about expressions at all.
  data _&_ok : RuntimeEnv → StaticEnv → Set where
    ok : ∀ {Σ : RuntimeEnv}
         → ∀ (Δ : StaticEnv)
         → (∀ (l : IndirectRef) → ((StaticEnv.locEnv Δ) ∋ l ⦂ base Void → (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ voidExpr)))
         → (∀ (l : IndirectRef) → ((StaticEnv.locEnv Δ) ∋ l ⦂ base Boolean → ∃[ b ] (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ boolExpr b)))
         → (∀ (l : IndirectRef)
           → ∀ (T : Tc)
           → (StaticEnv.locEnv Δ) ∋ l ⦂ (contractType T)         -- If a location is in Δ and has contract reference type...
           → ∃[ o ] (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objRef o × (o ObjectRefContext.∈dom (RuntimeEnv.μ Σ))) -- then location can be looked up in Σ...
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
              → ∃[ o ] (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objRef o × (o ObjectRefContext.∈dom (RuntimeEnv.μ Σ)))

  locLookup (ok Δ _ _ lContainment rc) lInDelta@(Z {Δ'} {x} {contractType a}) = lContainment x a lInDelta
  locLookup (ok Δ _ _ lContainment rc) lInDelta@(S {Δ'} {x} {y} {contractType a} {b} nEq xInRestOfDelta) = lContainment x a lInDelta

  voidLookup : ∀ {Σ : RuntimeEnv}
              → ∀ {Δ : StaticEnv}
              → ∀ {l : IndirectRef}
              → Σ & Δ ok
              → (StaticEnv.locEnv Δ) ∋ l ⦂ base Void
              → (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ voidExpr)
  voidLookup (ok Δ voidContainment _ _ _) voidType@(Z {Δ'} {l} {a}) = voidContainment l voidType
  voidLookup (ok Δ voidContainment _ _ _) voidType@(S {Δ'} {l} {y} {a} {b} nEq lInRestOfDelta) = voidContainment l voidType

  boolLookup : ∀ {Σ : RuntimeEnv}
              → ∀ {Δ : StaticEnv}
              → ∀ {l : IndirectRef}
              → Σ & Δ ok
              → (StaticEnv.locEnv Δ) ∋ l ⦂ base Boolean
              → ∃[ b ] (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ boolExpr b)
  boolLookup (ok Δ _ boolContainment _ _) boolType@(Z {Δ'} {l} {a}) = boolContainment l boolType
  boolLookup (ok Δ _ boolContainment _ _) boolType@(S {Δ'} {l} {y} {a} {b} nEq lInRestOfDelta) = boolContainment l boolType 


  -- =================== LEMMAS RELATED TO HEAP CONSISTENCY =================

  -- If a location is in ρ, then there is a corresponding item in envTypesList.
  findLocationInEnvTypes : ∀ {Σ Δ l o T forbiddenRefs}
                           → (envTypesList : List Type)
                           → EnvTypes Σ (Δ ,ₗ l ⦂ T) o forbiddenRefs envTypesList
                           → IsConnectedTypeList envTypesList
                           → (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objRef o)                          
                           → T ∈ₜ envTypesList × All (λ T' → (T' ≢ T) → T ⟷ T') envTypesList
-- The argument will have the form: look, I found T in envTypes, so it must be compatible with everything in fieldTypes already.
  findLocationInEnvTypes {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {o} {T} .(T' ∷ R)
    (envTypesConcatMatchFound {R = R} {l = l₁} {T = T'} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = forbiddenRefs} .(Δ ,ₗ l ⦂ T) o envTypes l₁HasTypeT l₁NotForbidden)
    (consTypeList firstConnected restConnected)
    lInρ with l ≟ l₁
  ... | yes lEql₁ =
    -- We must have T' ≡ T because we looked up l in (Δ ,ₗ l ⦂ T).
    let
      TInEnvTypesList =  here (Eq.sym TEqContractTypet₁)
      TCompatWithRest = transformConnected R (Eq.subst (λ a → All (λ T' → a ⟷ T') R) TEqContractTypet₁ firstConnected)
    in
      ⟨ TInEnvTypesList ,  (λ TNeqT' → ⊥-elim (TNeqT' (TEqContractTypet₁))) ∷ TCompatWithRest ⟩ -- Eq.subst (λ a → All (λ a → T ⟷ a) (T' ∷ R)) TEqContractTypet₁ firstConnected ⟩
    where
      lInΔ' : ((StaticEnv.locEnv Δ) , l ⦂ T) ∋ l ⦂ T
      lInΔ' = Z
      
      TEqContractTypet₁ : T' ≡ T
      TEqContractTypet₁ = contextLookupUnique l₁HasTypeT (Eq.subst (λ a → (StaticEnv.locEnv Δ) TypeEnvContext., l ⦂ T ∋ a ⦂ T) lEql₁ lInΔ')

      transformConnected : ∀ L → All (λ T' → T ⟷ T') L → All (λ T' → (T' ≢ T) → T ⟷ T') L
      transformConnected L [] = []
      transformConnected (T' ∷ L) (h ∷ r) =  (λ tNeqT' → h) ∷ (transformConnected L r)
  ... | no lNeql₁ =
      -- envTypesList is T ∷ R, but T is not the one we were interested in. l ⦂ o is in ρ, but not at the end.
      -- Eventually we'll look up l in  (Δ ,ₗ l ⦂ T) and find T.
    let
      lInRestOfρ = IndirectRefContext.irrelevantReductionsOK lInρ lNeql₁
      recurse = findLocationInEnvTypes {re μ ρ φ ψ} {Δ} {l} {o} {T} R envTypes restConnected lInRestOfρ
      TInR = proj₁ recurse
      TCompatT' = Data.List.Relation.Unary.All.lookup firstConnected TInR
    in
      ⟨ there TInR , (λ TNeqT' → symCompat TCompatT') ∷ proj₂ recurse ⟩ -- firstConnected ⟩
  findLocationInEnvTypes {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {o} {T} envTypesList
    (envTypesConcatMatchNotFound {R = .envTypesList} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = forbiddenRefs} .(Δ ,ₗ l ⦂ T) o envTypes lNotInΔ')
    connected
    lInρ =
      let
        lNeql₁ = ≢-sym (∌dom-≢ lNotInΔ')
        lInRestOfρ = IndirectRefContext.irrelevantReductionsOK lInρ lNeql₁
      in
      findLocationInEnvTypes envTypesList envTypes connected lInRestOfρ 
  findLocationInEnvTypes {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o') φ ψ)} {Δ} {l} {o} {T} envTypesList
    (envTypesConcatMismatch {R = .envTypesList} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = forbiddenRefs} .(Δ ,ₗ l ⦂ T) o o' oNeqo' envTypes)
    connected
    lInρ =
      let
        -- from o ≢ o' I need to prove that objRef o ≢ objRef o'
        -- so I assume objRef o ≡ objRef o' and derive o ≡ o', which is a contradiction.
        oExprNeqo'Expr = λ objRefsEq → (objRefInjective oNeqo') (Eq.sym objRefsEq)
        lInRestOfρ = IndirectRefContext.irrelevantReductionsInValuesOK lInρ oExprNeqo'Expr 
      in
        findLocationInEnvTypes envTypesList envTypes connected lInRestOfρ

  -- If a list of types was all compatible with the env types for (Δ ,ₗ l ⦂ T), then everything in that list is compatible with T.
  prevCompatibilityImpliesCompatibility : ∀ {Σ Δ l o T}
                                          → (RT : RefTypes Σ (Δ ,ₗ l ⦂ T) o)
                                          → (TL : List Type)
                                          → All (λ T → All (_⟷_ T) TL) (RefTypes.envTypesList RT)
                                          → IsConnectedTypeList (RefTypes.envTypesList RT)
                                          → (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objRef o)                          
                                          → All (_⟷_ (T)) TL
  prevCompatibilityImpliesCompatibility {Σ} {Δ} {l} {o} {T} RT TL envTypesCompatibleWithTypes connected lInρ = 
    let
      TInEnvTypes = proj₁ (findLocationInEnvTypes (RefTypes.envTypesList RT) (RefTypes.envTypes RT) connected lInρ)
    in
      Data.List.Relation.Unary.All.lookup envTypesCompatibleWithTypes TInEnvTypes             

  envTypesForbiddenRefsObserved : ∀ {l forbiddenRefs T T' Σ R}
                                  → (Δ : StaticEnv)
                                  → (o : ObjectRef)
                                  → l ∈ forbiddenRefs
                                  → EnvTypes Σ (Δ ,ₗ l ⦂ T) o forbiddenRefs R
                                  → EnvTypes Σ (Δ ,ₗ l ⦂ T') o forbiddenRefs R

      -- We previously found l₁ ⦂ o at the END of ρ, and found l₁ : T in (Δ ,ₗ l ⦂ T). By "l₁NotInForbiddenRefs" we may derive l ≢ l₁.
      -- ρ , l₁ : objRef o
      -- WTS: when we look up l₁ in  (Δ ,ₗ l ⦂ T'), we'll still get objRef o.
  envTypesForbiddenRefsObserved {l} {forbiddenRefs} {T} {T'} {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {.(T₁ ∷ R)} Δ o lInLs
    envTypes@(envTypesConcatMatchFound {R = R} {l = l₁} {T = T₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = forbiddenRefs} .(Δ ,ₗ l ⦂ T) .o origEnvTypes l₁HasTypeT₁WithL l₁NotInForbiddenRefs) =
       let
         l₁NeqL : l₁ ≢ l
         l₁NeqL = listNoncontainment l₁NotInForbiddenRefs lInLs

         l₁HasSameTypeInNewContext = irrelevantExtensionsOK {y = l} {t' = T'} (irrelevantReductionsOK l₁HasTypeT₁WithL l₁NeqL) l₁NeqL

         recursiveCall = envTypesForbiddenRefsObserved {l = l} {T = T} {T' = T'} Δ o (there lInLs) origEnvTypes
      in
         envTypesConcatMatchFound {R = R} (Δ ,ₗ l ⦂ T') o recursiveCall l₁HasSameTypeInNewContext l₁NotInForbiddenRefs

  envTypesForbiddenRefsObserved {l} {forbiddenRefs} {T} {T'} {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {R} Δ o lInLs
    (envTypesConcatMatchNotFound {R = .R} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} .(Δ ,ₗ l ⦂ T) .o origEnvTypes l₁NotInDomΔ') =
      let
        prevEnvTypesObserved = envTypesForbiddenRefsObserved Δ o lInLs origEnvTypes
      in
        envTypesConcatMatchNotFound (Δ ,ₗ l ⦂ T') o prevEnvTypesObserved (∌domPreservation l₁NotInDomΔ')
    
  envTypesForbiddenRefsObserved {l} {forbiddenRefs} {T} {T'} {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o') φ ψ)} {R} Δ o lInLs
    (envTypesConcatMismatch {R = .R} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} .(Δ ,ₗ l ⦂ T) .o o' oNeqO' origEnvTypes) =
      let
        prevEnvTypesObserved = envTypesForbiddenRefsObserved Δ o lInLs origEnvTypes
      in
        envTypesConcatMismatch (Δ ,ₗ l ⦂ T') o o' oNeqO' prevEnvTypesObserved
      
  envTypesForbiddenRefsObserved {l} {forbiddenRefs} {T} {T'} {.(re μ IndirectRefContext.∅ φ ψ)} {.[]} Δ o lInLs
    (envTypesEmpty {μ = μ} {φ = φ} {ψ = ψ} {Δ = .(Δ ,ₗ l ⦂ T)} {o = .o} {forbiddenRefs = .forbiddenRefs}) =
    envTypesEmpty

  -- Inversion for isConnected
  isConnectedImpliesOsConnected : ∀ {Σ Δ o R}
                                  → IsConnected Σ Δ o R
                                  → IsConnectedTypeList (RefTypes.oTypesList R)
  isConnectedImpliesOsConnected {Σ} {Δ} {o} {R} (isConnected R cl _ _ _ ) = cl

  singleElementListsAreConnected : (T : Type)
                                 → IsConnectedTypeList [ T ]
  singleElementListsAreConnected T = consTypeList [] (emptyTypeList refl)          

  -- Basic properties of contexts
  ctxTypesExtension : ∀ {Δ o T}
                      → [ T ] ≡ ctxTypes (Δ , o ⦂ T) o
  ctxTypesExtension {Δ} {o} {T} with o ≟ o
  ... | yes oEq = refl
  ... | no oNeq = ⊥-elim (oNeq refl)

  ctxTypesExtensionNeq : ∀ {Δ o o' T}
                         → o' ≢ o
                         → ctxTypes (Δ , o ⦂ T) o' ≡ ctxTypes Δ o'
  ctxTypesExtensionNeq {Δ} {o} {o'} {T} oNeq with o' ≟ o
  ... | yes p = ⊥-elim (oNeq p)
  ... | no ¬p = refl


  irrelevantRefTypesExtensionO : ∀ {Σ Δ o o' T}
                                → o' ≢ o
                                → (RT : RefTypes Σ (Δ ,ₒ o ⦂ T) o')
                                → RefTypes Σ Δ o'
  irrelevantRefTypesExtensionO {Σ} {Δ} {o} {o'} {T} oNeq rtPrev = {!!} {-
    let
      ctxTypesEq : ctxTypes (StaticEnv.objEnv (Δ ,ₒ o ⦂ T)) o' ≡ ctxTypes (StaticEnv.objEnv Δ) o'
      ctxTypesEq = ctxTypesExtensionNeq oNeq

      envTypesEq : envTypes Σ (Δ ,ₒ o ⦂ T) o' ≡ envTypes Σ Δ o'
      envTypesEq = refl

      refFieldTypesEq = refFieldTypes Σ (Δ ,ₒ o ⦂ T) o' ≡ refFieldTypes Σ Δ o'
      refFieldTypesEq = refl
    in
      cong₃ (λ a → λ b → λ c → rt a b c) ctxTypesEq envTypesEq refFieldTypesEq
-}

  t₁CompatibilityImpliest₂Compatibility : ∀ {Γ TL T₁ T₂ T₃}
                                          → All (λ T' → T₁ ⟷ T') TL
                                          →  Γ ⊢ T₁ ⇛ T₂ / T₃
                                          → All (λ T' → T₂ ⟷ T') TL
  t₁CompatibilityImpliest₂Compatibility {Γ} {.[]} {T₁} {T₂} {T₃} [] spl = []
  t₁CompatibilityImpliest₂Compatibility {Γ} {.(x ∷ xs)} {T₁} {T₂} {T₃} (_∷_ {x = x} {xs = xs} TCompat restCompat) spl =
    let
      TCompat' = proj₁ (splittingRespectsHeap spl (symCompat TCompat))
    in
      (symCompat TCompat') ∷ t₁CompatibilityImpliest₂Compatibility restCompat spl


  t₁CompatibilityImpliest₃Compatibility : ∀ {Γ TL T₁ T₂ T₃}
                                          → All (λ T' → T₁ ⟷ T') TL
                                          →  Γ ⊢ T₁ ⇛ T₂ / T₃
                                          → All (λ T' → T₃ ⟷ T') TL
  t₁CompatibilityImpliest₃Compatibility {Γ} {.[]} {T₁} {T₂} {T₃} [] spl = []
  t₁CompatibilityImpliest₃Compatibility {Γ} {.(x ∷ xs)} {T₁} {T₂} {T₃} (_∷_ {x = x} {xs = xs} TCompat restCompat) spl =
    let
      TCompat' = proj₂ (splittingRespectsHeap spl (symCompat TCompat))
    in
      (symCompat TCompat') ∷ t₁CompatibilityImpliest₃Compatibility restCompat spl

  connectedTypeListsAreConnected : ∀ {T Ts}
                                    → IsConnectedTypeList Ts
                                    → (TIncluded : T ∈ₜ Ts)
                                    → (T'Included : T ∈ₜ Ts)
                                    → TIncluded ≢ T'Included
                                    → T ⟷ T
  connectedTypeListsAreConnected {.x} {.(x ∷ xs)} TsConnected (here {x = x} {xs = xs} refl) (here {x = .x} {xs = .xs} refl) TNeqT' = ⊥-elim (TNeqT' refl)
  connectedTypeListsAreConnected {T} {.(T ∷ D)} (consTypeList {T = .T} {D = D} TConnectedToRest TsConnected) (here {x = .T} {xs = .D} refl) (there {x = .T} {xs = .D} T'InTs) TNeqT' = Data.List.Relation.Unary.All.lookup TConnectedToRest T'InTs
  connectedTypeListsAreConnected {T} {.(T ∷ D)} (consTypeList {T = .T} {D = D} TConnectedToRest TsConnected) (there {x = .T} {xs = .D} TInTs) (here {x = .T} {xs = .D} refl) TNeqT' =  Data.List.Relation.Unary.All.lookup TConnectedToRest TInTs
  connectedTypeListsAreConnected {T} {.(x ∷ xs)} (consTypeList {T = .x} {D = .xs} x₁ restConnected) (there {x = x} {xs = xs} TInTs) (there {x = .x} {xs = .xs} T'InTs) TNeqT' =
    connectedTypeListsAreConnected restConnected TInTs T'InTs λ prfsEqual → TNeqT' (Eq.cong there prfsEqual)

{-
  record EnvTypesForSplitResult (Σ : RuntimeEnv) (Δ : StaticEnv) (T₂ : Type) (T₃ : Type) (l : IndirectRef) (o : ObjectRef) : Set where
    field
      Ts' : List Type
      T₂Compat : (All (λ T' → T₂ ⟷ T') Ts')
      et : (EnvTypes Σ (Δ ,ₗ l ⦂ T₃) o forbiddenRefs Ts')
      connected : (IsConnectedTypeList Ts')
      externalCompat : (λ T' → All (λ T'' → T' ⟷ T'') Ts → All (λ T'' → T' ⟷ T'') Ts')
-}

  -- Trying to assume that T₁ is compatible with all the Ts is too strong, since T₁ may be in the Ts.
  -- What do I actually need here I need to show that T₂ is compatible with all the new types.
  -- So 
  envTypesForSplit : ∀ {Γ Σ Δ l o T₁ T₂ T₃ Ts forbiddenRefs}
                                   → EnvTypes Σ (Δ ,ₗ l ⦂ T₁) o forbiddenRefs Ts
                                   → (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objRef o)
                                   → All (λ T' → (T' ≢ T₁) → T₁ ⟷ T') Ts
                                   → IsConnectedTypeList Ts
                                   → Γ ⊢ T₁ ⇛ T₂ / T₃
                                   → ∃[ Ts' ] ((EnvTypes Σ (Δ ,ₗ l ⦂ T₃) o forbiddenRefs Ts') ×
                                              (All (λ T' → T₂ ⟷ T') Ts') ×
                                              (IsConnectedTypeList Ts') ×
                                              (∀ T' → All (λ T'' → T' ⟷ T'') Ts → All (λ T'' → T' ⟷ T'') Ts'))
  envTypesForSplit {Γ} {Σ@.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {.(T₄ ∷ R)} {forbiddenRefs}
    origMatch@(envTypesConcatMatchFound {R = R} {l = l₁} {T = T₄} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} .(Δ ,ₗ l ⦂ T₁) o origEnvTypes lookupResult l₁NotForbidden)
    lInρ
    (T₁Connected ∷ RConnected)
    (consTypeList firstConnected restConnected)
    spl with l₁ ≟ l
  ... | yes eq
    = 
      -- The last step in the proof of EnvTypes (ρ , l₁ ⦂ o) (Δ ,ₗ l ⦂ T₃) o T₁∷R is that we looked up l₁ and prepended the result to the rest of the Ts. 
      -- If l₁ ≡ l, then Ts' is T₃ ∷ R. Otherwise, we found l₁ ⦂ T₄ in Δ and Ts' is T₄ ∷ (recurse with the rest).
        ⟨ Ts' , ⟨ envTypesTs' , ⟨ T₂CompatWithT₃ ∷ T₂CompatWithRestOfEnvTypes ,  ⟨ (consTypeList T₃CompatWithRestOfEnvTypes restConnected) , externalCompat ⟩ ⟩ ⟩ ⟩
      where
        Ts'' = R
        Ts' = T₃ ∷ Ts''

        -- WTS: (EnvTypes Σ (Δ ,ₗ l ⦂ T₃) o Ts') × All (λ T' → T ⟷ T') Ts')
        lookupResultWithl : (StaticEnv.locEnv Δ , l ⦂ T₁) ∋ l ⦂ T₄
        lookupResultWithl = Eq.subst (λ a → StaticEnv.locEnv Δ , l ⦂ T₁ ∋ a ⦂ T₄) eq lookupResult

        origEnvTypesWithL : EnvTypes (re μ ρ φ ψ) (Δ ,ₗ l₁ ⦂ T₁) o (l₁ ∷ forbiddenRefs) R
        origEnvTypesWithL = Eq.subst (λ a → EnvTypes (re μ ρ φ ψ) (Δ ,ₗ a ⦂ T₁) o (l₁ ∷ forbiddenRefs) R) (Eq.sym eq) origEnvTypes

        forbiddenOK = envTypesForbiddenRefsObserved {T' = T₃} Δ o (here refl) origEnvTypesWithL
        forbiddenOKInL₁ : EnvTypes (re μ ρ φ ψ) (Δ ,ₗ l ⦂ T₃) o (l₁ ∷ forbiddenRefs) R
        forbiddenOKInL₁ = Eq.subst (λ a →  EnvTypes (re μ ρ φ ψ) (Δ ,ₗ a ⦂ T₃) o (l₁ ∷ forbiddenRefs) R) eq forbiddenOK

        -- We already know that the first match in ρ for o is l, so the new Ts' has to be right.
        envTypesTs' : EnvTypes Σ (Δ ,ₗ l ⦂ T₃) o forbiddenRefs Ts'
        envTypesTs' = envTypesConcatMatchFound {l = l₁} (Δ ,ₗ l ⦂ T₃) o forbiddenOKInL₁
          (Eq.subst (λ a →  StaticEnv.locEnv Δ , a ⦂ T₃ ∋ l₁ ⦂ T₃) eq Z) l₁NotForbidden

        T₄EqT₁ : T₄ ≡ T₁
        T₄EqT₁ = contextLookupUnique lookupResultWithl Z


        T₄CompatWithRestOfEnvTypes = firstConnected
        T₁CompatWithRestOfEnvTypes = Eq.subst (λ a → All (λ T' → a ⟷ T') R) T₄EqT₁ T₄CompatWithRestOfEnvTypes
        
        T₂CompatWithRestOfEnvTypes : All (λ T' → T₂ ⟷ T') R
        T₂CompatWithRestOfEnvTypes = t₁CompatibilityImpliest₂Compatibility T₁CompatWithRestOfEnvTypes spl

        T₂CompatWithT₃ = splitCompatibility spl

        -- Need to prove everything in R is connected to T₃, given that everything in R is connected to T₄ (≡ T₁).
        -- Follows from splittingRespectsHeap.
        T₃CompatWithRestOfEnvTypes : All (λ T' → T₃ ⟷ T') R
        T₃CompatWithRestOfEnvTypes = t₁CompatibilityImpliest₃Compatibility T₁CompatWithRestOfEnvTypes spl
        
        externalCompat : (T' : Type) →  All (λ T'' → T' ⟷ T'') (T₄ ∷ R) → All (λ T'' → T' ⟷ T'') Ts'
        externalCompat T' (_∷_ {x = x} {xs = xs} px T'CompatWithTs) =
          let
            T'CompatT₁ = Eq.subst (λ a →  T' ⟷ a) T₄EqT₁ px
            T'CompatT₃ = proj₂ (splittingRespectsHeap spl T'CompatT₁)
          in
            T'CompatT₃ ∷ T'CompatWithTs
  envTypesForSplit {Γ} {Σ@.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {.(T₄ ∷ R)} {forbiddenRefs}
    origMatch@(envTypesConcatMatchFound {R = R} {l = l₁} {T = T₄} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} .(Δ ,ₗ l ⦂ T₁) o origEnvTypes lookupResult l₁NotForbidden)
    lInρ
    (T₁Connected ∷ RConnected)
    allConnected@(consTypeList firstConnected restConnected)
    spl | no nEq with T₄ ≟t T₁
  ... | yes T₄EqT₁ =
        ⟨ Ts' , ⟨ envTypesTs' , ⟨ Ts'Compat , ⟨ consTypeList T₄CompatWithR' (proj₁ (proj₂ (proj₂ (proj₂ R'WithProof)))) , externCompatibility' ⟩ ⟩ ⟩ ⟩
      where
       -- Because we're in envTypeConcatMatchFound, l₁ ⦂ o was at the end of the context, and we found l₁ ⦂ T₄ in Δ , l ⦂ T₁.
        -- But l ≢ l₁, so we will be able to find l₁ ⦂ T₄ in Δ and use the same rule.
        -- However, there may be l's in ρ, which may result in a different R than before! Some T₁'s may be replaced with T₃'s.
        lInRestOfρ = IndirectRefContext.irrelevantReductionsOK lInρ (≢-sym nEq)
        R'WithProof = envTypesForSplit origEnvTypes lInRestOfρ RConnected restConnected spl
        R' = proj₁ R'WithProof
        Ts' = T₄ ∷ R'
        
        l₁InΔ : ((StaticEnv.locEnv Δ) , l ⦂ T₃) ∋ l₁ ⦂ T₄
        l₁InΔ = irrelevantExtensionsOK {t = T₄} {t' = T₃} (irrelevantReductionsOK lookupResult nEq) nEq

        envTypesTs' = envTypesConcatMatchFound (Δ ,ₗ l ⦂ T₃) o (proj₁ (proj₂ R'WithProof)) l₁InΔ l₁NotForbidden

        -- Previously, we knew that T₄ was compatible with everything in R.
        -- Specificially, we have T₁Connected : (T₄ ≢ T₁) → T₁ ⟷ T₄.
        -- Everything that was compatible with T₁ has to be compatible with T₂. 
        
        foundLocationInRest = findLocationInEnvTypes R origEnvTypes restConnected lInRestOfρ 
        T₁InR : T₁  ∈ₜ R
        T₁InR = proj₁ foundLocationInRest

        proofsNotEq : (there T₁InR) ≢ (here (Eq.sym T₄EqT₁))
        proofsNotEq ()
                     
        T₁CompatWithT₁ : T₁ ⟷ T₁
        T₁CompatWithT₁ = connectedTypeListsAreConnected allConnected (there T₁InR) (here (Eq.sym T₄EqT₁)) proofsNotEq
        -- We found another copy of T₁ in the original envTypes. But we know the original envTypes was all connected.
        -- There has to be ANOTHER copy of T₁ in R. We know THIS T₁ is connected to THAT T₁ because THIS T₁ is connected to everything in R.
        T₂CompatWithT₁ : T₂ ⟷ T₁
        T₂CompatWithT₁ = symCompat (proj₁ (splittingRespectsHeap spl T₁CompatWithT₁))
        T₂CompatWithT₄ = Eq.subst (λ a → T₂ ⟷ a) (Eq.sym T₄EqT₁) T₂CompatWithT₁

        T₂CompatWithR' = proj₁ (proj₂ (proj₂ R'WithProof))
        Ts'Compat = T₂CompatWithT₄ ∷ T₂CompatWithR'

        externCompatibility = proj₂ (proj₂ (proj₂ (proj₂ R'WithProof)))
        T₄CompatWithR' : All (λ T' → T₄ ⟷ T') R'
        T₄CompatWithR' = externCompatibility T₄ firstConnected

        externCompatibility' : (T' : Type) →  All (λ T'' → T' ⟷ T'') (T₄ ∷ R) → All (λ T'' → T' ⟷ T'') Ts'
        externCompatibility' T' (_∷_ {x = x} {xs = xs} px T'CompatWithTs) =
          px ∷ externCompatibility T' T'CompatWithTs

  ... | no T₄NeqT₁ =
        ⟨ Ts' , ⟨ envTypesTs' , ⟨ Ts'Compat ,  ⟨ consTypeList T₄CompatWithR' (proj₁ (proj₂ (proj₂ (proj₂ R'WithProof)))) , externCompatibility' ⟩ ⟩ ⟩ ⟩
      where
        -- Because we're in envTypeConcatMatchFound, l₁ ⦂ o was at the end of the context, and we found l₁ ⦂ T₄ in Δ , l ⦂ T₁.
        -- But l ≢ l₁, so we will be able to find l₁ ⦂ T₄ in Δ and use the same rule.
        -- However, there may be l's in ρ, which may result in a different R than before! Some T₁'s may be replaced with T₃'s.
        lInRestOfρ = IndirectRefContext.irrelevantReductionsOK lInρ (≢-sym nEq)
        R'WithProof = envTypesForSplit origEnvTypes lInRestOfρ RConnected restConnected spl
        
        R' = proj₁ R'WithProof
        Ts' = T₄ ∷ R'
        
        l₁InΔ : ((StaticEnv.locEnv Δ) , l ⦂ T₃) ∋ l₁ ⦂ T₄
        l₁InΔ = irrelevantExtensionsOK {t = T₄} {t' = T₃} (irrelevantReductionsOK lookupResult nEq) nEq

        envTypesTs' = envTypesConcatMatchFound (Δ ,ₗ l ⦂ T₃) o (proj₁ (proj₂ R'WithProof)) l₁InΔ l₁NotForbidden

        -- Previously, we knew that T₄ was compatible with everything in R.
        -- Specificially, we have T₁Connected : (T₄ ≢ T₁) → T₁ ⟷ T₄.
        -- Everything that was compatible with T₁ has to be compatible with T₂.
        T₄CompatWithT₁ :  T₄ ⟷ T₁
        T₄CompatWithT₁ = symCompat (T₁Connected T₄NeqT₁)
        T₄CompatWithT₂ : T₄ ⟷ T₂
        T₄CompatWithT₂ = proj₁ (splittingRespectsHeap spl T₄CompatWithT₁)

        T₂CompatWithT₄ = symCompat T₄CompatWithT₂
        T₂CompatWithR'  = proj₁ (proj₂ (proj₂ R'WithProof))
        Ts'Compat = T₂CompatWithT₄ ∷ T₂CompatWithR'

        externCompatibility = proj₂ (proj₂ (proj₂ (proj₂ R'WithProof)))
        T₄CompatWithR' : All (λ T' → T₄ ⟷ T') R'
        T₄CompatWithR' = externCompatibility T₄ firstConnected

        externCompatibility' : (T' : Type) →  All (λ T'' → T' ⟷ T'') (T₄ ∷ R) → All (λ T'' → T' ⟷ T'') Ts'
        externCompatibility' T' (_∷_ {x = x} {xs = xs} px T'CompatWithTs) =
          px ∷ externCompatibility T' T'CompatWithTs
    
  envTypesForSplit {Γ} {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {Ts} {T}
    (envTypesConcatMatchNotFound {R = .Ts} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} .(Δ ,ₗ l ⦂ T₁) o origEnvTypes l₁NotInΔ') lInρ T₁Connected origCompat spl =
    let
      lNeql₁ = ≢-sym (∌dom-≢ l₁NotInΔ')
      lInRestOfρ = IndirectRefContext.irrelevantReductionsOK lInρ lNeql₁
      compatibilityWithOrigρ = envTypesForSplit origEnvTypes lInRestOfρ T₁Connected origCompat spl
      envTypes = envTypesConcatMatchNotFound (Δ ,ₗ l ⦂ T₃) o (proj₁ (proj₂ compatibilityWithOrigρ)) (∌domPreservation l₁NotInΔ' )
    in
      ⟨ proj₁ compatibilityWithOrigρ , ⟨ envTypes ,  proj₂ (proj₂ compatibilityWithOrigρ) ⟩ ⟩
      
  envTypesForSplit {Γ} {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o') φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {Ts} {T}
    (envTypesConcatMismatch {R = .Ts} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} .(Δ ,ₗ l ⦂ T₁) o o' oNeqo' origEnvTypes) lInρ T₁Connected origCompat spl =
      let
        oExprNeqo'Expr = λ objRefsEq → (objRefInjective oNeqo') (Eq.sym objRefsEq)
        lInRestOfρ = IndirectRefContext.irrelevantReductionsInValuesOK lInρ oExprNeqo'Expr 

        compatibilityWithOrigρ = envTypesForSplit origEnvTypes lInRestOfρ T₁Connected origCompat spl
        envTypes = envTypesConcatMismatch (Δ ,ₗ l ⦂ T₃) o o' oNeqo' (proj₁ (proj₂ compatibilityWithOrigρ))
      in
        ⟨ proj₁ compatibilityWithOrigρ , ⟨ envTypes , proj₂ (proj₂ compatibilityWithOrigρ) ⟩ ⟩
      
  envTypesForSplit {Γ} {.(re μ IndirectRefContext.∅ φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {.[]} {T}
    (envTypesEmpty {μ = μ} {φ = φ} {ψ = ψ} {Δ = .(Δ ,ₗ l ⦂ T₁)} {o = .o}) T₁InTs T₁Connected origCompat spl =
      ⟨ [] , ⟨ envTypesEmpty , ⟨ [] , ⟨ emptyTypeList refl , (λ T' → λ a → []) ⟩ ⟩ ⟩ ⟩


{-
  newEnvTypesCompatibleWithT₁ : ∀ {Σ Δ l T₁ T₃ o o'}
                                → EnvTypes Σ (Δ ,ₗ l ⦂ T₁) o forbiddenRefs Ts
                                → Γ ⊢ T₁ ⇛ T₂ / T₃
                                → IsConnected Σ (Δ ,ₗ l ⦂ T₁) o' RT
                                → (envTypesList : (EnvTypes Σ (Δ ,ₗ l ⦂ T₃) o forbiddenRefs Ts')
                                → All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT')) [ T₁ ]
  newEnvTypesCompatibleWithT₁ {Σ} {Δ} {l} {T₁} {T₃} {o} {o'} RT RTConnected RT' with (RefTypes.envTypes RT)
  newEnvTypesCompatibleWithT₁ {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o₁) φ ψ)} {Δ} {l} {T₁} {T₃} {o} {.o₁}
    record { oTypesList = oTypesList ; oTypes = oTypes ; envTypesList = .(T ∷ R) ; envTypes = envTypes ; fieldTypesList = fieldTypesList }
    RTConnected
    RT' |
      envTypesConcatMatchFound {R = R} {l = l₁} {T = T} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .[]} .(Δ ,ₗ l ⦂ T₁) o₁ qq x x₁ with l₁ ≟ l 
  ... | yes eq = {!let
          TEqT₁ : T ≡ T₁
          TEqT₁ = ?
          TCompatWithT₃ = ?
        in
          TCompatWithT₃ ∷ newEnvTypesCompatibleWithT₁ RT!}
  ... | no nEq = {!!}
        {!!}

  newEnvTypesCompatibleWithT₁ {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o₁) φ ψ)} {Δ} {l} {T₁} {T₃} {o} {.o₁}
    RT
    RTConnected
    RT' |
      envTypesConcatMatchNotFound {R = .(RefTypes.envTypesList RT)} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .[]} .(Δ ,ₗ l ⦂ T₁) o₁ qq x =
      {!!}
  newEnvTypesCompatibleWithT₁ {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o') φ ψ)} {Δ} {l} {T₁} {T₃} {o} {.o₁}
    RT
    RTConnected
    RT' |
      envTypesConcatMismatch {R = .(RefTypes.envTypesList RT)} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .[]} .(Δ ,ₗ l ⦂ T₁) o₁ o' x qq =
      {!!}
  newEnvTypesCompatibleWithT₁ {.(re μ IndirectRefContext.∅ φ ψ)} {Δ} {l} {T₁} {T₃} {o} {o'}
    record { oTypesList = oTypesList ; oTypes = oTypes ; envTypesList = .[] ; envTypes = envTypes ; fieldTypesList = fieldTypesList }
    RTConnected
    RT' |
      envTypesEmpty {μ = μ} {φ = φ} {ψ = ψ} {Δ = .(Δ ,ₗ l ⦂ T₁)} {o = .o'} {forbiddenRefs = .[]} =
        {!!}

-}
  oExtensionIrrelevantToEnvTypes :  ∀ {Σ Δ R o o' forbiddenRefs T}
                                    → EnvTypes Σ Δ o' forbiddenRefs R
                                    → EnvTypes Σ (Δ ,ₒ o ⦂ T) o' forbiddenRefs R
  oExtensionIrrelevantToEnvTypes {.(re μ (ρ IndirectRefContext., l ⦂ objRef o₁) φ ψ)} {Δ} {.(T₁ ∷ R)} {o} {.o₁} {forbiddenRefs} {T}
    (envTypesConcatMatchFound {R = R} {l = l} {T = T₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} Δ o₁ envTypes x x₁) =
      let
        recurse = oExtensionIrrelevantToEnvTypes envTypes
      in
        envTypesConcatMatchFound (Δ ,ₒ o ⦂ T) o₁ recurse x x₁
  oExtensionIrrelevantToEnvTypes {.(re μ (ρ IndirectRefContext., l ⦂ objRef o₁) φ ψ)} {Δ} {R} {o} {.o₁} {forbiddenRefs} {T}
    (envTypesConcatMatchNotFound {R = .R} {l = l} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} Δ o₁ envTypes x) =
      let
        recurse = oExtensionIrrelevantToEnvTypes envTypes
      in
        envTypesConcatMatchNotFound (Δ ,ₒ o ⦂ T) o₁ recurse x
  oExtensionIrrelevantToEnvTypes {.(re μ (ρ IndirectRefContext., l ⦂ objRef o') φ ψ)} {Δ} {R} {o} {.o₁} {forbiddenRefs} {T}
    (envTypesConcatMismatch {R = .R} {l = l} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} Δ o₁ o' x envTypes) =
       let
        recurse = oExtensionIrrelevantToEnvTypes envTypes
      in
        envTypesConcatMismatch (Δ ,ₒ o ⦂ T) o₁ o' x recurse
  oExtensionIrrelevantToEnvTypes {.(re μ IndirectRefContext.∅ φ ψ)} {Δ} {.[]} {o} {o'} {forbiddenRefs} {T}
    (envTypesEmpty {μ = μ} {φ = φ} {ψ = ψ} {Δ = .Δ} {o = .o'} {forbiddenRefs = .forbiddenRefs}) =
      envTypesEmpty


  lExtensionCompatibility : ∀ {Γ Σ Δ l T₁ T₂ T₃ o}
                            → (RT : RefTypes Σ (Δ ,ₗ l ⦂ T₁) o)
                            → IsConnected Σ (Δ ,ₗ l ⦂ T₁) o RT
                            → Γ ⊢ T₁ ⇛ T₂ / T₃
                            → ∃[ RT' ] (IsConnected Σ (Δ ,ₗ l ⦂ T₃) o RT')
  lExtensionCompatibility {Γ} {Σ} {Δ} {l} {T₁} {T₂} {T₃} {o} rt rConnected@(isConnected R oConnected oAndFieldsConnected oAndLsConnected envFieldConnected) spl =
    let
      envTypes = {!envTypeCompatibility rt rConnected spl!}
    in
      ⟨
        record { oTypesList = RefTypes.oTypesList rt ;
                 oTypes = RefTypes.oTypes rt ;
                 envTypesList = {!proj₁ envTypes!} ;
                 envTypes = proj₂ envTypes ;
                 fieldTypesList = RefTypes.fieldTypesList rt
               }
        , {!!}
      ⟩ {-
      isConnected (refTypes Σ (Δ ,ₗ l ⦂ T₃) o) oConnected oAndFieldsConnected (oAndLsConnected' rConnected) {!!}
      where
        R' = refTypes Σ (Δ ,ₗ l ⦂ T₃) o
        oAndLsConnected' : IsConnected (refTypes Σ (Δ ,ₗ l ⦂ T₁) o) → All (λ T → All (λ T' → T ⟷ T') (RefTypes.lTypes R')) (RefTypes.oTypesList R')
        oAndLsConnected' (isConnected R oConnected oAndFieldsConnected oAndLsConnected envFieldConnected) with (RefTypes.oTypesList R')
        oAndLsConnected' (isConnected .(refTypes Σ (Δ ,ₗ l ⦂ T₁) o) oConnected oAndFieldsConnected oAndLsConnected envFieldConnected) | [] = []
        oAndLsConnected' (isConnected .(refTypes Σ (Δ ,ₗ l ⦂ T₁) o) oConnected oAndFieldsConnected oAndLsConnected envFieldConnected) | T ∷ rest = 
          let
            -- T is a type of something that is in oTypesList R'.
            TCompatWithR = Data.List.Relation.Unary.All.head oAndLsConnected          
            TOK = envTypesForSplit {Γ} {Σ} {Δ} {l} {o} T TCompatWithR spl
            restOK = {!!}
          in
            TOK ∷ restOK
-}


  consListEq : ∀ {A : Set}
               → ∀ {R R' : List A }
               → ∀ {T : A}
               → R ≡ R'
               → _≡_ {_} {List A} (T ∷ R) (T ∷ R')
  consListEq {A} {[]} {[]} {T₁} refl = refl
  consListEq {A} {[]} {x ∷ R'} {T₁} ()
  consListEq {A} {x ∷ R} {x' ∷ R'} {T} refl = refl


  -- Previously, all the types aliasing o were connected. Now, we've extended the context due to a split, and we need to show we still have connectivity.
  -- The idea here is that the expression in question is of type T₂, so the reference left in the heap is of type T₃.
  -- o corresponds to the bject that was affected by the split, whereas o' is the reference we are interested in analyzing aliases to.
  splitReplacementOK : ∀ {Γ Σ Δ o o' l}
                       → {t₁ t₂ : Tc}
                       → {T₁ T₂ T₃ : Type}
                       → (T₁EqctT₁ : T₁ ≡ (contractType t₁))
                       → (T₂EqctT₂ : T₂ ≡ (contractType t₂))
                       → (globalConsistency : (Σ & (Δ ,ₗ l ⦂ T₁) ok))
                       → o ≡ proj₁ (locLookup {T = t₁} globalConsistency (Eq.subst (λ a → StaticEnv.locEnv (Δ ,ₗ l ⦂ T₁) ∋ l ⦂ a) T₁EqctT₁ (TypeEnvContext.Z {StaticEnv.locEnv Δ} {l} {T₁}) ))
                       → (RT : RefTypes Σ (Δ ,ₗ l ⦂ T₁) o')
                       → IsConnected Σ (Δ ,ₗ l ⦂ T₁) o' RT
                       → Γ ⊢ T₁ ⇛ T₂ / T₃
                       → ∃[ RT' ] (IsConnected Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₂) o' RT')
  splitReplacementOK {Γ} {Σ} {Δ} {o} {o'} {l} {t₁} {t₂} {T₁} {T₂} {T₃}
    refl
    refl
    origConsis@(ok _ _ _ objLookup refConsistencyFunc)
    refl
    RT
    rConnected@(isConnected _ oTypesListConnected oConnectedToFieldTypes oConnectedToLTypes envFieldConnected@(envTypesConnected _ etc fieldTypesConnected fieldEnvTypesConnected))
    spl
    with (o' ≟ o)
  ... | yes osEq =
    -- In this case, there are no O types yet, but adding o ⦂ T₂ will add one.
      ⟨ RT' , isConnected RT' TsForOsConnected  TsForOsConnectedToFieldTypes TsForOsConnectedToLTypes envAndFieldConnected ⟩
    where
      Δ' = ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₂)

      TsForOs = [ T₂ ] -- Since o ≡ o', when we look up o' in the new context, we'll get T₂.

      o'TypeExtension : [ T₂ ] ≡  ctxTypes (StaticEnv.objEnv Δ Context., o' ⦂ T₂) o'
      o'TypeExtension = ctxTypesExtension {StaticEnv.objEnv (Δ ,ₗ l ⦂ T₃)} {o'} {T₂}
      
      RT'o'TypesEq : [ T₂ ] ≡ ctxTypes (StaticEnv.objEnv Δ') o'
      RT'o'TypesEq =  Eq.subst (λ a → ([ T₂ ] ≡ (ctxTypes (StaticEnv.objEnv Δ , a ⦂ T₂) o'))) (osEq) o'TypeExtension 

      lLookupResult = objLookup l t₁ Z
      oForL = proj₁ lLookupResult 
      lInρ = proj₁ (proj₂ lLookupResult)


      -- Can I show that T₂ is compatible with all of the old env types? Because that's really what I need.
      lInρForFindLocation = (Eq.subst (λ a → RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objRef a) (Eq.sym osEq) lInρ)
      TInEnvTypes = findLocationInEnvTypes (RefTypes.envTypesList RT) (RefTypes.envTypes RT) etc lInρForFindLocation
      newEnvTypes = envTypesForSplit (RefTypes.envTypes RT) lInρForFindLocation (proj₂ TInEnvTypes) etc spl

      RT' : RefTypes Σ Δ' o'
      RT' = record {oTypesList = TsForOs ;
                   oTypes = Eq.sym (RT'o'TypesEq) ;
                   envTypesList = proj₁ newEnvTypes ;
                   envTypes = oExtensionIrrelevantToEnvTypes (proj₁ (proj₂ newEnvTypes)) ;
                   fieldTypesList = RefTypes.fieldTypesList RT} -- field types are unchanged.

      TsForOsIsRight : TsForOs ≡ (RefTypes.oTypesList RT')
      TsForOsIsRight =  Eq.trans RT'o'TypesEq (RefTypes.oTypes RT')

      TsForOsConnected : IsConnectedTypeList (RefTypes.oTypesList RT')
      TsForOsConnected = (Eq.subst (λ a → IsConnectedTypeList a) TsForOsIsRight (singleElementListsAreConnected T₂))

      -- Need to find the proof that T₁ is compatible with all the field types and use it directly. It has to be in there somewhere.
      -- Specifically, it has to be in envFieldConnected because T₁ has to have been in the old env types.
 
      oldEnvFieldsCompatible : All (λ T → All (_⟷_ T) (RefTypes.fieldTypesList RT)) (RefTypes.envTypesList RT)
      oldEnvFieldsCompatible = envFieldInversion3 envFieldConnected
      -- One of the items in oldEnvFieldsCompatible is proof that T₁ is connected to everything in fieldTypesList! But which one?
      
      T₁CompatWithAllFieldTypes : All (λ T' → T₁ ⟷ T') (RefTypes.fieldTypesList RT)      
      T₁CompatWithAllFieldTypes =
        prevCompatibilityImpliesCompatibility RT (RefTypes.fieldTypesList RT) oldEnvFieldsCompatible etc (Eq.subst (λ a → RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objRef a) (Eq.sym osEq) lInρ)

      T₂CompatWithAllFieldTypes = t₁CompatibilityImpliest₂Compatibility T₁CompatWithAllFieldTypes spl

      TsForOsConnectedToFieldTypes :  All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList RT')) (RefTypes.oTypesList RT')
      TsForOsConnectedToFieldTypes = T₂CompatWithAllFieldTypes ∷ []

      T₂ConnectedToLTypes :  All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT')) [ T₂ ]
      T₂ConnectedToLTypes =  proj₁ (proj₂ (proj₂ newEnvTypes)) ∷ []

      TsForOsConnectedToLTypes :  All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT')) (RefTypes.oTypesList RT')
      TsForOsConnectedToLTypes =  Eq.subst (λ a →  All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT')) a) TsForOsIsRight T₂ConnectedToLTypes

      externalCompat =  proj₂ (proj₂ (proj₂ (proj₂ newEnvTypes)))
      -- TODO: use fieldEnvTypesConnected
      convertCompatibility : (All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList RT)) (RefTypes.envTypesList RT)) → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList RT')) (RefTypes.envTypesList RT')
      convertCompatibility [] = {![]!}
      convertCompatibility (_∷_ {x = x} {xs = xs} origFieldTypesEnvTypesCompat origFieldTypesEnvTypesCompat₁) = {!!}

      envTypesCompatibleWithFieldTypes : All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList RT')) (RefTypes.envTypesList RT')
      envTypesCompatibleWithFieldTypes = {!!}

      envAndFieldConnected : IsConnectedEnvAndField Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₂) o' RT'
      envAndFieldConnected = envTypesConnected RT' ( proj₁ (proj₂ (proj₂ (proj₂ newEnvTypes)))) fieldTypesConnected envTypesCompatibleWithFieldTypes
       
  ... | no osNeq =
        --  The change in Δ has no impact on looking up o', since o' ≢ o.
        let
          --connectivity : IsConnected Σ (Δ ,ₗ l ⦂ T₃) o' RT'
          --connectivity = lExtensionCompatibility {Γ} {Σ} {_} {_} {_} {_} {_} {_} rConnected spl RT'
          foo = osNeq

          --ΔRefTypesEq1 : RefTypes Σ (Δ ,ₗ l ⦂ T₃) o'
          --ΔRefTypesEq1 = irrelevantRefTypesExtensionO {Σ} {Δ ,ₗ l ⦂ T₃} osNeq RT

          -- Now it suffices to show IsConnected (refTypes (Δ ,ₗ l ⦂ T₃) o').
          --connectivity : IsConnected (refTypes Σ (Δ ,ₗ l ⦂ T₃) o')
          --connectivity = lExtensionCompatibility {Γ} {Σ} {_} {_} {_} {_} {_} {_} rConnected spl
        in
          {!!}
          -- Eq.subst (λ a → IsConnected a) (Eq.sym ΔRefTypesEq1) connectivity

