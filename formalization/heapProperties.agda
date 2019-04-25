{-# OPTIONS --allow-unsolved-metas #-}
{-# OPTIONS --show-implicit #-}

module HeapProperties where

  open import Silica

  import Relation.Binary.PropositionalEquality as Eq
  import Context


  import Data.AVL.Sets
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
      oTypes : List Type -- Corresponds to types from the o's in Δ.
      envTypesList : List Type
      envTypes : EnvTypes Σ Δ o [] envTypesList
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

  data IsConnectedEnvAndField  (Σ : RuntimeEnv) (Δ : StaticEnv) (o : ObjectRef) : RefTypes Σ Δ o → Set where
    envTypesConnected : (R : RefTypes Σ Δ o)
                        → IsConnectedTypeList (RefTypes.envTypesList R)
                        → IsConnectedTypeList (RefTypes.fieldTypes R)
                        → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypes R)) (RefTypes.envTypesList R) -- all of the l types are connected to all of the field types
                        ----------------------------------------------
                        → IsConnectedEnvAndField Σ Δ o R

  data IsConnected (Σ : RuntimeEnv) (Δ : StaticEnv) (o : ObjectRef) : RefTypes Σ Δ o → Set where              
    isConnected : (R : RefTypes Σ Δ o)
                  → IsConnectedTypeList (RefTypes.oTypes R)
                  → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypes R)) (RefTypes.oTypes R) -- all of the o types are connected to all of the field types
                  → All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList R)) (RefTypes.oTypes R) -- all of the o types are connected to all of the l types
                  → IsConnectedEnvAndField Σ Δ o R
                  ----------------------------------------------
                  → IsConnected Σ Δ o R




  refFieldTypesHelper : ObjectRefEnv → StaticEnv → ObjectRef → List Type
  refFieldTypesHelper ObjectRefContext.∅ Δ o = []
  refFieldTypesHelper (ObjectRefContext._,_⦂_ μ o' obj) Δ o = refFieldTypesHelper μ Δ o  -- TODO; this is bogus!

  refFieldTypes : RuntimeEnv → StaticEnv → ObjectRef → List Type
  refFieldTypes Σ Δ o = refFieldTypesHelper (RuntimeEnv.μ Σ) Δ o

--  refTypes : RuntimeEnv → StaticEnv → ObjectRef → RefTypes
--  refTypes Σ Δ o = record {oTypes = (ctxTypes (StaticEnv.objEnv Δ) o) ; lTypes = (envTypes Σ Δ o) ; fieldTypes = (refFieldTypes Σ Δ o)}


  -- =================== LEMMAS RELATED TO HEAP CONSISTENCY =================
  -- Changes in Δ that pertain to forbidden locations are irrelevant.
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
                                  → IsConnectedTypeList (RefTypes.oTypes R)
  isConnectedImpliesOsConnected {Σ} {Δ} {o} {R} (isConnected R cl _ _ _ ) = cl

  singleElementListsAreConnected : (T : Type)
                                 → IsConnectedTypeList [ T ]
  singleElementListsAreConnected T = consTypeList [] (emptyTypeList refl)          

  -- Basic properties of contexts
  ctxTypesExtension : ∀ {Δ o T}
                      → ctxTypes (Δ , o ⦂ T) o ≡ [ T ]
  ctxTypesExtension {Δ} {o} {T} with o ≟ o
  ... | yes oEq = refl
  ... | no oNeq = ⊥-elim (oNeq refl)

  ctxTypesExtensionNeq : ∀ {Δ o o' T}
                         → o' ≢ o
                         → ctxTypes (Δ , o ⦂ T) o' ≡ ctxTypes Δ o'
  ctxTypesExtensionNeq {Δ} {o} {o'} {T} oNeq with o' ≟ o
  ... | yes p = ⊥-elim (oNeq p)
  ... | no ¬p = refl

  cong₃ : ∀ {a b c d} {A : Set a} {B : Set b} {C : Set c} {D : Set d}
        (f : A → B → C → D) {x y u v w t} → x ≡ y → u ≡ v → w ≡ t → f x u w ≡ f y v t
  cong₃ f refl refl refl = refl


  irrelevantRefTypesExtensionO : ∀ {Σ Δ o o' T RT}
                                → o' ≢ o
                                → (RT : RefTypes Σ (Δ ,ₒ o ⦂ T) o')
                                → RefTypes Σ Δ o'
  irrelevantRefTypesExtensionO {Σ} {Δ} {o} {o'} {T} oNeq rtPrev = ? {-
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

  EnvTypesChangeForΔReplacement : ∀ {Σ Δ l T T' TCompat o R forbiddenRefs } -- TCompat is something that everything in R is compatible with.
                                  → EnvTypes Σ (Δ ,ₗ l ⦂ T) o forbiddenRefs R
                                  → All (λ T' → TCompat ⟷ T') R
                                  → (TCompat ⟷ T → (TCompat ⟷ T')) -- If TCompat was compatible with T, then TCompat is compatible with T'.
                                  → ∃[ R' ] (EnvTypes Σ (Δ ,ₗ l ⦂ T') o forbiddenRefs R' ×  All (λ T' → TCompat ⟷ T') R')
                                  
  EnvTypesChangeForΔReplacement {Σ} {Δ} {l} {T} {T'} {TCompat} {o} {R} {forbiddenRefs} origEnvTypes origCompat TCompatCompatibility with (l ∈? forbiddenRefs)
  ... | yes lInForbiddenRefs = ⟨ R , ⟨ envTypesForbiddenRefsObserved Δ o lInForbiddenRefs origEnvTypes , origCompat ⟩ ⟩

-- Two cases for envTypesConcatMatchFound: either l₁ is the last thing in Δ, in which case we're going to find T', or it's not, in which case we recurse with the rest of the list.
  EnvTypesChangeForΔReplacement {.(re μ (ρ IndirectRefContext., l ⦂ objRef o) φ ψ)} {Δ} {l} {T} {T'} {TCompat} {o} {.(T ∷ R)} {forbiddenRefs}
    (envTypesConcatMatchFound {R = R} {l = l} {T = .T} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} .(Δ ,ₗ l ⦂ T) o origEnvTypes (Context.Z {Γ = .(StaticEnv.locEnv Δ)} {x = .l} {a = .T}) x₁) (TCompatCompatWithT ∷ TCompatWithR) TCompatCompatibility | no lNotInForbiddenRefs = 
    let
      R' = T' ∷ R
      envTypesR' = envTypesConcatMatchFound (Δ ,ₗ l ⦂ T') o (envTypesForbiddenRefsObserved Δ o (here refl) origEnvTypes) Z x₁
      TCompatWithT' : TCompat ⟷ T'
      TCompatWithT' = TCompatCompatibility TCompatCompatWithT
    in
      ⟨ R' , ⟨ envTypesR' , TCompatWithT' ∷ TCompatWithR ⟩ ⟩
  EnvTypesChangeForΔReplacement {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {T} {T'} {TCompat} {o} {.(T₁ ∷ R)} {forbiddenRefs}
    envTypes@(envTypesConcatMatchFound {R = R} {l = l₁} {T = T₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} .(Δ ,ₗ l ⦂ T) o origEnvTypes (Context.S {Γ = .(StaticEnv.locEnv Δ)} {x = .l₁} {y = .l} {a = .T₁} {b = .T} l₁Neql l₁HasTypeT₁) l₁NotInForbiddenRefs) (T₁Compat ∷ RCompat) TCompatCompatibility | no lNotInForbiddenRefs =
      let
        rest = EnvTypesChangeForΔReplacement {Σ = re μ ρ φ ψ} {Δ} {l} {T} {T'} {TCompat} {o} {R} origEnvTypes RCompat TCompatCompatibility
        R' = T₁ ∷ (proj₁ rest)
        envTypesR' = envTypesConcatMatchFound (Δ ,ₗ l ⦂ T') o (proj₁ (proj₂ rest)) (S l₁Neql l₁HasTypeT₁) l₁NotInForbiddenRefs
        restCompatibleWithTCompat = proj₂ (proj₂ rest)
      in
        ⟨ R' , ⟨ envTypesR' , T₁Compat ∷ restCompatibleWithTCompat ⟩ ⟩
-- Other cases for EnvTypes...
  EnvTypesChangeForΔReplacement {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {T} {T'} {TCompat} {o} {R} {forbiddenRefs}
    (envTypesConcatMatchNotFound {R = .R} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} .(Δ ,ₗ l ⦂ T) o origEnvTypes l₁NotInDomΔ')
    origCompat TCompatCompatibility | no lNotInForbiddenRefs =
      let
        origEnvTypesChange = EnvTypesChangeForΔReplacement origEnvTypes origCompat TCompatCompatibility
        envTypesR = envTypesConcatMatchNotFound (Δ ,ₗ l ⦂ T') o (proj₁ (proj₂ origEnvTypesChange)) (∌domPreservation l₁NotInDomΔ')
      in
        ⟨ proj₁ origEnvTypesChange , ⟨ envTypesR , proj₂ (proj₂ origEnvTypesChange) ⟩ ⟩
  
  EnvTypesChangeForΔReplacement {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o') φ ψ)} {Δ} {l} {T} {T'} {TCompat} {o} {R} {forbiddenRefs}
    (envTypesConcatMismatch {R = .R} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} {forbiddenRefs = .forbiddenRefs} .(Δ ,ₗ l ⦂ T)
    o o' oNeqO' origEnvTypes) origCompat TCompatCompatibility | no lNotInForbiddenRefs =
      let
        origEnvTypesChange = EnvTypesChangeForΔReplacement origEnvTypes origCompat TCompatCompatibility
        envTypesR = envTypesConcatMismatch (Δ ,ₗ l ⦂ T') o o' oNeqO' (proj₁ (proj₂ origEnvTypesChange))
      in
        ⟨ proj₁ origEnvTypesChange , ⟨ envTypesR , proj₂ (proj₂ origEnvTypesChange) ⟩ ⟩
  
  EnvTypesChangeForΔReplacement {.(re μ IndirectRefContext.∅ φ ψ)} {Δ} {l} {T} {T'} {TCompat} {o} {.[]} {forbiddenRefs}
    (envTypesEmpty {μ = μ} {φ = φ} {ψ = ψ} {Δ = .(Δ ,ₗ l ⦂ T)} {o = .o} {forbiddenRefs = .forbiddenRefs})
    origCompat TCompatCompatibility | no lNotInForbiddenRefs =
      ⟨ [] , ⟨ envTypesEmpty , [] ⟩ ⟩

  TCompatibleWithAllNewEnvTypes' : ∀ {Γ Σ Δ l o T₁ T₂ T₃ Ts}
                                   → (T : Type)
                                   → EnvTypes Σ (Δ ,ₗ l ⦂ T₁) o [] Ts
                                   → All (λ T' → T ⟷ T') Ts
                                   → Γ ⊢ T₁ ⇛ T₂ / T₃
                                   → ∃[ Ts' ] ((EnvTypes Σ (Δ ,ₗ l ⦂ T₃) o [] Ts') × All (λ T' → T ⟷ T') Ts')
  TCompatibleWithAllNewEnvTypes' {Γ} {Σ@.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {.(T₄ ∷ R)} T
    origMatch@(envTypesConcatMatchFound {R = R} {l = l₁} {T = T₄} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} .(Δ ,ₗ l ⦂ T₁) o origEnvTypes lookupResult lNotForbidden)
    (TCompat ∷ RCompat)
    spl with l₁ ≟ l
  ... | yes eq
    = 
      -- The last step in the proof of EnvTypes (ρ , l₁ ⦂ o) (Δ ,ₗ l ⦂ T₁) o T₁∷R is that we looked up l₁ and prepended the result to the rest of the Ts. Therefore, Ts ≡ T₁ ∷ Ts'' and Ts' = T₃ ∷ Ts''.
      -- If l₁ ≡ l, then Ts' should be T₃ ∷ R. Otherwise, we got the same thing we got when we did the lookup in regular Δ. Ts' ≡ Ts.
      let
        Ts'' = R
        Ts' = T₃ ∷ Ts''
        -- WTS: (EnvTypes Σ (Δ ,ₗ l ⦂ T₃) o Ts') × All (λ T' → T ⟷ T') Ts')
        lookupResultWithl : (StaticEnv.locEnv Δ , l ⦂ T₁) ∋ l ⦂ T₄
        lookupResultWithl = Eq.subst (λ a → StaticEnv.locEnv Δ , l ⦂ T₁ ∋ a ⦂ T₄) eq lookupResult

        T₄EqT₁ : T₄ ≡ T₁
        T₄EqT₁ = contextLookupUnique lookupResultWithl Z

        origEnvTypesWithL : EnvTypes (re μ ρ φ ψ) (Δ ,ₗ l₁ ⦂ T₁) o (l₁ ∷ []) R
        origEnvTypesWithL = Eq.subst (λ a → EnvTypes (re μ ρ φ ψ) (Δ ,ₗ a ⦂ T₁) o (l₁ ∷ []) R) (Eq.sym eq) origEnvTypes

        forbiddenOK = envTypesForbiddenRefsObserved {T' = T₃} Δ o (here refl) origEnvTypesWithL
        forbiddenOKInL₁ : EnvTypes (re μ ρ φ ψ) (Δ ,ₗ l ⦂ T₃) o (l₁ ∷ []) R
        forbiddenOKInL₁ = Eq.subst (λ a →  EnvTypes (re μ ρ φ ψ) (Δ ,ₗ a ⦂ T₃) o (l₁ ∷ []) R) eq forbiddenOK

        -- We already know that the first match in ρ for o is l, so the new Ts' has to be right.
        envTypesTs' : EnvTypes Σ (Δ ,ₗ l ⦂ T₃) o [] Ts'
        envTypesTs' = envTypesConcatMatchFound {l = l₁} (Δ ,ₗ l ⦂ T₃) o forbiddenOKInL₁
          (Eq.subst (λ a →  StaticEnv.locEnv Δ , a ⦂ T₃ ∋ l₁ ⦂ T₃) eq Z) emptyListIsEmpty

        TCompatWithT₁ : T ⟷ T₁
        TCompatWithT₁ = Eq.subst (λ a → T ⟷ a) T₄EqT₁ TCompat
        TCompatWithT₃ : T ⟷ T₃
        TCompatWithT₃ = proj₂ (splittingRespectsHeap spl TCompatWithT₁)

        Ts'Compat :  All (λ T' → T ⟷ T') Ts'
        Ts'Compat = TCompatWithT₃ ∷ RCompat
      in
        ⟨ Ts' , ⟨ envTypesTs' , Ts'Compat ⟩ ⟩
  ... | no nEq = 
      let
        -- Because we're in envTypeConcatMatchFound, l₁ ⦂ o was at the end of the context, and we found l₁ ⦂ T₄ in Δ , l ⦂ T₁.
        -- But l ≢ l₁, so we will be able to find l₁ ⦂ T₄ in Δ and use the same rule.
        -- However, there may be l's in ρ, which may result in a different R than before! Some T₁'s may be replaced with T₃'s.
        R'WithProof = EnvTypesChangeForΔReplacement {T' = T₃} {TCompat = T} origEnvTypes RCompat (λ compat → proj₂ (splittingRespectsHeap spl compat))
        R' = proj₁ R'WithProof
        Ts' = T₄ ∷ R'
        
        l₁InΔ : ((StaticEnv.locEnv Δ) , l ⦂ T₃) ∋ l₁ ⦂ T₄
        l₁InΔ = irrelevantExtensionsOK {t = T₄} {t' = T₃} (irrelevantReductionsOK lookupResult nEq) nEq

        envTypesTs' = envTypesConcatMatchFound (Δ ,ₗ l ⦂ T₃) o (proj₁ (proj₂ R'WithProof)) l₁InΔ emptyListIsEmpty

        R'Compat : All (λ T' → T ⟷ T') R'
        R'Compat = proj₂ (proj₂ R'WithProof)

        Ts'Compat : All (λ T' → T ⟷ T') Ts'
        Ts'Compat = TCompat ∷ R'Compat
      in
        ⟨ Ts' , ⟨ envTypesTs' , Ts'Compat ⟩ ⟩

  TCompatibleWithAllNewEnvTypes' {Γ} {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o) φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {Ts} T
    (envTypesConcatMatchNotFound {R = .Ts} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} .(Δ ,ₗ l ⦂ T₁) o origEnvTypes l₁NotInΔ') origCompat spl =
    let
      compatibilityWithOrigρ = TCompatibleWithAllNewEnvTypes' T origEnvTypes origCompat spl
      envTypes = envTypesConcatMatchNotFound (Δ ,ₗ l ⦂ T₃) o (proj₁ (proj₂ compatibilityWithOrigρ)) (∌domPreservation l₁NotInΔ' )
    in
      ⟨ proj₁ compatibilityWithOrigρ , ⟨ envTypes , proj₂ (proj₂ compatibilityWithOrigρ) ⟩ ⟩
      
  TCompatibleWithAllNewEnvTypes' {Γ} {.(re μ (ρ IndirectRefContext., l₁ ⦂ objRef o') φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {Ts} T
    (envTypesConcatMismatch {R = .Ts} {l = l₁} {μ = μ} {ρ = ρ} {φ = φ} {ψ = ψ} .(Δ ,ₗ l ⦂ T₁) o o' oNeqo' origEnvTypes) origCompat spl =
      let
        compatibilityWithOrigρ = TCompatibleWithAllNewEnvTypes' T origEnvTypes origCompat spl
        envTypes = envTypesConcatMismatch (Δ ,ₗ l ⦂ T₃) o o' oNeqo' (proj₁ (proj₂ compatibilityWithOrigρ))
      in
        ⟨ proj₁ compatibilityWithOrigρ , ⟨ envTypes , proj₂ (proj₂ compatibilityWithOrigρ) ⟩ ⟩
      
  TCompatibleWithAllNewEnvTypes' {Γ} {.(re μ IndirectRefContext.∅ φ ψ)} {Δ} {l} {o} {T₁} {T₂} {T₃} {.[]} T
    (envTypesEmpty {μ = μ} {φ = φ} {ψ = ψ} {Δ = .(Δ ,ₗ l ⦂ T₁)} {o = .o}) origCompat spl =
      ⟨ [] , ⟨ envTypesEmpty , [] ⟩ ⟩


  lExtensionCompatibility : ∀ {Γ Σ Δ l T₁ T₂ T₃ o}
                            → (RT : RefTypes Σ (Δ ,ₗ l ⦂ T₁) o)
                            → IsConnected Σ (Δ ,ₗ l ⦂ T₁) o RT
                            → Γ ⊢ T₁ ⇛ T₂ / T₃
                            → (RT' : RefTypes  Σ (Δ ,ₗ l ⦂ T₃) o)
                            → IsConnected Σ (Δ ,ₗ l ⦂ T₃) o RT'
  lExtensionCompatibility {Γ} {Σ} {Δ} {l} {T₁} {T₂} {T₃} {o} rt rConnected@(isConnected R oConnected oAndFieldsConnected oAndLsConnected envFieldConnected) spl rt' = ? {-
      isConnected (refTypes Σ (Δ ,ₗ l ⦂ T₃) o) oConnected oAndFieldsConnected (oAndLsConnected' rConnected) {!!}
      where
        R' = refTypes Σ (Δ ,ₗ l ⦂ T₃) o
        oAndLsConnected' : IsConnected (refTypes Σ (Δ ,ₗ l ⦂ T₁) o) → All (λ T → All (λ T' → T ⟷ T') (RefTypes.lTypes R')) (RefTypes.oTypes R')
        oAndLsConnected' (isConnected R oConnected oAndFieldsConnected oAndLsConnected envFieldConnected) with (RefTypes.oTypes R')
        oAndLsConnected' (isConnected .(refTypes Σ (Δ ,ₗ l ⦂ T₁) o) oConnected oAndFieldsConnected oAndLsConnected envFieldConnected) | [] = []
        oAndLsConnected' (isConnected .(refTypes Σ (Δ ,ₗ l ⦂ T₁) o) oConnected oAndFieldsConnected oAndLsConnected envFieldConnected) | T ∷ rest = 
          let
            -- T is a type of something that is in oTypes R'.
            TCompatWithR = Data.List.Relation.Unary.All.head oAndLsConnected          
            TOK = TCompatibleWithAllNewEnvTypes {Γ} {Σ} {Δ} {l} {o} T TCompatWithR spl
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


  splitReplacementEnvFieldOK : ∀ {Γ Σ Δ o l T₁ T₂ T₃}
                               → (RT : RefTypes  Σ (Δ ,ₗ l ⦂ T₁) o)
                               → IsConnectedEnvAndField Σ (Δ ,ₗ l ⦂ T₁) o RT
                               → Γ ⊢ T₁ ⇛ T₂ / T₃
                               → (RT' : RefTypes  Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o)
                               → IsConnectedEnvAndField  Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o RT'
  splitReplacementEnvFieldOK = {!!}

  fieldTypesExtensionEq2 :  ∀ {Σ Δ o l T₁ T₃}
                            →  refFieldTypes Σ (Δ ,ₗ l ⦂ T₁) o ≡ refFieldTypes Σ (Δ ,ₗ l ⦂ T₃) o
  fieldTypesExtensionEq2 = {!!}     

  fieldTypesExtensionEq :  ∀ {Σ Δ o l T₁ T₃}
                           →  refFieldTypes Σ (Δ ,ₗ l ⦂ T₁) o ≡ refFieldTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o
  fieldTypesExtensionEq {Σ} {se varEnv locEnv Context.∅} {o} {l} {T₁} {T₃} = {!!}
  fieldTypesExtensionEq {Σ} {se varEnv locEnv (objEnv Context., x ⦂ x₁)} {o} {l} {T₁} {T₃} = {!!}

  splitReplacementRefFieldsOK : ∀ {Σ Δ o l T₁ T₃}
                                → (RT : RefTypes Σ (Δ ,ₗ l ⦂ T₁) o)
                                → (RT' : RefTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o)
                                → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypes RT)) (RefTypes.oTypes RT)
                                → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypes RT')) (RefTypes.oTypes RT')
  splitReplacementRefFieldsOK {Σ} {Δ} {o} {l} {T₁} {T₃} RT RT' oConnectedToFieldTypes = 
    let
      -- I know that everything in fieldTypes R was previously compatible with whatever garbage was in oTypes R.
      -- Now I need to show that the same stuff, whatever it is, is compatible with [ T₁ ].
      -- Anything compatible with T₁ is compatible with T₂ and T₃, but that doesn't help...
      refFieldsEq : RefTypes.fieldTypes RT ≡ RefTypes.fieldTypes RT'
      refFieldsEq = fieldTypesExtensionEq

      P a =  All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypes a)) (RefTypes.oTypes a)
    in
      {!!} -- Eq.subst P refFieldsEq oConnectedToFieldTypes    

  splitReplacementEnvFieldsOK : ∀ {Σ Δ o l T₁ T₃ R R'}
                                → (RT : RefTypes Σ (Δ ,ₗ l ⦂ T₁) o)
                                → (RT' : RefTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o)
                                → All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT)) (RefTypes.oTypes RT)
                                → All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT')) [ T₁ ]
  splitReplacementEnvFieldsOK rt rt' oConnectedToLTypes = {!!}   

  -- Previously, all the types aliasing o were connected. Now, we've extended the context due to a split, and we need to show we still have connectivity.
  -- The idea here is that the expression in question is of type T₂, so the reference left in the heap is of type T₃.
  -- o corresponds to the object that was affected by the split, whereas o' is the reference we are interested in analyzing aliases to.
  splitReplacementOK : ∀ {Γ Σ Δ o o' l}
                       → {T₁ T₂ T₃ : Type}
                       → (RT : RefTypes Σ (Δ ,ₗ l ⦂ T₁) o')
                       → IsConnected Σ (Δ ,ₗ l ⦂ T₁) o' RT
                       → Γ ⊢ T₁ ⇛ T₂ / T₃
                       → (RT' : RefTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o')
                       → IsConnected Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o' RT'
  splitReplacementOK {Γ} {Σ} {Δ} {o} {o'} {l} {T₁} {T₂} {T₃}
    RT
    rConnected@(isConnected R oTypesConnected oConnectedToFieldTypes oConnectedToLTypes envFieldConnected)
    spl
    RT'
    with (o' ≟ o)
  ... | yes osEq =
    -- In this case, there are no O types yet, but adding o ⦂ T₁ will add one.
    let
      foo = spl
    
      Δ' = ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁)

      ot = ctxTypes (StaticEnv.objEnv Δ) o
      ot' = ctxTypes (StaticEnv.objEnv Δ') o
      oldOsConnected = isConnectedImpliesOsConnected rConnected
      TsForOs = [ T₁ ]
      TsForOsIsRight : TsForOs ≡ (RefTypes.oTypes RT')
      TsForOsIsRight = ? --  Eq.sym (ctxTypesExtension {StaticEnv.objEnv Δ} {o} {T₁})

      TsForOsConnected : IsConnectedTypeList (ctxTypes ((StaticEnv.objEnv Δ) , o ⦂ T₁) o)
      TsForOsConnected = (Eq.subst (λ a → IsConnectedTypeList a) ? {-TsForOsIsRight-} (singleElementListsAreConnected T₁))

      TsForOsConnectedToFieldTypes :  All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypes RT')) (RefTypes.oTypes RT')
      TsForOsConnectedToFieldTypes = splitReplacementRefFieldsOK {Σ} {Δ} {o} {l} ? ? ? -- (Eq.cong (λ a → ? {-(refTypes Σ (Δ ,ₗ l ⦂ T₁) a) -} ) osEq) refl oConnectedToFieldTypes

      T₁ConnectedToLTypes :  All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT')) TsForOs
      T₁ConnectedToLTypes = splitReplacementEnvFieldsOK {Σ} {Δ} {o} {l} ? ? ? -- (Eq.cong (λ a → ? {- (refTypes Σ (Δ ,ₗ l ⦂ T₁) a) -} ) osEq) refl oConnectedToLTypes

      TsForOsConnectedToLTypes :  All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT')) (RefTypes.oTypes RT')
      TsForOsConnectedToLTypes =  Eq.subst (λ a →  All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList RT')) a) TsForOsIsRight T₁ConnectedToLTypes

      envAndFieldConnected : IsConnectedEnvAndField Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o (Eq.subst (λ a → RefTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) a) osEq RT')
      envAndFieldConnected = splitReplacementEnvFieldOK {Γ} {Σ} {Δ} {o} {l} ? ? ? ? --  (Eq.subst (λ a → IsConnectedEnvAndField ? {- (refTypes Σ (Δ ,ₗ l ⦂ T₁) a) -}) osEq envFieldConnected) spl

      connectedO = isConnected RT' TsForOsConnected TsForOsConnectedToFieldTypes TsForOsConnectedToLTypes envAndFieldConnected
    in
     Eq.subst (λ a → IsConnected ? {-(refTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) a) -} ) (Eq.sym osEq) connectedO
  ... | no osNeq = 
        let
          foo = osNeq -- REMOVE
        {-
          ΔRefTypesEq1 : refTypes Σ ((Δ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₁) o' ≡ refTypes Σ (Δ ,ₗ l ⦂ T₃) o'
          ΔRefTypesEq1 = irrelevantRefTypesExtensionO {Σ} {Δ ,ₗ l ⦂ T₃} osNeq

          -- Now it suffices to show IsConnected (refTypes (Δ ,ₗ l ⦂ T₃) o').
          connectivity : IsConnected (refTypes Σ (Δ ,ₗ l ⦂ T₃) o')
          connectivity = lExtensionCompatibility {Γ} {Σ} {_} {_} {_} {_} {_} {_} rConnected spl
        -}
        in
          ?
          -- Eq.subst (λ a → IsConnected a) (Eq.sym ΔRefTypesEq1) connectivity


-- ================================ OVERALL HEAP CONSISTENCY ===========================

  data ReferenceConsistency : RuntimeEnv → StaticEnv → ObjectRef → Set where
    referencesConsistent : ∀ {Σ : RuntimeEnv}
                         → ∀ {Δ : StaticEnv}
                         → ∀ {o : ObjectRef}
                         → ∀ {RT : RefTypes Σ Δ o}
                         → IsConnected RT
                           -- TODO: add subtype constraint: C <: (refTypes Σ Δ o)
                         ---------------------------
                         → ReferenceConsistency Σ Δ o

  -- Inversion for reference consistency: connectivity
  referencesConsistentImpliesConnectivity : ∀ {Σ Δ o}
                                            → ReferenceConsistency Σ Δ o
                                            → ∃[ RT ] (IsConnected RT)

  referencesConsistentImpliesConnectivity (referencesConsistent ic) = ? -- ic

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
