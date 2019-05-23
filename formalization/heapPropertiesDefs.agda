module HeapPropertiesDefs where
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
                    
                    -- ============= DEFINITIONS OF HEAP CONSISTENCY ===============

  ctxTypes : TypeEnv → ObjectRef → List Type
  ctxTypes ∅ _ = []
  ctxTypes (Δ , o' ⦂ T) o with o ≟ o'
  ...                    | yes eq = [ T ]
  ...                    | no nEq = ctxTypes Δ o

  envTypesHelper : IndirectRefEnv → TypeEnv → ObjectRef → List Type
  envTypesHelper IndirectRefContext.∅  Δ o = []


  envTypesHelper (IndirectRefContext._,_⦂_ ρ l (objVal o')) Δ o with (o' ≟ o) | (TypeEnvContext.lookup Δ l)
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
                               → EnvTypes (re μ (ρ IndirectRefContext., l ⦂ (objVal o)) φ ψ) Δ o forbiddenRefs (T ∷ R)

    envTypesConcatMatchNotFound : ∀ {R l μ ρ φ ψ forbiddenRefs}
                                  → (Δ : StaticEnv)
                                  → (o : ObjectRef)
                                  → EnvTypes (re μ ρ φ ψ) Δ o forbiddenRefs R
                                  → l ∉dom (StaticEnv.locEnv Δ)
                                  → EnvTypes (re μ (ρ IndirectRefContext., l ⦂ (objVal o)) φ ψ) Δ o forbiddenRefs R
 
    envTypesConcatMismatch : ∀ {R l μ ρ φ ψ forbiddenRefs}
                             → (Δ : StaticEnv)
                             → (o o' : ObjectRef)
                             → o ≢ o'
                             → EnvTypes (re μ ρ φ ψ) Δ o forbiddenRefs R -- Mismatch in ρ, so keep looking in the rest of ρ
                             → EnvTypes (re μ (ρ IndirectRefContext., l ⦂ (objVal o')) φ ψ) Δ o forbiddenRefs R

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

  data IsConnectedEnvAndField : List Type → List Type → Set where
    envTypesConnected : {envTypesList : List Type}
                        → {fieldTypesList : List Type}
                        → IsConnectedTypeList envTypesList
                        → IsConnectedTypeList fieldTypesList
                        → All (λ T → All (λ T' → T ⟷ T') fieldTypesList) envTypesList  -- all of the l types are connected to all of the field types
                        ----------------------------------------------
                        → IsConnectedEnvAndField envTypesList fieldTypesList  


  envFieldInversion1 : ∀ {envTypesList fieldTypesList}
                       → IsConnectedEnvAndField  envTypesList fieldTypesList
                       → IsConnectedTypeList envTypesList
  envFieldInversion1 (envTypesConnected env _ _) = env      

  envFieldInversion2 :  ∀ {envTypesList fieldTypesList}
                        → IsConnectedEnvAndField envTypesList fieldTypesList
                        → IsConnectedTypeList fieldTypesList
  envFieldInversion2 (envTypesConnected _ f _) = f

  envFieldInversion3 :  ∀ {envTypesList fieldTypesList}
                        → IsConnectedEnvAndField envTypesList fieldTypesList
                        → All (λ T → All (λ T' → T ⟷ T') fieldTypesList) envTypesList
  envFieldInversion3 (envTypesConnected _ _ envField) = envField
  

  data IsConnected (Σ : RuntimeEnv) (Δ : StaticEnv) (o : ObjectRef) : RefTypes Σ Δ o → Set where              
    isConnected : (R : RefTypes Σ Δ o)
                  → IsConnectedTypeList (RefTypes.oTypesList R)
                  → All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList R)) (RefTypes.oTypesList R) -- all of the o types are connected to all of the field types
                  → All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList R)) (RefTypes.oTypesList R) -- all of the o types are connected to all of the l types
                  → IsConnectedEnvAndField (RefTypes.envTypesList R) (RefTypes.fieldTypesList R)
                  ----------------------------------------------
                  → IsConnected Σ Δ o R




  refFieldTypesHelper : ObjectRefEnv → StaticEnv → ObjectRef → List Type
  refFieldTypesHelper ObjectRefContext.∅ Δ o = []
  refFieldTypesHelper (ObjectRefContext._,_⦂_ μ o' obj) Δ o = refFieldTypesHelper μ Δ o  -- TODO; this is bogus!

  refFieldTypes : RuntimeEnv → StaticEnv → ObjectRef → List Type
  refFieldTypes Σ Δ o = refFieldTypesHelper (RuntimeEnv.μ Σ) Δ o

  -- Inversion for IsConnected
  isConnectedInversion : ∀ {Σ Δ o R}
                         → IsConnected Σ Δ o R
                         → (IsConnectedTypeList (RefTypes.oTypesList R) ×
                                                 All (λ T → All (λ T' → T ⟷ T') (RefTypes.fieldTypesList R)) (RefTypes.oTypesList R) ×
                                                 All (λ T → All (λ T' → T ⟷ T') (RefTypes.envTypesList R)) (RefTypes.oTypesList R) ×
                                                 IsConnectedEnvAndField (RefTypes.envTypesList R) (RefTypes.fieldTypesList R))

  isConnectedInversion {Σ} {Δ} {o} {R} (isConnected R x x₁ x₂ x₃) = ⟨ x , ⟨ x₁ , ⟨ x₂ , x₃ ⟩ ⟩ ⟩

{-
  oTypesIrrelevantForIsConnectedEnvAndField : ∀ {Σ Δ o o' T R R' μ'}
                                              → IsConnectedEnvAndField Σ Δ o R
                                              → IsConnectedEnvAndField (re μ' (RuntimeEnv.ρ Σ) (RuntimeEnv.φ Σ) (RuntimeEnv.ψ Σ)) (Δ ,ₒ o' ⦂ T) o R'
-}
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

  referencesConsistentImpliesConnectivity (referencesConsistent ic) = ic

  ------------ Global Consistency -----------
  -- I'm going to need the fact that if an expression typechecks, and I find a location in it, then the location can be looked
  -- up in the runtime environment. But every location in the expression has to also be in the typing context, so I can state this
  -- without talking about expressions at all.
  data _&_ok : RuntimeEnv → StaticEnv → Set where
    ok : ∀ {Σ : RuntimeEnv}
         → ∀ (Δ : StaticEnv)
         → (∀ (l : IndirectRef) → ((StaticEnv.locEnv Δ) ∋ l ⦂ base Void → (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ voidVal)))
         → (∀ (l : IndirectRef) → ((StaticEnv.locEnv Δ) ∋ l ⦂ base Boolean → ∃[ b ] (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ boolVal b)))
         → (∀ (l : IndirectRef)
           → ∀ (T : Tc)
           → (StaticEnv.locEnv Δ) ∋ l ⦂ (contractType T)         -- If a location is in Δ and has contract reference type...
           → ∃[ o ] (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objVal o × (o ObjectRefContext.∈dom (RuntimeEnv.μ Σ))) -- then location can be looked up in Σ...
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
              → ∃[ o ] (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ objVal o × (o ObjectRefContext.∈dom (RuntimeEnv.μ Σ)))

  locLookup (ok Δ _ _ lContainment rc) lInDelta@(Z {Δ'} {x} {contractType a}) = lContainment x a lInDelta
  locLookup (ok Δ _ _ lContainment rc) lInDelta@(S {Δ'} {x} {y} {contractType a} {b} nEq xInRestOfDelta) = lContainment x a lInDelta

  voidLookup : ∀ {Σ : RuntimeEnv}
              → ∀ {Δ : StaticEnv}
              → ∀ {l : IndirectRef}
              → Σ & Δ ok
              → (StaticEnv.locEnv Δ) ∋ l ⦂ base Void
              → (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ voidVal)
  voidLookup (ok Δ voidContainment _ _ _) voidType@(Z {Δ'} {l} {a}) = voidContainment l voidType
  voidLookup (ok Δ voidContainment _ _ _) voidType@(S {Δ'} {l} {y} {a} {b} nEq lInRestOfDelta) = voidContainment l voidType

  boolLookup : ∀ {Σ : RuntimeEnv}
              → ∀ {Δ : StaticEnv}
              → ∀ {l : IndirectRef}
              → Σ & Δ ok
              → (StaticEnv.locEnv Δ) ∋ l ⦂ base Boolean
              → ∃[ b ] (RuntimeEnv.ρ Σ IndirectRefContext.∋ l ⦂ boolVal b)
  boolLookup (ok Δ _ boolContainment _ _) boolType@(Z {Δ'} {l} {a}) = boolContainment l boolType
  boolLookup (ok Δ _ boolContainment _ _) boolType@(S {Δ'} {l} {y} {a} {b} nEq lInRestOfDelta) = boolContainment l boolType 


 
  ctxTypesIrrelevantExtension : ∀ {Δ x x' T}
                                → x ≢ x'
                                → ctxTypes (Δ , x' ⦂ T) x ≡ ctxTypes Δ x
  ctxTypesIrrelevantExtension {Context.∅} {x} {x'} {T} x≢x' with x ≟ x'
  ctxTypesIrrelevantExtension {Context.∅} {x} {.x} {T} x≢x' | yes refl = ⊥-elim (x≢x' refl)
  ... | no _ = refl
  ctxTypesIrrelevantExtension {Δ Context., x₁ ⦂ x₂} {x} {x'} {T} x≢x' with x ≟ x'
  ... | yes x≡x' = ⊥-elim (x≢x' x≡x')
  ... | no _ = refl
