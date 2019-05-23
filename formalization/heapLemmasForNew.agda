{-# OPTIONS --allow-unsolved-metas #-}

module HeapLemmasForNew where
  open import HeapPropertiesDefs

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
 
  heapConsistencyForNew : ∀ {Σ Σ' Δ Δ' o obj T}
                          → Σ & Δ ok
                          → Σ' ≡ record Σ {μ = RuntimeEnv.μ Σ ObjectRefContext., o ⦂ obj }
                          → Δ' ≡ Δ ,ₒ o ⦂ T
                          → Σ' & Δ' ok

  heapConsistencyForNew {Σ} {Σ'} {Δ} {Δ'} {o} {obj} {T}
    consis@(ok {Σ} Δ voidLookup boolLookup objLookup referenceConsistencyFunc)
    refl
    refl = 
    ok {Σ'} Δ' voidLookup boolLookup objLookup' referenceConsistency'
    where
      objLookup' : (∀ (l : IndirectRef)
                   → ∀ (T : Tc)
                   → (StaticEnv.locEnv Δ') ∋ l ⦂ (contractType T)
                   → ∃[ o ] (RuntimeEnv.ρ Σ' IndirectRefContext.∋ l ⦂ objVal o × (o ObjectRefContext.∈dom (RuntimeEnv.μ Σ')))
                   )
      objLookup' l t lInΔ =
        let
          oldLookupResult = objLookup l t lInΔ
          o' = proj₁ oldLookupResult
        in
          ⟨ o' , ⟨ proj₁ (proj₂ oldLookupResult) , ObjectRefContext.∈domExtension (proj₂ (proj₂ oldLookupResult)) ⟩ ⟩

      referenceConsistency' : (∀ o → o ObjectRefContext.∈dom (RuntimeEnv.μ Σ') → ReferenceConsistency Σ' Δ' o)
      referenceConsistency' o' o'Inμ' with o' ≟ o
      ... | yes o'≡o = 
        let
          origRefConsistency = referencesConsistentImpliesConnectivity(referenceConsistencyFunc o' {!!})
          RT = proj₁ origRefConsistency
          RT' = {!record {oTypesList = [ T ] ;
                         oTypes = refl ;
                         envTypesList = (RefTypes.envTypesList RT)}!}
        in
          referencesConsistent ⟨ RT' , {!!} ⟩
          
      ... | no o'≢o =      
        let
          origRefConsistency = referenceConsistencyFunc o'
        in
          referencesConsistent {!!}
