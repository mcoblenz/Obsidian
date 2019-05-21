{-# OPTIONS --allow-unsolved-metas #-}


open import Silica
open import HeapProperties

open import Data.List.Membership.DecSetoid ≡-decSetoid 
open import Data.List.Relation.Unary.Any

open TypeEnvContext

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
locationsInExprAreInContext (objTy o spl) objValFL ()
locationsInExprAreInContext (boolTy b) boolFL ()
locationsInExprAreInContext ty voidFL ()

-- TODO: relax progress a little per statement of Theorem 5.1.
data Progress : Expr → Set where
  step : ∀ (Σ Σ' : RuntimeEnv)
         → ∀ (e e' : Expr)       
         → (Σ , e ⟶ Σ' , e')
         -------------
         → Progress e

  done : ∀ {e : Value}
         ---------
         → Progress (valExpr e)
         
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
    step Σ Σ (simpleExpr (loc l)) (valExpr voidVal) (SElookup ty heapLookupResult)
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l booleanSplit) = 
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = boolLookup consis lInDelta
  in
    step Σ Σ (simpleExpr (loc l)) (valExpr (boolVal (proj₁ heapLookupResult))) (SElookup ty (proj₂ heapLookupResult))
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l (unownedSplit _ _ _ _)) = 
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₁ (proj₂ heapLookupResult)
    o = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) (valExpr (objVal o)) (SElookup ty heapLookupFound)
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l (shared-shared-shared _)) =
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₁ (proj₂ heapLookupResult)
    o = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) (valExpr (objVal o)) (SElookup ty heapLookupFound)
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l (owned-shared _)) =
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₁ (proj₂ heapLookupResult)
    o = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) (valExpr (objVal o)) (SElookup ty heapLookupFound)
progress Σ cl consis@(ok {Σ} _ _ _ _ _) ty@(locTy l (states-shared _)) =
  let
    locationExistsInContext = locationsInExprAreInContext ty (locFL l) (here refl)
    lInDelta = proj₂ locationExistsInContext
    heapLookupResult = locLookup consis lInDelta
    heapLookupFound = proj₁ (proj₂ heapLookupResult)
    o = proj₁ heapLookupResult
  in
    step Σ Σ (simpleExpr (loc l)) (valExpr (objVal o)) (SElookup ty heapLookupFound)

progress Σ cl consis (objTy o split) =  done
progress Σ cl consis (boolTy b) = done
progress Σ cl consis (voidTy) = done
progress Σ cl consis (assertTyₗ {s₁ = s} {l = l} tcEq subset) = step Σ Σ (assertₗ l s) (valExpr voidVal) (SEassertₗ {Σ} l s)
progress Σ cl consis (assertTyₓ {s₁ = s} {x = x} tcEq subset) = step Σ Σ (assertₓ x s) (valExpr voidVal) (SEassertₓ {Σ} x s)
