

open import Silica
open import HeapProperties
import Context

open TypeEnvContext

open import Data.Nat
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Data.Empty
import Relation.Binary.PropositionalEquality as Eq

open import ConsistencyForSimpleExprs

data Preservation : Expr → Set where
  pres : ∀ {e e' : Expr}
       → ∀ {T T' : Type}
       → ∀ {Δ Δ'' Δ''' : StaticEnv}
       → ∀ {Σ Σ' : RuntimeEnv }
       → Δ ⊢ e ⦂ T ⊣ Δ'' 
       → Σ & Δ ok        
       -- TODO: hdref(e)
       → Σ , e ⟶ Σ' , e'
       → (Δ' : StaticEnv)
       → (Δ' ⊢ e' ⦂ T' ⊣ Δ''') 
       → (Σ' & Δ' ok)  -- 
       → (Δ''' <* Δ'')
       -----------------
       → Preservation e

splitIdempotent : ∀ {Γ T₁ T₂ T₃}
                    → Γ ⊢ T₁ ⇛ T₂ / T₃
                    → Γ ⊢ T₂ ⇛ T₂ / T₃ 
splitIdempotent {Γ} {.(base Void)} {.(base Void)} {.(base Void)} voidSplit = voidSplit
splitIdempotent {Γ} {.(base Boolean)} {.(base Boolean)} {.(base Boolean)} booleanSplit = booleanSplit
splitIdempotent {Γ} {.(contractType _)} {.(contractType _)} {.(contractType _)} (unownedSplit namesEq1 namesEq2 permsEq permUnowned) =
  unownedSplit refl (Eq.trans (Eq.sym namesEq1) namesEq2) refl permUnowned
splitIdempotent {Γ} {.(contractType _)} {.(contractType _)} {.(contractType _)} (shared-shared-shared x) =
  shared-shared-shared x
splitIdempotent {Γ} {.(contractType (tc _ Owned))} {.(contractType (tc _ Shared))} {.(contractType (tc _ Shared))} (owned-shared x) =
  shared-shared-shared refl
splitIdempotent {Γ} {.(contractType (record { contractName = _ ; perm = S _ }))}
                    {.(contractType (record { contractName = _ ; perm = Shared }))}
                    {.(contractType (record { contractName = _ ; perm = Shared }))}
                    (states-shared x) =
                    shared-shared-shared refl


preservation : ∀ {e e' : Expr}
               → {Γ : ContractEnv.ctx}
               → ∀ {T : Type}
               → ∀ {Δ Δ'' : StaticEnv}
               → ∀ {Σ Σ' : RuntimeEnv}
               → Δ ⊢ e ⦂ T ⊣ Δ'' 
               → Σ & Δ ok   
               → Σ , e ⟶ Σ' , e'
               -----------------------
               → Preservation e

-- Proof proceeds by induction on the dynamic semantics.

preservation ty@(locTy {Γ} {Δ₀} l voidSplit) consis st@(SElookup {l = l} {v = voidVal} _ lookupL) =
  let
    Δ = (Δ₀ ,ₗ l ⦂ base Void)
    Δ' = Δ
    e'TypingJudgment = voidTy {Γ} {Δ = Δ'}
  in 
    pres ty consis st Δ' e'TypingJudgment consis <*-refl
preservation ty@(locTy {Γ} {Δ₀} l booleanSplit) consis st@(SElookup {l = l} {v = boolVal b} _ lookupL) = 
 let
    Δ = (Δ₀ ,ₗ l ⦂ base Boolean)
    Δ' = Δ
    e'TypingJudgment = boolTy {Γ} {Δ = Δ'} b
  in 
    pres {Δ = Δ} ty consis st Δ' e'TypingJudgment consis <*-refl
-- The code duplication below is very sad, but I haven't figured out a way that doesn't require refactoring the split judgment to be two-level.
preservation ty@(locTy {Γ} {Δ₀} {T₁ = contractType t₁} {T₂ = contractType t₂} {T₃ = contractType t₃} l spl@(unownedSplit _ _ _ _))
                       consis@(ok Δ voidLookup boolLookup objLookup refConsistencyFunc)
                       st@(SElookup {Σ} {Δ = Δ'} {l = l} {v = objVal o} _ lookupL) = 
  let
    spl' = splitIdempotent spl
    T₁ = contractType t₁
    T₂ = contractType t₂
    T₃ = contractType t₃
    consis' = locEvalPreservesConsistency consis ty lookupL

    Δ'' = Δ₀ ,ₗ l ⦂ T₃ -- This is the result of checking e.
    Δ' = (Δ₀ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₂
    e'TypingJudgment : Δ' ⊢ valExpr (objVal o) ⦂ T₂ ⊣ ((Δ₀ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₃)
    e'TypingJudgment =  objTy {Γ} {Δ''} {T₁ = T₂} {T₂ = T₂} {T₃ = T₃} o spl' -- -- Yes, I do mean T₁ = T₂ here.
  in
    pres {_} {valExpr (objVal o)} {_} {T₂} {Δ = Δ} {Δ'' = Δ''} {_} ty consis st Δ' e'TypingJudgment consis' <*-o-extension
    
preservation ty@(locTy {Γ} {Δ₀} {T₁ = contractType t₁} {T₂ = contractType t₂} {T₃ = contractType t₃} l spl@(shared-shared-shared _))
                       consis@(ok Δ voidLookup boolLookup objLookup refConsistencyFunc)
                       st@(SElookup {Σ} {l = l} {v = objVal o} _ lookupL) = 
   let
    spl' = splitIdempotent spl
    T₂ = contractType t₂
    T₃ = contractType t₃
    Δ'' = Δ₀ ,ₗ l ⦂ T₃ -- This is the result of checking e.
    Δ' = (Δ₀ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₂

    e'TypingJudgment = objTy {Γ} {Δ = Δ''} {T₁ = contractType t₂} {T₂ = contractType t₂} {T₃ = contractType t₃} o spl'
   
    consis' = locEvalPreservesConsistency consis ty lookupL
  in
    pres {_} {_} {_} {_} {Δ = Δ} {Δ'' = Δ''} {_} {_} {_} ty consis st Δ' e'TypingJudgment consis' <*-o-extension

preservation ty@(locTy {Γ} {Δ₀} {T₁ = contractType t₁} {T₂ = contractType t₂} {T₃ = contractType t₃} l spl@(owned-shared _))
                       consis@(ok Δ voidLookup boolLookup objLookup refConsistencyFunc)
                       st@(SElookup {Σ} {l = l} {v = objVal o} _ lookupL) = 
   let
     T₂ = contractType t₂
     T₃ = contractType t₃
     Δ'' = Δ₀ ,ₗ l ⦂ T₃ -- This is the result of checking e.
     Δ' = (Δ₀ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₂
     spl' = splitIdempotent spl
     e'TypingJudgment = objTy {Γ} {Δ = Δ''} {T₁ = contractType t₂} {T₂ = contractType t₂} {T₃ = contractType t₃} o spl'
   
     consis' = locEvalPreservesConsistency consis ty lookupL
  in
    pres {_} {_} {_} {_} {Δ = Δ} {Δ'' = Δ''} {_} {_} {_} ty consis st Δ' e'TypingJudgment consis' <*-o-extension


preservation ty@(locTy {Γ} {Δ₀} {T₁ = contractType t₁} {T₂ = contractType t₂} {T₃ = contractType t₃} l spl@(states-shared _))
                       consis@(ok Δ voidLookup boolLookup objLookup refConsistencyFunc)
                       st@(SElookup {Σ} {l = l} {v = objVal o} _ lookupL) = 
   let
     T₂ = contractType t₂
     T₃ = contractType t₃
     Δ'' = Δ₀ ,ₗ l ⦂ T₃ -- This is the result of checking e.
     Δ' = (Δ₀ ,ₗ l ⦂ T₃) ,ₒ o ⦂ T₂
     spl' = splitIdempotent spl
     e'TypingJudgment = objTy {Γ} {Δ = Δ''} {T₁ = contractType t₂} {T₂ = contractType t₂} {T₃ = contractType t₃} o spl'
   
     consis' = locEvalPreservesConsistency consis ty lookupL
  in
    pres {_} {_} {_} {_} {Δ = Δ} {Δ'' = Δ''} {_} {_} {_} ty consis st Δ' e'TypingJudgment consis' <*-o-extension

preservation {T = contractType t₂}
  (locTy {Γ} {Δ₀} {contractType t₁} {.(contractType t₂)} {contractType t₃} l spl)
  (ok .(Δ₀ ,ₗ l ⦂ contractType t₁) voidLookup boolLookup objLookup refConsistencyFunc)
  (SElookup {Σ} {l = l} {boolVal b} _ lookupL) =
    -- The context says the location refers to a contract, but also a boolean value?? Inconsistent.
      ⊥-elim (lookupNeq lookupUnique)
    where
      objLookupResult = objLookup l t₁ Z
      o = proj₁ objLookupResult
      lHasObjType = proj₁ (proj₂ objLookupResult)
      lookupUnique = IndirectRefContext.contextLookupUnique lHasObjType lookupL
      lookupNeq : (objVal o) ≢ boolVal b
      lookupNeq ()

preservation {T = contractType t₂}
  (locTy {Γ} {Δ₀} {contractType t₁} {.(contractType t₂)} {contractType t₃} l spl)
  (ok .(Δ₀ ,ₗ l ⦂ contractType t₁) voidLookup boolLookup objLookup refConsistencyFunc)
  (SElookup {Σ} {l = l} {voidVal} _ lookupL) =
    -- The context says the location refers to a contract, but also a boolean value?? Inconsistent.
      ⊥-elim (lookupNeq lookupUnique)
    where
      objLookupResult = objLookup l t₁ Z
      o = proj₁ objLookupResult
      lHasObjType = proj₁ (proj₂ objLookupResult)
      lookupUnique = IndirectRefContext.contextLookupUnique lHasObjType lookupL
      lookupNeq : (objVal o) ≢ voidVal
      lookupNeq ()

preservation
  (locTy {Γ} {Δ₀} {.(base Void)} l voidSplit)
  (ok .(Δ₀ ,ₗ l ⦂ base Void) voidLookup boolLookup objLookup refConsistencyFunc)
  (SElookup {Σ} {Δ} {T = T} {l = l} {boolVal b} lookupLType lookupL) = 
            ⊥-elim (lookupNeq lookupUnique)
    where
      lLookupResult = voidLookup l Z
      lookupUnique = IndirectRefContext.contextLookupUnique lLookupResult lookupL
      lookupNeq : voidVal ≢ boolVal b
      lookupNeq ()


preservation
  (locTy {Γ} {Δ₀} {.(base Void)} l voidSplit)
  (ok .(Δ₀ ,ₗ l ⦂ base Void) voidLookup boolLookup objLookup refConsistencyFunc)
  (SElookup {Σ} {Δ} {T = T} {l = l} {objVal o} lookupLType lookupL) = 
            ⊥-elim (lookupNeq lookupUnique)
    where
      lLookupResult = voidLookup l Z
      lookupUnique = IndirectRefContext.contextLookupUnique lLookupResult lookupL
      lookupNeq : voidVal ≢ objVal o
      lookupNeq ()

preservation
  (locTy {Γ} {Δ₀} {.(base Boolean)} l booleanSplit)
  (ok .(Δ₀ ,ₗ l ⦂ base Boolean) voidLookup boolLookup objLookup refConsistencyFunc)
  (SElookup {Σ} {Δ} {T = T} {l = l} {voidVal} lookupLType lookupL) = 
            ⊥-elim (lookupNeq lookupUnique)
    where
      lLookupResult = boolLookup l Z
      lookupUnique = IndirectRefContext.contextLookupUnique (proj₂ lLookupResult) lookupL
      lookupNeq : boolVal (proj₁ lLookupResult) ≢ voidVal
      lookupNeq ()
      
preservation
  (locTy {Γ} {Δ₀} {.(base Boolean)} l booleanSplit)
  (ok .(Δ₀ ,ₗ l ⦂ base Boolean) voidLookup boolLookup objLookup refConsistencyFunc)
  (SElookup {Σ} {Δ} {T = T} {l = l} {objVal o} lookupLType lookupL) = 
            ⊥-elim (lookupNeq lookupUnique)
    where
      lLookupResult = boolLookup l Z
      lookupUnique = IndirectRefContext.contextLookupUnique (proj₂ lLookupResult) lookupL     
      lookupNeq : boolVal (proj₁ lLookupResult) ≢ objVal o
      lookupNeq ()

preservation {Γ = Γ} {Δ = Δ} {Δ'' = Δ''} ty@(assertTyₓ _ _) consis st@(SEassertₓ x s) =
  pres ty consis st Δ (voidTy {Γ = Γ} {Δ = Δ}) consis <*-refl
preservation {Γ = Γ} {Δ = Δ} {Δ'' = Δ''} ty@(assertTyₗ _ _) consis st@(SEassertₗ x s) =
  pres ty consis st Δ (voidTy {Γ = Γ} {Δ = Δ}) consis <*-refl

preservation  {Γ = Γ} {Δ = Δ} {Δ'' = Δ''}
  ty@(newTy {Δ = Δ} {Δ' = Δintermed} {Δ'' = Δ''} {C = C} {st = newState} {x₁ = loc l₁} {x₂ = loc l₂}
    newStateOK
    locTy₁@(locTy l₁ spl₁)
    locTy₂@(locTy l₂ spl₂)
    CInΓ
    refl)
  consis
  step@(SEnew {Σ} {o = o} oFresh refl refl) =
    pres ty consis step Δ' newTypeJudgment {!!} {!!} -- newConsis {!<*-o-extension!}
    where
      T = contractType (tc C (S [ newState ]) )
      Δ' = Δ'' ,ₒ o ⦂ T
      -- We have two simple expressions that each have type judgments ty₁ and ty₂.
      spl = unownedSplit {Γ} refl refl refl refl
      newTypeJudgment = objTy o spl
      newConsis₁ = locEvalPreservesConsistency consis locTy₁ {!!}
      newConsis = heapConsistencyForNew consis refl refl

