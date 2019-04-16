open import Silica
open import HeapProperties
import Context

open TypeEnvContext

open import Data.Nat
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Data.Empty



data Preservation : Expr → Set where
  pres : ∀ {e e' : Expr}
       → ∀ {T T' : Type}
       → ∀ {Δ Δ'' Δ''' : StaticEnv}
       → ∀ {Σ Σ' : RuntimeEnv }
       → Δ ⊢ e ⦂ T ⊣ Δ'' 
       → Σ & Δ ok        
       -- TODO: hdref(e)
       → Σ , e ⟶ Σ' , e'
       → ∀ Δ'
       → (Δ' ⊢ e' ⦂ T' ⊣ Δ''') 
       → (Σ' & Δ' ok)  -- 
       → (Δ''' <* Δ'')
       -----------------
       → Preservation e

preservation : ∀ {e e' : Expr}
               → ∀ {T : Type}
               → ∀ {Δ Δ'' : StaticEnv}
               → ∀ {Σ Σ' : RuntimeEnv}
               → Δ ⊢ e ⦂ T ⊣ Δ'' 
               → Σ & Δ ok   
               → Σ , e ⟶ Σ' , e'
               -----------------------
               → Preservation e

-- Proof proceeds by induction on the dynamic semantics.

preservation ty@(locTy {Γ} {Δ₀} l voidSplit) consis st@(SElookup {l = l} {v = voidExpr} _ lookupL) =
  let
    Δ = (Δ₀ ,ₗ l ⦂ base Void)
    Δ' = Δ
    e'TypingJudgment = voidTy {Γ} {Δ = Δ'}
  in 
    pres ty consis st Δ' e'TypingJudgment consis <*-refl
preservation ty@(locTy {Γ} {Δ₀} l booleanSplit) consis st@(SElookup {l = l} {v = boolExpr b} _ lookupL) = 
 let
    Δ = (Δ₀ ,ₗ l ⦂ base Boolean)
    Δ' = Δ
    e'TypingJudgment = boolTy {Γ} {Δ = Δ'} b
  in 
    pres {Δ = Δ} ty consis st Δ' e'TypingJudgment consis <*-refl
preservation ty@(locTy {Γ} {Δ₀} {T₁ = contractType t₁} {T₃ = contractType t₃} l spl@(unownedSplit _ _ _ _))
                       consis@(ok Δ voidLookup boolLookup objLookup refConsistencyFunc)
                       st@(SElookup {Σ} {l = l} {v = objRef o} _ lookupL) = 
  let
    e'TypingJudgment = objTy {Γ} {Δ = Δ''} o spl
   
    consis' = ok Δ' voidLookup' boolLookup' objLookup' refConsistency'
  in
    pres {Δ = Δ} {Δ'' = Δ''} ty consis st Δ' e'TypingJudgment consis' {!!}
  where
    splT = splitType spl
    Δ'' = Δ₀ ,ₗ l ⦂ (SplitType.t₃ splT)
    Δ' = Δ'' ,ₒ o ⦂ (SplitType.t₁ splT)
    --Δ = Δ₀ ,ₗ l ⦂ (SplitType.t₁ splT)
    -- Show that if you look up l in the new context, you get the same type as before.
    voidLookup' : (∀ (l' : IndirectRef)
                  → ((StaticEnv.locEnv Δ') ∋ l' ⦂ base Void
                  → (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l' ≡ just voidExpr)))
    voidLookup' l' l'InΔ' with l' Data.Nat.≟ l
    voidLookup' l' (Context.S l'NeqL l'VoidType) | yes eq = ⊥-elim (l'NeqL eq)
    voidLookup' l' l'InΔ' | no nEq = voidLookup l' (S nEq (irrelevantReductionsOK l'InΔ' nEq))

    boolLookup' : (∀ (l' : IndirectRef)
                  → ((StaticEnv.locEnv Δ') ∋ l' ⦂ base Boolean
                  → ∃[ b ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l' ≡ just (boolExpr b))))
    boolLookup' l' l'InΔ' with l' Data.Nat.≟ l
    boolLookup' l' (Context.S l'NeqL l'BoolType) | yes eq = ⊥-elim (l'NeqL eq)
    boolLookup' l' l'InΔ' | no nEq = boolLookup l' (S nEq (irrelevantReductionsOK l'InΔ' nEq))
    
    objLookup' : (∀ (l' : IndirectRef) → ∀ (T : Tc)
                  → ((StaticEnv.locEnv Δ') ∋ l' ⦂ (contractType T)
                  → ∃[ o ] (IndirectRefContext.lookup (RuntimeEnv.ρ Σ) l' ≡ just (objRef o))))
    objLookup' l' _ l'InΔ' with l' Data.Nat.≟ l
    objLookup' l' _ (Context.S l'NeqL l'ObjType) | yes eq = ⊥-elim (l'NeqL eq)
    objLookup' l' t l'InΔ'@(Z {a = contractType t₃}) | yes eq = objLookup l' t₁ Z
    objLookup' l' t l'InΔ' | no nEq = objLookup l' t (S nEq (irrelevantReductionsOK l'InΔ' nEq))

    -- Show that all location-based aliases from the previous environment are compatible with the new alias.
    refConsistency' = λ o → λ oInμ →
      let
        origRefConsistency = refConsistencyFunc o oInμ
        origConnected = referencesConsistentImpliesConnectivity {Σ} {Δ} origRefConsistency
      in 
        referencesConsistent (splitReplacementOK origConnected spl)


preservation ty consis st@(SEassertₓ x s) = pres ty consis st {!!} {!!} {!!} {!!}
preservation ty consis st@(SEassertₗ l s) = pres ty consis st {!!} {!!} {!!} {!!}
