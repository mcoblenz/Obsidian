-- Adapted from Wadler: https://plfa.github.io/Lambda/


module Context (A : Set) where

  open import Data.Nat
  open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; sym)
  open import Data.Maybe
  open import Data.Product using (_×_; proj₁; proj₂; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
  open import Relation.Nullary.Decidable
  open import Relation.Nullary using (Dec; yes; no)
  open import Data.Empty


  infixl 5  _,_⦂_
  
  -- Internal type of contexts
  data ctx : Set where
    ∅     : ctx
    _,_⦂_  : ctx → ℕ → A → ctx

  infix  4  _∋_⦂_

  data _∋_⦂_ : ctx → ℕ → A → Set where

    Z : ∀ {Γ : ctx}
      → ∀ {x : ℕ}
      → ∀ {a : A}
        ------------------
      → Γ , x ⦂ a ∋ x ⦂ a
  
    S : ∀ {Γ x y a b}
      → x ≢ y
      → Γ ∋ x ⦂ a
        ------------------
      → Γ , y ⦂ b ∋ x ⦂ a

  lookup : ctx → ℕ → Maybe A
  lookup ∅ _ = nothing
  lookup (Γ , x ⦂ t) y with compare x y
  ...                       | equal _ = just t
  ...                       | _ = lookup Γ x

  data _∈dom_ : ℕ → ctx → Set where
    inDom : ∀ {x Γ a}
          → Γ ∋ x ⦂ a
          -------------
          → x ∈dom Γ

  data _∌dom_ : ℕ → ctx → Set where
    notInEmpty : ∀ {x}
               ----------
               → x ∌dom ∅

    notInNonempty : ∀ {x x' Γ T}
                    → x ≢ x'
                    → x ∌dom Γ
                    --------------------
                    → x ∌dom (Γ , x' ⦂ T)

  ∌domPreservation : ∀ {x x' Γ T T'}
                     → x ∌dom (Γ , x' ⦂ T)
                     ---------------------
                     → x ∌dom (Γ , x' ⦂ T')
  ∌domPreservation {x} {x'} {Γ} {T} {T'} (notInNonempty xNeqX' xNotInDom) = notInNonempty xNeqX' xNotInDom

                     

-- Removing elements from a context
  _#_ : ctx → ℕ → ctx
  ∅ # x = ∅
  (Γ , x' ⦂ T) # x with compare x x'
  ... | equal _  = Γ
  ... | _ = (Γ # x) , x' ⦂ T

  
  irrelevantExtensionsOK : ∀ {Γ : ctx}
                           → ∀ {x y t t'}
                           → Γ ∋ x ⦂ t
                           → x ≢ y
                           → Γ , y ⦂ t' ∋ x ⦂ t
  irrelevantExtensionsOK {Γ} {x} {y} {t} cont@(Z {Γ₀} {x} {t}) neq = S neq cont
  irrelevantExtensionsOK (S neq' rest) neq = S neq (irrelevantExtensionsOK rest neq')

  irrelevantReductionsOK :  ∀ {Γ : ctx}
                           → ∀ {x y t t'}
                           → Γ , x ⦂ t ∋ y ⦂ t'
                           → y ≢ x
                           → Γ ∋ y ⦂ t'
                           -- ⊥-elim (!neq (Relation.Binary.PropositionalEquality.sym x x))
  irrelevantReductionsOK {Γ} {x} {y} {t} {t'} z@(Z {Γ} {x} {t}) neq =
    let
      s : x ≡ x
      s = refl
      bot = neq s
    in
      Data.Empty.⊥-elim bot
      
  irrelevantReductionsOK {Γ} {x} {y} {t} {t'} (S x₁ qq) neq = qq                 

  irrelevantReductionsInValuesOK :  ∀ {Γ : ctx}
                                 → ∀ {x y t t'}
                                 → Γ , x ⦂ t ∋ y ⦂ t'
                                 → t ≢ t'
                                 → Γ ∋ y ⦂ t'
  irrelevantReductionsInValuesOK {Γ} {x} {.x} {t} {.t} Z tNeqt' = ⊥-elim (tNeqt' refl)
  irrelevantReductionsInValuesOK {Γ} {x} {y} {t} {t'} (S yNeqx yt'InΓ') tNeqt' = yt'InΓ'

  contextLookupUnique : ∀ {Γ : ctx}
                        → ∀ {x t t'}
                        → Γ ∋ x ⦂ t
                        → Γ ∋ x ⦂ t'
                        → t ≡ t'
  contextLookupUnique z1@Z z2@Z = refl
  contextLookupUnique z1@Z s2@(S {Γ} {x} {y} {a} {b} neq xHasTypeT') = Data.Empty.⊥-elim (neq refl)
  contextLookupUnique (S neq xHasTypeT) Z = Data.Empty.⊥-elim (neq refl)
  contextLookupUnique (S x₁ xHasTypeT) (S x₂ xHasTypeT') = contextLookupUnique xHasTypeT xHasTypeT'

  contextLookupNeq : ∀ {Γ : ctx}
                     → ∀ {x x' t t'}
                     → Γ , x ⦂ t ∋ x' ⦂ t'
                     → t ≢ t'
                     → x ≢ x'
  contextLookupNeq Z tNeq = Data.Empty.⊥-elim (tNeq refl)
  contextLookupNeq (S xNeq x'InΓ) tNeq = λ xEq → xNeq (sym xEq)

  lookupWeakening : ∀ {Γ : ctx}
                    → ∀ {x x' t t'}
                    → Γ ∋ x ⦂ t
                    → ∃[ T ] ((Γ , x' ⦂ t') ∋ x ⦂ T)
                    
  lookupWeakening {Γ} {x} {x'} {t} {t'} Γcontainment with x ≟ x'
  ...                                                 | yes refl = ⟨ t' , Z {Γ = Γ} {x = x'} {a = t'} ⟩
  ...                                                 | no neq =  ⟨ t , S neq Γcontainment ⟩

  ∌dom-≢ : {Γ : ctx}
           → ∀ {x x' t}
           → x ∌dom (Γ , x' ⦂ t)
           → x ≢ x'

  ∌dom-≢ {Γ} {x} {x'} {t} (notInNonempty xNeqx' xNotInΓ') xEqx' = xNeqx' xEqx'
           
