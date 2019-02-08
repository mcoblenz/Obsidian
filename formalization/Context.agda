-- Adapted from Wadler: https://plfa.github.io/Lambda/


module Context (A : Set) where

  open import Data.Nat
  open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl)
  open import Data.Maybe
  open import Data.Product using (_×_; proj₁; proj₂; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
  open import Relation.Nullary.Decidable
  open import Relation.Nullary using (Dec; yes; no)


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
  

  obviousContainment : ∀ {x t}
                       → ∀ (Γ : ctx)
                       → Γ , x ⦂ t ∋ x ⦂ t
                     
  obviousContainment Γ = Z

{- I will probably need this eventually, but not yet.
  lookupWorksWithContainment : ∀ {Γ : ctx}
                               → ∀ {x t}
                               → Γ ∋ x ⦂ t
                               --------------------
                               → lookup Γ x ≡ just t


  lookupWorksWithContainment {Γ} (Z {Γ'} {x} {a}) = {!!}
-}

  lookupWeakening : ∀ {Γ : ctx}
                    → ∀ {x x' t t'}
                    → Γ ∋ x ⦂ t
                    → ∃[ T ] ((Γ , x' ⦂ t') ∋ x ⦂ T)
                    
  lookupWeakening {Γ} {x} {x'} {t} {t'} Γcontainment with x ≟ x'
  ...                                                 | yes refl = ⟨ t' , Z {Γ = Γ} {x = x'} {a = t'} ⟩
  ...                                                 | no neq =  ⟨ t , S neq Γcontainment ⟩
