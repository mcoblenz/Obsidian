module Prelude where
  open import Agda.Primitive using (Level; lzero; lsuc) renaming (_⊔_ to lmax)
  open import Data.Nat
  import Relation.Binary.PropositionalEquality as Eq
  open Eq using (_≡_; refl)
  open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

  -- empty type
  data ⊥ : Set where

  -- from false, derive whatever
  abort : ∀ {C : Set} → ⊥ → C
  abort ()

  -- unit
  data ⊤ : Set where
    <> : ⊤

  -- sums
  data _+̇_ (A B : Set) : Set where
    Inl : A → A +̇ B
    Inr : B → A +̇ B

  -- pairs
  infixr 1 _,_
  record Σ {l1 l2 : Level} (A : Set l1) (B : A → Set l2) : Set (lmax l1 l2) where
    constructor _,_
    field
      π1 : A
      π2 : B π1
  open Σ public

  -- Sigma types, or dependent pairs, with nice notation.
  syntax Σ A (\ x -> B) = Σ[ x ∈ A ] B

  _×_ : {l1 : Level} {l2 : Level} → (Set l1) → (Set l2) → Set (lmax l1 l2)
  A × B = Σ A λ _ → B

  infixr 1 _×_

{-
-- equality of naturals is decidable. we represent this as computing a
  -- choice of units, with inl <> meaning that the naturals are indeed the
  -- same and inr <> that they are not.
  natEQ : (x y : ℕ) → ((x ≡ y) + ((x ≡ y) → ⊥))
  natEQ Z Z = Inl refl
  natEQ Z (1+ y) = Inr (λ ())
  natEQ (1+ x) Z = Inr (λ ())
  natEQ (1+ x) (1+ y) with natEQ x y
  natEQ (1+ x) (1+ .x) | Inl refl = Inl refl
  ... | Inr b = Inr (λ x₁ → b (1+inj x y x₁))

  -- nat equality as a predicate. this saves some very repetative casing.
  natEQp : (x y : ℕ) → Set
  natEQp x y with natEQ x y
  natEQp x .x | Inl refl = ⊥
  natEQp x y | Inr x₁ = ⊤
-}

{-

  -- equality
  data _≡_ {l : Level} {A : Set l} (M : A) : A → Set l where
    refl : M ≡ M

  infixr 9 _≡_

  {-# BUILTIN EQUALITY _≡_ #-}

  -- transitivity of equality
  _·_ : {l : Level} {α : Set l} {x y z : α} → x ≡ y → y ≡ z → x ≡ z
  refl · refl = refl

  -- symmetry of equality
  ! : {l : Level} {α : Set l} {x y : α} → x ≡ y → y ≡ x
  ! refl = refl

  -- ap, in the sense of HoTT, that all functions respect equality in their
  -- arguments. named in a slightly non-standard way to avoid naming
  -- clashes with hazelnut constructors.
  ap1 : {l1 l2 : Level} {α : Set l1} {β : Set l2} {x y : α} (F : α → β)
          → x ≡ y → F x ≡ F y
  ap1 F refl = refl

  -- transport, in the sense of HoTT, that fibrations respect equality
  tr : {l1 l2 : Level} {α : Set l1} {x y : α}
                    (B : α → Set l2)
                    → x ≡ y
                    → B x
                    → B y
  tr B refl x₁ = x₁

  -- options
  data Maybe (A : Set) : Set where
    Some : A → Maybe A
    None : Maybe A

  -- the some constructor is injective. perhaps unsurprisingly.
  someinj : {A : Set} {x y : A} → Some x ≡ Some y → x ≡ y
  someinj refl = refl

  --  some isn't none.
  somenotnone : {A : Set} {x : A} → Some x ≡ None → ⊥
  somenotnone ()

  -- function extensionality, used to reason about contexts as finite
  -- functions.
  postulate
     funext : {A : Set} {B : A → Set} {f g : (x : A) → (B x)} →
              ((x : A) → f x ≡ g x) → f ≡ g

  -- non-equality is commutative
  flip : {A : Set} {x y : A} → (x ≡ y → ⊥) → (y ≡ x → ⊥)
  flip neq eq = neq (! eq)

  -- two types are said to be equivalent, or isomorphic, if there is a pair
  -- of functions between them where both round-trips are stable up to ≡
  _≃_ : Set → Set → Set
  _≃_ A B = Σ[ f ∈ (A → B) ] Σ[ g ∈ (B → A) ]
             (((a : A) → g (f a) ≡ a) × (((b : B) → f (g b) ≡ b)))




 -}
