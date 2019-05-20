module Prelude where
  open import Agda.Primitive using (Level; lzero; lsuc) renaming (_⊔_ to lmax)
  open import Relation.Binary.PropositionalEquality as Eq

  open import Data.List
  open import Data.Nat.Properties

  open import Data.List.Membership.DecSetoid ≡-decSetoid 

  open import Data.List.Relation.Unary.Any
  open import Data.Empty

  open import Data.List.Relation.Unary.All


  -- sums
  data _+̇_ (A B : Set) : Set where
    Inl : A → A +̇ B
    Inr : B → A +̇ B

 -- Basic properties of lists
  emptyListIsEmpty : ∀ {x}
                     → x ∉ []
  emptyListIsEmpty ()


  listNoncontainment : ∀ {x x' L}
                        → x ∉ L
                        → x' ∈ L
                        → x ≢ x'
  listNoncontainment {x} {x'} {L} xNotInL x'InL = λ xEqx' → xNotInL (Eq.subst (λ a → a ∈ L) (Eq.sym xEqx') x'InL)


  listConcatNoncontainment : ∀ {x x' L}
                             → x ≢ x'
                             → x ∉ L
                             → x ∉ (x' ∷ L)
  listConcatNoncontainment {x} {x'} {L} xNeqx' xNotInL (here xEqx') = xNeqx' xEqx'
  listConcatNoncontainment {x} {x'} {L} xNeqx' xNotInL (there xInConcat) = xNotInL xInConcat

  listConcatContainment : ∀ {x x' t}
                          → x ∈ (x' ∷ t)
                          → x' ≢ x
                          → x ∈ t

  listConcatContainment {x} {x'} {t} (here px) x'Neqx = ⊥-elim (x'Neqx (Eq.sym px))
  listConcatContainment {x} {x'} {t} (there xInCons) x'Neqx = xInCons
  
-- Basic properties of equality
  ≢-sym : ∀ {a} {A : Set a} → {x x' : A}
          → x ≢ x'
          → x' ≢ x
  ≢-sym xNeqx' = λ x'Eqx → xNeqx' (Eq.sym x'Eqx)

-- Congruence
  cong₃ : ∀ {a b c d} {A : Set a} {B : Set b} {C : Set c} {D : Set d}
        (f : A → B → C → D) {x y u v w t} → x ≡ y → u ≡ v → w ≡ t → f x u w ≡ f y v t
  cong₃ f refl refl refl = refl

-- All
  allInsideOut : {A : Set}
                 → {L L' : List A}
                 → {P : A → A → Set}
                 → (∀ {T T'} → P T T' → P T' T)
                 → All (λ T → All (λ T' → P T T') L) L'
                 → All (λ T → All (λ T' → P T T') L') L
  allInsideOut {A} {[]} {.[]} sym [] = []
  allInsideOut {A} {x ∷ L} {.[]} sym [] = [] ∷ allRelatedToEmpty
    where
      allRelatedToEmpty : {A : Set}
                          → {L : List A}
                          → {P : A → A → Set}
                          → All (λ T → All (P T) []) L
      allRelatedToEmpty {A} {[]} {P} = []
      allRelatedToEmpty {A} {x ∷ L} {P} = [] ∷ allRelatedToEmpty
  allInsideOut {A} {[]} {.(_ ∷ _)} sym (all₁ ∷ all₂) = []
  allInsideOut {A} {x ∷ L} {(x' ∷ L')} {P} sym (all₁ ∷ all₂) =
    ((sym (Data.List.Relation.Unary.All.head all₁)) ∷ unwrap L' all₂) ∷
      allInsideOut sym (Data.List.Relation.Unary.All.tail all₁ ∷  unwrap2 L' all₂)
    where
      unwrap : (M : List A)
               → All (λ T → All (P T) (x ∷ L)) M
               → All (λ T' → P x T') M
      unwrap .[] [] = []
      unwrap (x ∷ r) (h ∷ t) = (sym (Data.List.Relation.Unary.All.head h)) ∷ unwrap r t

      unwrap2 : (M : List A)
                → All (λ T → All (P T) (x ∷ L)) M
                → All (λ T → All (P T) L) M
      unwrap2 [] all = []
      unwrap2 (x ∷ M) (all₁ ∷ all₂) = (Data.List.Relation.Unary.All.tail all₁) ∷ unwrap2 M all₂

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
