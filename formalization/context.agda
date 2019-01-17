-- Contexts, from https://github.com/hazelgrove/agda-popl17/blob/master/core.agda
open import Data.Nat
open import Data.Maybe
open import Prelude
import Relation.Binary.PropositionalEquality 
open Relation.Binary.PropositionalEquality using (_≡_; refl)
open Relation.Binary.PropositionalEquality.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

module Context (τ̇ : Set) where
  -- variables are named with naturals in expr. therefore we represent
    -- contexts as functions from names for variables (nats) to possible
    -- bindings.
·ctx : Set
·ctx = ℕ → Maybe τ̇
  
-- convenient shorthand for the (unique up to fun. ext.) empty context
∅ : ·ctx
∅ _ = nothing
  
-- add a new binding to the context, clobbering anything that might have
-- been there before.
_,,_ : ·ctx → (ℕ × τ̇) → ·ctx
(Γ ,, (x , t)) y with compare x y
(Γ ,, (x , t)) .x | equal _ = just t
(Γ ,, (x , t)) y  | _  = Γ y
                                   
-- membership, or presence, in a context
_∈_ : (p : ℕ × τ̇) → (Γ : ·ctx) → Set
(x , t) ∈ Γ = (Γ x) ≡ just t
                              
-- apartness for contexts, so that we can follow barendregt's convention
_#_ : (n : ℕ) → (Γ : ·ctx) → Set
x # Γ = (Γ x) ≡ nothing
                 
-- without: remove a variable from a context
_/_ : ·ctx → ℕ → ·ctx
(Γ / x) y with compare x y
(Γ / x) .x | equal _ = nothing
(Γ / x) y  | _  = Γ y
  
