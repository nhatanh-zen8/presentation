module hello where
open import Data.Vec
open import Data.Nat
import Data.Fin using (Fin)

-- ADT examples
-- Boolean
data Bool : Set where
  true : Bool
  false : Bool

-- Peano-encoding of natural number
-- data N : Set where
--  zero : N
--  suc  : N → N

-- _+_ : N → N → N
-- zero + zero = zero
-- zero + n = n
-- (suc n') + n = suc (n + n')

-- Mixfixed operators
-- if-then-else is just another operator in Agda instead of being a builtin language construct
if_then_else_ : { A : Set } → Bool → A → A → A
if true then expr else nop = expr
if false then nop else expr = expr

-- Equality 
data _≡_ : {A : Set} -> A -> A -> Set where
  refl : {A : Set} {a : A} -> a ≡ a


1eq1 : 1 ≡ 1
1eq1 = refl

-- 2eq1 : 2 ≡ 1
-- 2eq1 = ? -- impossible to implement

-- 2 vectors of length n and m append together can only result in a vector of length n+m
_+++_ : {a : Set} {n m : ℕ} -> Vec a n -> Vec a m -> Vec a (n + m)
[] +++ t = t
(x ∷ v) +++ t = x ∷ (v +++ t)

-- type-safe indexing
-- Fin n = all natural numbers < n
-- with index of type Fin n, where n is the length of the vector, it's impossible to write an implementation where you can get
element outside of the vector's bound
-- This is *the* answer to the infamous off-by-one error
_:[_] : {n : ℕ} {a : Set} -> Vec a n -> (ind : Data.Fin.Fin n) -> a
(x ∷ vec) :[ Data.Fin.Fin.zero ] = x
(x ∷ vec) :[ Data.Fin.Fin.suc index ] = vec :[ index ]

