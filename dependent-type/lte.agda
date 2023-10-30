module Lte where
open import Data.List.Base public using (List; []; _∷_)
open import Data.Nat
--open import Data.List.Relation.Unary.Sorted.TotalOrder using (Sorted)

--ls : List ℕ
--ls = 1 ∷ 2 ∷ 3 ∷ []

data _<=_ : ℕ -> ℕ -> Set where
  zerolt : {n : ℕ} -> zero <= n
  lt : {m n : ℕ} -> m <= n -> (suc m) <= (suc n)

-- proof of transitivity
transitive-lt : {l m n : ℕ} -> l <= m -> m <= n -> l <= n
transitive-lt zerolt _ = zerolt
transitive-lt (lt lt1) (lt lt2) = lt (transitive-lt lt1 lt2)

-- using proof to guarantee only n < 10 can be added to a list
addToLs : (n : ℕ) -> (n <= 10) -> List ℕ -> List ℕ
addToLs n proof ls = n ∷ ls

nextLs = addToLs 11 ? ls
