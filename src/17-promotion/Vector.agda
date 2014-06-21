module Vector where

infixr 10 _∷_

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ    #-}
{-# BUILTIN ZERO    zero #-}
{-# BUILTIN SUC     suc  #-}

infixl 6 _+_

_+_ : ℕ → ℕ → ℕ
0 + n = n
suc m + n = suc (m + n)

data Vec (A : Set) : ℕ → Set where
  []  : Vec A 0
  _∷_ : ∀ {n} → A → Vec A n → Vec A (suc n)

_++_ : ∀ {A n m} → Vec A n → Vec A m → Vec A (n + m)
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

infix 4 _≡_

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

subst : {A : Set} → (P : A → Set) → ∀{x y} → x ≡ y → P x → P y
subst P refl p = p

cong : {A B : Set} (f : A → B) → {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

vec : ∀ {A} (k : ℕ) → Set
vec {A} k = Vec A k

plus_zero : {n : ℕ} → n + 0 ≡ n 
plus_zero {zero}  = refl
plus_zero {suc n} = cong suc plus_zero

plus_suc : {n : ℕ} → n + (suc 0) ≡ suc n 
plus_suc {zero}  = refl
plus_suc {suc n} = cong suc (plus_suc {n})

reverse : ∀ {A n} → Vec A n → Vec A n
reverse []       = []
reverse {A} {suc n} (x ∷ xs) = subst vec (plus_suc {n}) (reverse xs ++ (x  ∷ []))
