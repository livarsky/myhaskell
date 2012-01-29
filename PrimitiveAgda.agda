-- Date: December 2011
-- Author: Jan Malakhovski
-- WWW: http://twitter.com/oxij http://blog.oxij.org

-- This file was written for Agda programming language subcourse
-- of functional programming course at NRU ITMO.

-- Everything here is in Public Domain.

module PrimitiveAgda where

-- ≡ is \==
-- Martin-Lof equivalence (a special version for values only, not for types, kinds, sorts and so on).
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

-- Properties.
≡refl : {A : Set}{a b : A} → a ≡ b → b ≡ a
≡refl refl = refl

≡trans : {A : Set}{a b c : A} → a ≡ b → b ≡ c → a ≡ c
≡trans refl refl = refl

_~_ : {A : Set}{a b c : A} → a ≡ b → b ≡ c → a ≡ c
_~_ = ≡trans

cong : {A B : Set} {a b : A} → (f : A → B) → a ≡ b → f a ≡ f b
cong f refl = refl

-- ⊥ is \bot
-- Empty type.
data ⊥ : Set where

-- Absurd implies anything.
⊥-elim : { A : Set } → ⊥ → A
⊥-elim ()

-- ⊤ is \top
-- One element type.
record ⊤ : Set where
  constructor tt
  
-- Booleans.
data Bool : Set where
  true false : Bool

-- Magic!
isTrue : Bool -> Set
isTrue false = ⊥
isTrue true = ⊤

-- ℕ is \bn
-- Naturals.
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

notzero : ℕ → Bool
notzero zero = false
notzero (succ n) = true

---------------------------------------------------------
-- Example usage of the magic.

kindadiv : (a : ℕ) → (b : ℕ) → { x : isTrue (notzero b) } → ℕ
kindadiv a zero {()}
kindadiv a (succ b) = a 

--------------------------------------------------------

-- ∘ is \o
-- λ is \lambda
-- Most abstract (and scary) way to type a function composition.
_∘_ : {A : Set} {B : A → Set} {C : {x : A} → B x → Set} → (f : {x : A} → (y : B x) → C y) → (g : (x : A) → B x) → ((x : A) → C (g x))
f ∘ g = λ x → f (g x)

-- ¬ is \lnot
-- Logical negation.
¬ : Set → Set
¬ a = a → ⊥

-- If A implies B then not B implies not A.
¬impl : {A B : Set} → (A → B) → (¬ B → ¬ A)
¬impl f = λ g → g ∘ f

-- Contradiction implies anything.
contradiction : {A B : Set} → A → ¬ A → B
contradiction a nota = ⊥-elim (nota a)

-------------------------------------------

-- Sum of two naturals.
_+_ : ℕ → ℕ → ℕ
_+_ zero a = a
_+_ (succ a) b = succ (a + b)

-- Operations' associativity and priorities to make Agda's parser happy.
infixl 10 _~_
infix 10 _≡_
infixl 20 _+_

-- Properties.
lemma-succ : {x y : ℕ} → x ≡ y → succ x ≡ succ y
lemma-succ refl = refl

lemma-unsucc : {x y : ℕ} → succ x ≡ succ y → x ≡ y
lemma-unsucc refl = refl

assoc : {x y z : ℕ} → x + (y + z) ≡ (x + y) + z
assoc {zero} {y} {z} = refl
assoc {succ x} {y} {z} = lemma-succ (assoc {x} {y} {z})

y=y+0 : {y : ℕ} → y ≡ y + zero
y=y+0 {zero} = refl
y=y+0 {succ y} = lemma-succ (y=y+0 {y})

sy=y+1 : {y : ℕ} → succ y ≡ y + succ zero
sy=y+1 {zero} = refl
sy=y+1 {succ y} = lemma-succ (sy=y+1 {y})


sx+y=x+sy : {x y : ℕ} → succ x + y ≡ x + succ y
sx+y=x+sy {zero} {y} = refl
sx+y=x+sy {succ x} {y} = lemma-succ (sx+y=x+sy {x} {y})


-- (*) Commutativity.
comm : {x y : ℕ} → x + y ≡ y + x
comm {zero} {y} = y=y+0 {y}
comm {succ x} {y} = (lemma-succ (comm {x} {y}))~(sx+y=x+sy {x} {y})
-- Hint: dpohaboea=+=absfaojdf
-- Caesar +1 code.
--
-- R=" abcdefghijklmnopqrstuvwxyz_=~+."
-- A="$HINT"
-- findat() { 
--   for ((i=0; i<${#R}; i++)); do if [[ ${R:$i:1} == $1 ]]; then echo $i; return; fi; done; echo -1;
-- }
-- for ((i=0; i<${#A}; i++)); do x=`findat ${A:$i:1}`; echo -n ${R:$((x+1)):1}; done

-------------------------------------------
-- Emulating type classes.

-- Interface. Almost Haskell's "class" keyword
record Summable (A : Set) : Set where
  field
    _+'_ : A → A → A

-- Haskell: (Summable A) => A -> A -> A
abstractSum : ∀ {A} → (Summable A) → A → A → A
abstractSum s = _+'_ where
  open Summable s
-- But! In Haskell Summable is not just an argument, it is inferenced.

-- Taking Summable A from a context of a call (only possible in recent Agda versions):
abstractSum' : ∀ {A} → {{s : Summable A}} → A → A → A
abstractSum' {{s}} = _+'_ where
  open Summable s

-- Inferencing with bare hands:

-- Hidden details (not visible in Haskell).
data What : Set where
  bool nat : What

-- Reversable total function.
W : What -> Set
W bool = Bool
W nat = ℕ

-- If you give me a name of a type I'll give you an implementation of the interface.
-- Almost Haskell's "instance" declarations.
getSummable : (x : What) → Summable (W x)
getSummable bool = record { _+'_ = (λ x y -> x) }
getSummable nat = record { _+'_ = (λ x y -> y) }

-- Magic!
abstractSum'' : {x : What} → W x → W x → W x
abstractSum'' {x} = abstractSum {A = W x} (getSummable x)

-------------------------------------------

-- Maybes (optional values).
data Maybe (A : Set) : Set where
  just : A → Maybe A
  nothing : Maybe A

-- Sum of types.
data Either (A B : Set) : Set where
  left : A → Either A B
  right : B → Either A B
  
-- ∷ is \::
-- Lists.
data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

length : ∀ {A} → List A → ℕ
length [] = zero
length (a ∷ as) = succ (length as)
  
-- Lists of known length.
data Vec (A : Set) : ℕ → Set where
  [0] : Vec A zero
  _::_ : {n : ℕ} → A → Vec A n → Vec A (succ n)

head : ∀ {A n} → Vec A (succ n) → A
head (a :: as) = a

-- try this:
--test1 = head [0]

-- Finite type. Each Fin n has exactly n elements.
data Fin : ℕ → Set where
  fzero : Fin (succ zero)
  fsucc : ∀ {n} → Fin n → Fin (succ n)

-- Get an element from a Vec by its number.
lookup : ∀ {A n} → Fin n → Vec A n → A
lookup fzero (a :: as) = a
lookup (fsucc f) (a :: as) = lookup f as

list2vec : ∀ {A} → (l : List A) → Vec A (length l)
list2vec [] = [0]
list2vec (a :: as) = a :: (list2vec as)

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : ∀ {A B} → A × B → A
fst (a , b) = a

snd : ∀ {A B} → A × B → B
snd (a , b) = b

infixl 15 _×_

data _<_ : ℕ → ℕ → Set where
  z<s : {n : ℕ} → zero < succ n
  s<s  : {n m : ℕ} → n < m → succ n < succ m

_≤_ : ℕ → ℕ → Set
a ≤ b = Either (a ≡ b) (a < b)

-- (**) If you give me a list and a proof that its length is less than n
-- I'll give you a tuple (prefix of length n, suffix)
cuthead : ∀ {A} {n : ℕ} → (l : List A) → n ≤ length l → Vec A n × List A
cuthead = {!!}

-- (***) Previous definition does not guarantee correct split
-- (e.g. you can make up any suffix). Define a better one.
-- splitn : ∀ {A} {n : ℕ} → ? → Vec A n × ?

------------------------------------------------------
-- By the way, in Haskell you can have a kind of type-level naturals like this.
-- No pattern matching on types => no type normalization
-- => no nice equivalence, though.

postulate Zero : Set
postulate Succ : Set → Set

postulate _+t_ : Set → Set → Set

test2 = Zero +t Succ Zero
