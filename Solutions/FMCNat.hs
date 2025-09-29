{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
  ( Bool (..),
    Eq (..),
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    fst,
    snd,
    error,
    not,
    otherwise,
    undefined,
    ($),
    (&&),
    (++),
    (.),
    (||),
  )

-- New data type called Nat
data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- Typeclass implementations
----------------------------------------------------------------

instance Show Nat where
  -- zero should be shown as O
  -- three should be shown as SSSO
  show O     = "O"
  show (S n) = "S" ++ show n

instance Eq Nat where
  O   == O   = True
  S n == S m = n == m
  _   == _   = False

instance Ord Nat where
  O   <= _   = True
  S _ <= O   = False
  S n <= S m = n <= m

  -- Ord does not REQUIRE defining min and max.
  -- Howevener, you should define them WITHOUT using (<=).
  -- Both are binary functions: max m n = ..., etc.

  min O _         = O
  min _ O         = O
  min (S n) (S m) = S (min n m)

  max O n         = n
  max n O         = n
  max (S n) (S m) = S (max n m)

----------------------------------------------------------------
-- Syntatic sugar for numbers
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight, nine, ten :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine  = S eight
ten   = S nine

----------------------------------------------------------------
-- Internalized predicates
----------------------------------------------------------------

-- Is zero
isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- Predecessor
-- (We define zero's pred to be zero.)
pred :: Nat -> Nat
pred O     = O
pred (S n) = n

-- Even
even :: Nat -> Bool
even O         = True
even (S O)     = False
even (S (S n)) = even n

-- Odd
odd :: Nat -> Bool
odd n = not (even n)

----------------------------------------------------------------
-- Operations
----------------------------------------------------------------

-- Addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> S m = S (n + m)

infixl 6 <+>

-- Monus
-- This is called the dotminus or monus operator (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0 when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (<->)

(<->) :: Nat -> Nat -> Nat
n   <-> O   = n
O   <-> _   = O
S n <-> S m = n <-> m

infixl 6 <->

-- Multiplication
times :: Nat -> Nat -> Nat
times = (<*>)

(<*>) :: Nat -> Nat -> Nat
n <*> O   = O
n <*> S m = n <*> m + n

infixl 7 <*>

-- Power / Exponentiation
pow :: Nat -> Nat -> Nat
pow = (<^>)

exp :: Nat -> Nat -> Nat
exp = (<^>)

(<^>) :: Nat -> Nat -> Nat
_ <^> O   = S O
n <^> S m = n <^> m * n

infixr 8 <^>

-- Euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (_, O) = undefined
eucdiv (n, m)
  | n < m     = (O, n)
  | otherwise = (q, r)
  where
    q = S q'
    r = r'
    (q', r') = eucdiv (n <-> m, m)

-- Quotient
quot :: Nat -> Nat -> Nat
quot = (</>)

(</>) :: Nat -> Nat -> Nat
n </> m = (fst . eucdiv) (n, m)

infixl 7 </>

-- Remainder
rem :: Nat -> Nat -> Nat
rem = (</>)

(<%>) :: Nat -> Nat -> Nat
n <%> m = (snd . eucdiv) (n, m)

infixl 7 <%>

-- Divides
divides :: Nat -> Nat -> Bool
divides = (<|>)

(<|>) :: Nat -> Nat -> Bool
O <|> n = isZero n
n <|> m = isZero (m <%> n)

infix 4 <|>

-- Distance
-- It's the distance between nats, given by x `dist` y = |x - y|.
-- (Careful here: this (-) is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = (|-|)

(|-|) :: Nat -> Nat -> Nat
n |-| m =
  if n < m
    then m <-> n
    else n <-> m

infixl 6 |-|

-- Factorial
factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n <*> factorial n

-- Signum
-- The signum of a number can be: -1 for negative numbers, 0 for zero, and 1 for positive numbers.
-- (We won't use -1 because we're working only with the naturals!)
sg :: Nat -> Nat
sg O = O
sg _ = S O

-- Logarithm
-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = undefined
lo (S O) _ = undefined
lo _ O = undefined
lo n m =
  if m < n
    then O
    else S (lo n (m </> n))

----------------------------------------------------------------
-- Num & Integral
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: (Integral a) => a -> Nat
toNat x
  | x < 0     = undefined
  | x == 0    = O
  | otherwise = S (toNat (x - 1))

fromNat :: (Integral a) => Nat -> a
fromNat O     = 0
fromNat (S n) = 1 + fromNat n

-- Voilà: we can now easily make Nat an instance of Num.
instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0     = undefined
    | x == 0    = O
    | otherwise = S (fromInteger (x - 1))
