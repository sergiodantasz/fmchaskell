module FMCBabyNat where

-- Do not alter this import!
import Prelude (Eq (..), Show (..), undefined)

-- Define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- Syntactic sugar for some successors
zero, one, two, three, four, five, six, seven, eight, nine, ten :: Nat
zero = O
one = S zero
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven
nine = S eight
ten = S nine

-- Sugar for `yes` and `no`
yes, no :: Nat
yes = one
no = zero

-- Addition
(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

-- Syntactic associativity: L (left)
-- Syntactic precedence: 6
infixl 6 +

-- Output: O (no) means False, S O (yes) means True
isZero :: Nat -> Nat
isZero O = yes
isZero n = no

-- Predecessor
-- We define zero's pred to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Even
-- Output: O (no) means False, S O (yes) means True
even :: Nat -> Nat
even O = yes
even (S O) = no
even (S (S n)) = even n

-- Odd
-- Output: O (no) means False, S O (yes) means True
odd :: Nat -> Nat
odd n = isZero (even n)

-- Monus
-- This is called the dotminus or monus operator (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0 when "normal" subtraction would return a negative number.
(-*) :: Nat -> Nat -> Nat
n -* O = n
O -* _ = O
(S n) -* (S m) = n -* m

monus :: Nat -> Nat -> Nat
monus = (-*)

infixl 6 -*

-- Multiplication
(*) :: Nat -> Nat -> Nat
_ * O = O
n * (S m) = (n * m) + n

infixl 7 *

-- Exponentiation
(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ (S m) = n ^ m * n

infixr 8 ^

-- Less than
-- Output: O (no) means False, S O (yes) means True
(<) :: Nat -> Nat -> Nat
O < (S _) = yes
_ < O = no
(S n) < (S m) = n < m

lt :: Nat -> Nat -> Nat
lt = (<)

infix 4 <

-- Quotient
(/) :: Nat -> Nat -> Nat
_ / O = undefined
O / _ = O
n / m = case n < m of
          S O -> O
          O   -> S O + (n -* m) / m

infixl 7 /

-- Remainder
(%) :: Nat -> Nat -> Nat
_ % O = undefined
n % m = case n < m of
          S O -> n
          O   -> (n -* m) % m

infixl 7 %

-- Divides
-- Just for a change, we start by defining the "symbolic" operator and then define `divides` as a synonym to it.
-- Output: O (no) means False, S O (yes) means True
(|||) :: Nat -> Nat -> Nat
O ||| n = isZero n
n ||| m = isZero (m % n)

divides :: Nat -> Nat -> Nat
divides = (|||)

infix 4 |||

-- Absolute difference
-- n |-| m = |n - m|
-- (Careful here: this (-) is the actual minus operator we know from the integers!)
(|-|) :: Nat -> Nat -> Nat
n |-| m = case n < m of
            S O -> m -* n
            O   -> n -* m

absDiff :: Nat -> Nat -> Nat
absDiff = (|-|)

infixl 6 |-|

-- Factorial
fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

-- Signum of a number (-1, 0, or 1)
-- (We won't use -1 because we're working only with the naturals!)
sg :: Nat -> Nat
sg O = zero
sg _ = one

-- Logarithm
-- lo n m is the floor of the logarithm base n of m
lo :: Nat -> Nat -> Nat
lo O _ = undefined
lo (S O) _ = undefined
lo _ O = undefined
lo n m = case m < n of
           S O -> O
           O   -> S O + lo n (m / n)
