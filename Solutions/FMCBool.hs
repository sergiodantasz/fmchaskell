module ExBool where

-- Do not alter this import!
import Prelude
  ( Char,
    Enum (..),
    Eq (..),
    Int,
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    error,
    otherwise,
    undefined,
    ($),
    (++),
    (.),
  )

-- Define everything that is undefined, without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where
  show False = "False"
  show True  = "True"

instance Enum Bool where
  toEnum = undefined

  fromEnum = undefined

-- Conjunction (AND)
(&&) :: Bool -> Bool -> Bool
(&&) = undefined

infixr 3 &&

-- Disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) = undefined

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) = undefined

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) = undefined

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) = undefined

infixr 2 <=/=>

-- Boolean negation
not :: Bool -> Bool
not = undefined

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse = undefined

-- Logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) = undefined

infixr 1 ==>

-- Logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) = undefined

infixl 1 <==

-- Logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) = undefined

infixr 1 <=>
