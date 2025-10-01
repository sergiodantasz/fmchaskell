module FMCBool where

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

data Bool = False | True

instance Show Bool where
  show False = "False"
  show True  = "True"

instance Enum Bool where
  toEnum 0 = False
  toEnum 1 = True
  toEnum _ = undefined

  fromEnum False = 0
  fromEnum True  = 1

-- Conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

infixr 3 &&

-- Disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
True /|\ True = False
_    /|\ _    = True

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
False \|/ False = True
_     \|/ _     = False

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True  <=/=> False = True
False <=/=> True  = True
_     <=/=> _     = False

infixr 2 <=/=>

-- Boolean negation
not :: Bool -> Bool
not True  = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  thenVal  _elseVal = thenVal
ifThenElse False _thenVal elseVal  = elseVal

-- Logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_    ==> _     = True

infixr 1 ==>

-- Logical "implied by"
(<==) :: Bool -> Bool -> Bool
False <== True = False
_     <== _    = True

infixl 1 <==

-- Logical equivalence
(<=>) :: Bool -> Bool -> Bool
True  <=> True  = True
False <=> False = True
_     <=> _     = False

infixr 1 <=>
