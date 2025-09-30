{-# LANGUAGE GADTs #-}

module FMCList where

import Data.Char qualified as C
import Data.List qualified as L
import Prelude
  ( Bool (..),
    Char,
    Double,
    Enum (..),
    Eq (..),
    Float,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    String,
    curry,
    error,
    flip,
    not,
    otherwise,
    uncurry,
    undefined,
    ($),
    (&&),
    (.),
    (||),
  )
import Prelude qualified as P

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}

{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head []      = undefined
head (x : _) = x

tail :: [a] -> [a]
tail []       = undefined
tail (_ : xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: (Integral i) => [a] -> i
length []       = 0
length (_ : xs) = 1 + length xs

sum :: (Num a) => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

product :: (Num a) => [a] -> a
product []       = 1
product (x : xs) = x * product xs

(++) :: [a] -> [a] -> [a]
[]       ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ [x]

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x []       = [x]
snoc x (y : ys) = y : snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []       = xs
xs +++ [y]      = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

-- minimum
minimum :: Ord a => [a] -> a
minimum []       = undefined
minimum [x]      = x
minimum (x : xs) = min x (minimum xs)

-- maximum
maximum :: Ord a => [a] -> a
maximum []       = undefined
maximum [x]      = x
maximum (x : xs) = max x (maximum xs)

-- take
take :: (Integral i) => i -> [a] -> [a]
take 0 _        = []
take _ []       = []
take x (y : ys) = y : take (x - 1) ys

-- drop
drop :: (Integral i) => i -> [a] -> [a]
drop 0 xs       = xs
drop _ []       = []
drop x (_ : ys) = drop (x - 1) ys

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x       = dropWhile p xs
  | otherwise = x : xs

-- tails
tails :: [a] -> [[a]]
tails []       = [[]]
tails (x : xs) = (x : xs) : tails xs

-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
