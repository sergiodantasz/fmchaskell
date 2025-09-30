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
init :: [a] -> [a]
init []       = undefined
init [_]      = []
init (x : xs) = x : init xs

-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = xs : inits (init xs)

-- subsequences
subsequences :: [a] -> [[a]]
subsequences []       = [[]]
subsequences (x : xs) =
  let tailSubs = subsequences xs
   in tailSubs ++ map (x :) tailSubs

-- any
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x : xs)
  | p x       = True
  | otherwise = any p xs

-- all
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x : xs)
  | p x       = all p xs
  | otherwise = False

-- and
and :: [Bool] -> Bool
and []       = True
and (b : bs) = b && and bs

-- or
or :: [Bool] -> Bool
or []       = False
or (b : bs) = b || or bs

-- concat
concat :: [[a]] -> [a]
concat []       = []
concat (l : ls) = l ++ concat ls

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x = any (== x)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys)
  | x == y    = True
  | otherwise = elem' x ys

-- (!!)
(!!) :: [a] -> Int -> a
[]       !! _ = undefined
(x : _)  !! 0 = x
(_ : xs) !! i = xs !! (i - 1)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- map
map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = undefined
cycle xs = xs ++ cycle xs

-- repeat
repeat :: a -> [a]
repeat x = cycle [x]

-- replicate
replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs ys = isPrefixOf xs ys || isInfixOf xs ys

-- isSuffixOf
isSufixOf :: Eq a => [a] -> [a] -> Bool
isSufixOf xs ys = reverse xs `isPrefixOf` reverse ys

-- zip
zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

-- intercalate
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [ys] = ys
intercalate xs (ys : yss) = ys ++ xs ++ intercalate xs yss

-- nub
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (x /=) xs)

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
-- the problem is performance! the function iterates through the list two times.
splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 xs = ([], xs)
splitAt _ [] = ([], [])
splitAt n (x : xs) =
  let (leftPart, rightPart) = splitAt (n - 1) xs
   in (x : leftPart, rightPart)

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break p (x : xs)
  | p x       = ([], x : xs)
  | otherwise =
    let (ys, zs) = break p xs
     in (x : ys, zs)

-- lines
lines :: String -> [String]
lines "" = []
lines s =
  let (firstLine, rem) = break (== '\n') s
   in case rem of
        "" -> [firstLine]
        (_ : afterBreak) -> firstLine : lines afterBreak

-- words
words :: String -> [String]
words s =
  case dropWhile C.isSpace s of
    "" -> []
    s' -> let (word, rem) = break C.isSpace s'
           in word : words rem

-- unlines
unlines :: [String] -> String
unlines [] = ""
unlines s = intercalate "\n" s ++ "\n"

-- unwords

-- transpose

-- normalize
normalize :: String -> String
normalize s = filter C.isAlphaNum (map C.toLower s)

-- palindrome
-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome s =
  let normS = normalize s
   in normS == reverse normS

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
