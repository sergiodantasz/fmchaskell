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

-- head
head :: [a] -> a
head [] = error "head called with an empty list"
head (x : _) = x

-- tail
tail :: [a] -> [a]
tail [] = error "tail called with an empty list"
tail (_ : xs) = xs

-- null
null :: [a] -> Bool
null [] = True
null _ = False

-- length
length :: (Integral i) => [a] -> i
length [] = 0
length (_ : xs) = 1 + length xs

-- sum
sum :: (Num a) => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

-- product
product :: (Num a) => [a] -> a
product [] = 1
product (x : xs) = x * product xs

-- (**)
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

infixr 5 ++

-- reverse
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- snoc
-- it is cons written backwards
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y : ys) = y : snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- (+++)
-- it is a different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ [] = xs
xs +++ [y] = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

infixl 5 +++

-- minimum
minimum :: (Ord a) => [a] -> a
minimum [] = error "minimum called with an empty list"
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

-- maximum
maximum :: (Ord a) => [a] -> a
maximum [] = error "maximum called with an empty list"
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

-- take
take :: (Integral i) => i -> [a] -> [a]
take x _ | x <= 0 = []
take _ [] = []
take x (y : ys) = y : take (x - 1) ys

-- drop
drop :: (Integral i) => i -> [a] -> [a]
drop x xs | x <= 0 = xs
drop _ [] = []
drop x (_ : ys) = drop (x - 1) ys

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = dropWhile p xs
  | otherwise = x : xs

-- tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

-- init
init :: [a] -> [a]
init [] = error "init called with an empty list"
init [_] = []
init (x : xs) = x : init xs

-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = xs : inits (init xs)

-- subsequences
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) =
  let tailSubs = subsequences xs
   in tailSubs ++ map (x :) tailSubs

-- any
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x : xs)
  | p x = True
  | otherwise = any p xs

-- all
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x : xs)
  | p x = all p xs
  | otherwise = False

-- and
and :: [Bool] -> Bool
and [] = True
and (b : bs) = b && and bs

-- or
or :: [Bool] -> Bool
or [] = False
or (b : bs) = b || or bs

-- concat
concat :: [[a]] -> [a]
concat [] = []
concat (l : ls) = l ++ concat ls

-- elem
-- (using the function 'any' above)
elem :: (Eq a) => a -> [a] -> Bool
elem x = any (== x)

-- elem'
-- same as elem but elementary definition (without using other functions except (==))
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys)
  | x == y = True
  | otherwise = elem' x ys

-- (!!)
(!!) :: (Integral i) => [a] -> i -> a
_ !! i | i < 0 = error "(!!) called with a negative index"
[] !! _ = error "(!!) called with an empty list"
(x : _) !! 0 = x
(_ : xs) !! i = xs !! (i - 1)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = error "cycle called with an empty list"
cycle xs = xs ++ cycle xs

-- repeat
repeat :: a -> [a]
repeat x = cycle [x]

-- replicate
replicate :: (Integral i) => i -> a -> [a]
replicate i x = take i (repeat x)

-- isPrefixOf
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- isInfixOf
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs (y : ys) = isPrefixOf xs (y : ys) || isInfixOf xs ys

-- isSuffixOf
isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf xs ys = reverse xs `isPrefixOf` reverse ys

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
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (x /=) xs)

-- splitAt
splitAt :: (Integral i) => i -> [a] -> ([a], [a])
splitAt i xs | i <= 0 = ([], xs)
splitAt _ [] = ([], [])
splitAt i (x : xs) =
  let (leftPart, rightPart) = splitAt (i - 1) xs
   in (x : leftPart, rightPart)

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break p (x : xs)
  | p x = ([], x : xs)
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
    s' ->
      let (word, rem) = break C.isSpace s'
       in word : words rem

-- unlines
unlines :: [String] -> String
unlines [] = ""
unlines s = intercalate "\n" s ++ "\n"

-- unwords
unwords :: [String] -> String
unwords = intercalate " "

-- transpose
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose matrix =
  let nonEmptyRows = filter (not . null) matrix
   in if null nonEmptyRows
        then []
        else map head nonEmptyRows : transpose (map tail nonEmptyRows)

-- normalize
normalize :: String -> String
normalize s = filter C.isAlphaNum (map C.toLower s)

-- palindrome
palindrome :: String -> Bool
palindrome s =
  let normS = normalize s
   in normS == reverse normS
