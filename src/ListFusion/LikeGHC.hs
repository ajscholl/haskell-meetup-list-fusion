{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListFusion.LikeGHC (
     sum
    ,map
    ,take
    ,drop
    ,iterate
    ,maximum
    ,enumFromTo
    ,foldl
    ,replicate
    ,(++)
    ) where

import Prelude (Num(..), Ord(..), id, otherwise)
import GHC.Exts (Int(..), Int#, isTrue#, (+#), (>#), (==#))
import GHC.Magic (oneShot)
import GHC.List (errorEmptyList)


{-

Module containing the relevant chunks of code from base-4.9.1 without
any of the rewrite rules used by GHC.

-}

sum                     :: (Num a) => [a] -> a
{-# INLINE sum #-}
sum                     =  foldl (+) 0

map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [0] map #-}
  -- We want the RULEs "map" and "map/coerce" to fire first.
  -- map is recursive, so won't inline anyway,
  -- but saying so is more explicit, and silences warnings
map _ []     = []
map f (x:xs) = f x : map f xs


take                   :: Int -> [a] -> [a]
{- We always want to inline this to take advantage of a known length argument
sign. Note, however, that it's important for the RULES to grab take, rather
than trying to INLINE take immediately and then letting the RULES grab
unsafeTake. Presumably the latter approach doesn't grab it early enough; it led
to an allocation regression in nofib/fft2. -}
{-# INLINE [1] take #-}
take n xs | 0 < n     = unsafeTake n xs
          | otherwise = []

{-# NOINLINE [1] unsafeTake #-}
unsafeTake :: Int -> [a] -> [a]
unsafeTake !_  []     = []
unsafeTake 1   (x: _) = [x]
unsafeTake m   (x:xs) = x : unsafeTake (m - 1) xs

drop                   :: Int -> [a] -> [a]
{-# INLINE drop #-}
drop n ls
  | n <= 0     = ls
  | otherwise  = unsafeDrop n ls
  where
    -- A version of drop that drops the whole list if given an argument
    -- less than 1
    unsafeDrop :: Int -> [a] -> [a]
    unsafeDrop !_ []     = []
    unsafeDrop 1  (_:xs) = xs
    unsafeDrop m  (_:xs) = unsafeDrop (m - 1) xs

{-# INLINE iterate #-}
iterate :: (a -> a) -> a -> [a]
iterate f = go
    where
        go x = x : go (f x)

{-# ANN maximum "hlint: ignore Use maximum" #-}
maximum                 :: (Ord a) => [a] -> a
{-# INLINEABLE maximum #-}
maximum []              =  errorEmptyList "maximum"
maximum xs              =  foldl1 max xs

-- We want this to be specialized so that with a strict max function, GHC
-- produces good code. Note that to see if this is happending, one has to
-- look at -ddump-prep, not -ddump-core!
{-# SPECIALIZE  maximum :: [Int] -> Int #-}

foldl1                  :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)         =  foldl f x xs
foldl1 _ []             =  errorEmptyList "foldl1"

{-# INLINE enumFromTo #-}
enumFromTo :: Int -> Int -> [Int]
enumFromTo (I# x) (I# y) = eftInt x y

{-# NOINLINE [1] eftInt #-}
eftInt :: Int# -> Int# -> [Int]
-- [x1..x2]
eftInt x0 y | isTrue# (x0 ># y) = []
            | otherwise         = go x0
               where
                 go x = I# x : if isTrue# (x ==# y)
                               then []
                               else go (x +# 1#)

foldr            :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []     =  z
-- foldr f z (x:xs) =  f x (foldr f z xs)
{-# INLINE [0] foldr #-}
-- Inline only in the final stage, after the foldr/cons rule has had a chance
-- Also note that we inline it when it has *two* parameters, which are the
-- ones we are keen about specialising!
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
{-# INLINE foldl #-}
foldl k z0 xs =
  foldr (\(v::a) (fn::b->b) -> oneShot (\(z::b) -> fn (k z v))) (id :: b -> b) xs z0

repeat :: a -> [a]
{-# INLINE [0] repeat #-}
-- The pragma just gives the rules more chance to fire
repeat x = xs where xs = x : xs

{-# INLINE replicate #-}
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

(++) :: [a] -> [a] -> [a]
{-# NOINLINE [1] (++) #-}    -- We want the RULE to fire first.
                             -- It's recursive, so won't inline anyway,
                             -- but saying so is more explicit
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys
