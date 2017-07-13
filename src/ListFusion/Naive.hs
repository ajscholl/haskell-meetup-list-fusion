module ListFusion.Naive (
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

import Prelude (Num(..), Ord(..), Int, otherwise, error)

{-

Simple implementations with a little bit of thought:

 - Use a worker function where possible (e.g. to use an accumulator instead of
   stack space)
 - Inline most of the functions (e.g. to specialize for function arguments)

-}

{-# INLINE sum #-}
sum :: Num a => [a] -> a
sum = foldl (+) 0

{-# ANN map "hlint: ignore Use map" #-}
{-# INLINE map #-}
map :: (a -> b) -> [a] -> [b]
map f = go
    where
        go []     = []
        go (x:xs) = f x : go xs

{-# INLINE take #-}
take :: Int -> [a] -> [a]
take n xs | n > 0     = unsafeTake n xs
          | otherwise = []
    where
        unsafeTake _ []     = []
        unsafeTake 0 _      = []
        unsafeTake m (y:ys) = y : unsafeTake (m - 1) ys

{-# INLINE drop #-}
drop :: Int -> [a] -> [a]
drop n xs | n > 0     = unsafeDrop n xs
          | otherwise = xs
    where
        unsafeDrop _ []     = []
        unsafeDrop 0 ys     = ys
        unsafeDrop m (_:ys) = unsafeDrop (m - 1) ys

{-# INLINE iterate #-}
iterate :: (a -> a) -> a -> [a]
iterate f = go
    where
        go x = x : go (f x)

{-# INLINE maximum #-}
maximum :: Ord a => [a] -> a
maximum []     = error "maximum: empty list"
maximum (x:xs) = go x xs
    where
        go y []     = y
        go y (z:zs) = go (max y z) zs

enumFromTo :: Int -> Int -> [Int]
enumFromTo from to | from <= to = from : enumFromTo (from + 1) to
                   | otherwise  = []

{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f = go
     where
        go z []     = z
        go z (x:xs) = go (f z x) xs

{-# INLINE replicate #-}
replicate :: Int -> a -> [a]
replicate n a | n > 0     = go n
              | otherwise = []
    where
        go 0 = []
        go m = a : go (m - 1)

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
