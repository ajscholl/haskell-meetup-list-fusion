module ListFusion.GHC (
     sum
    ,map
    ,take
    ,drop
    ,iterate
    ,maximum
    ,ListFusion.GHC.enumFromTo
    ,foldl
    ,replicate
    ,(++)
    ) where

import Prelude as P

{-

Reexport GHCs implementation for most of the functions.
For enumFromTo, we first specialize it (and inline it directly)
to make sure we have the correct types.

-}

{-# INLINE enumFromTo #-}
enumFromTo :: Int -> Int -> [Int]
enumFromTo = P.enumFromTo
