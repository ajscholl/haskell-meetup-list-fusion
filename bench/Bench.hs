module Main (main) where

import Control.Monad

import Criterion.Main
import Criterion.Types

import qualified ListFusion.Naive as N
import qualified ListFusion.LikeGHC as LG
import qualified ListFusion.LikeGHCWithRules as LGR
import qualified ListFusion.GHC as G

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

sqr :: Int -> Int
sqr x = x * x

-- * Sum of squares

-- | Straightforward implementation from FP 101. Maybe a little bit more optimized.
sumSquaresNaive :: (Int, Int) -> Int
sumSquaresNaive (from, to) = N.sum . N.map sqr . N.take (to - from + 1) $ N.iterate (+1) from

-- | Implementation using functions defined like those in GHC but without any rewrite rules.
sumSquaresLikeGHC :: (Int, Int) -> Int
sumSquaresLikeGHC (from, to) = LG.sum . LG.map sqr . LG.take (to - from + 1) $ LG.iterate (+1) from

-- | Implementation using list functions which should employ all (relevant) optimizations of GHCs implementation.
sumSquaresLikeGHCWithRules :: (Int, Int) -> Int
sumSquaresLikeGHCWithRules (from, to) = LGR.sum . LGR.map sqr . LGR.take (to - from + 1) $ LGR.iterate (+1) from

-- | Implementation using GHC's list functions.
sumSquaresGHC :: (Int, Int) -> Int
sumSquaresGHC (from, to) = G.sum . G.map sqr . G.take (to - from + 1) $ G.iterate (+1) from

-- | Implementation without lists, should be completely free of allocation.
sumSquaresNoList :: (Int, Int) -> Int
sumSquaresNoList (from, to) = go 0 from
    where
        go acc n | acc `seq` (n <= to) = go (acc + sqr n) (n + 1)
                 | otherwise           = acc

sumSquaresVector :: (Int, Int) -> Int
sumSquaresVector (from, to) = V.sum . V.map sqr $ V.iterateN (to - from + 1) (+1) from

sumSquaresUnboxedVector :: (Int, Int) -> Int
sumSquaresUnboxedVector (from, to) = VU.sum . VU.map sqr $ VU.iterateN (to - from + 1) (+1) from

sumSquaresStorableVector :: (Int, Int) -> Int
sumSquaresStorableVector (from, to) = VS.sum . VS.map sqr $ VS.iterateN (to - from + 1) (+1) from

-- * Maximum of list, take

maximumTakeNaive :: (Int, Int) -> Int
maximumTakeNaive (from, to) = N.maximum . N.map (\ x -> sqr x `rem` to) . N.take from $ N.enumFromTo 0 to

maximumTakeLikeGHC :: (Int, Int) -> Int
maximumTakeLikeGHC (from, to) = LG.maximum . LG.map (\ x -> sqr x `rem` to) . LG.take from $ LG.enumFromTo 0 to

maximumTakeLikeGHCWithRules :: (Int, Int) -> Int
maximumTakeLikeGHCWithRules (from, to) = LGR.maximum . LGR.map (\ x -> sqr x `rem` to) . LGR.take from $ LGR.enumFromTo 0 to

maximumTakeGHC :: (Int, Int) -> Int
maximumTakeGHC (from, to) = G.maximum . G.map (\ x -> sqr x `rem` to) . G.take from $ G.enumFromTo 0 to

maximumTakeNoList :: (Int, Int) -> Int
maximumTakeNoList (from, to) = to `seq` go (-1) 0
    where
        go acc n | acc `seq` (n < from) = go (max acc ((n * n) `rem` to)) (n + 1)
                 | otherwise            = acc

maximumTakeVector :: (Int, Int) -> Int
maximumTakeVector (from, to) = V.maximum . V.map (\ x -> sqr x `rem` to) . V.take from $ V.generate (to - 0 + 1) (+0)

maximumTakeUnboxedVector :: (Int, Int) -> Int
maximumTakeUnboxedVector (from, to) = VU.maximum . VU.map (\ x -> sqr x `rem` to) . VU.take from $ VU.generate (to - 0 + 1) (+0)

maximumTakeStorableVector :: (Int, Int) -> Int
maximumTakeStorableVector (from, to) = VS.maximum . VS.map (\ x -> sqr x `rem` to) . VS.take from $ VS.generate (to - 0 + 1) (+0)

-- * Maximum of list, drop

maximumDropNaive :: (Int, Int) -> Int
maximumDropNaive (from, to) = N.maximum . N.map (\ x -> sqr x `rem` to) . N.drop from $ N.enumFromTo 0 to

maximumDropLikeGHC :: (Int, Int) -> Int
maximumDropLikeGHC (from, to) = LG.maximum . LG.map (\ x -> sqr x `rem` to) . LG.drop from $ LG.enumFromTo 0 to

maximumDropLikeGHCWithRules :: (Int, Int) -> Int
maximumDropLikeGHCWithRules (from, to) = LGR.maximum . LGR.map (\ x -> sqr x `rem` to) . LGR.drop from $ LGR.enumFromTo 0 to

maximumDropGHC :: (Int, Int) -> Int
maximumDropGHC (from, to) = G.maximum . G.map (\ x -> sqr x `rem` to) . G.drop from $ G.enumFromTo 0 to

maximumDropNoList :: (Int, Int) -> Int
maximumDropNoList (from, to) = to `seq` go (-1) from
    where
        go acc n | acc `seq` (n <= to) = go (max acc ((n * n) `rem` to)) (n + 1)
                 | otherwise           = acc

maximumDropVector :: (Int, Int) -> Int
maximumDropVector (from, to) = V.maximum . V.map (\ x -> sqr x `rem` to) . V.drop from $ V.generate (to - 0 + 1) (+0)

maximumDropUnboxedVector :: (Int, Int) -> Int
maximumDropUnboxedVector (from, to) = VU.maximum . VU.map (\ x -> sqr x `rem` to) . VU.drop from $ VU.generate (to - 0 + 1) (+0)

maximumDropStorableVector :: (Int, Int) -> Int
maximumDropStorableVector (from, to) = VS.maximum . VS.map (\ x -> sqr x `rem` to) . VS.take from $ VS.generate (to - 0 + 1) (+0)

-- * Average

-- | Compute sum in first element, length in second element
avgHelper :: (Int, Int) -> Int -> (Int, Int)
avgHelper (acc, len) x = ((,) $! acc + x) $! (len + 1)

-- | Most complex way to compute (a + b) / 2? Notice foldl!
averageNaive :: (Int, Int, Int) -> Int
averageNaive (a, b, count) = uncurry quot . N.foldl avgHelper (0, 0) $ N.replicate count a N.++ N.replicate count b

averageLikeGHC :: (Int, Int, Int) -> Int
averageLikeGHC (a, b, count) = uncurry quot . LG.foldl avgHelper (0, 0) $ LG.replicate count a LG.++ LG.replicate count b

averageLikeGHCWithRules :: (Int, Int, Int) -> Int
averageLikeGHCWithRules (a, b, count) = uncurry quot . LGR.foldl avgHelper (0, 0) $ LGR.replicate count a LGR.++ LGR.replicate count b

averageGHC :: (Int, Int, Int) -> Int
averageGHC (a, b, count) = uncurry quot . G.foldl avgHelper (0, 0) $ G.replicate count a G.++ G.replicate count b

-- | Compute (a + b) / 2 similar to the functions which use lists but without using lists.
--   ((a + b) `quot` 2 would be too fast and not make any sense as a comparison).
averageNoList :: (Int, Int, Int) -> Int
averageNoList (a, b, count) = go1 count 0 0
    where
        go1 0 acc len = go2 count acc len
        go1 n acc len = (go1 (n - 1) $! (acc + a)) $! (len + 1)

        go2 0 acc len = acc `quot` len
        go2 n acc len = (go2 (n - 1) $! (acc + b)) $! (len + 1)

averageVector :: (Int, Int, Int) -> Int
averageVector (a, b, count) = uncurry quot . V.foldl avgHelper (0, 0) $ V.replicate count a V.++ V.replicate count b

averageUnboxedVector :: (Int, Int, Int) -> Int
averageUnboxedVector (a, b, count) = uncurry quot . VU.foldl avgHelper (0, 0) $ VU.replicate count a VU.++ VU.replicate count b

averageStorableVector :: (Int, Int, Int) -> Int
averageStorableVector (a, b, count) = uncurry quot . VS.foldl avgHelper (0, 0) $ VS.replicate count a VS.++ VS.replicate count b

-- * Benchmark

-- | Check whether all functions actually do the same
testSame :: [a -> Int] -> a -> IO ()
testSame []     _   = fail "What to test?"
testSame (x:xs) arg = do
    let r  = x arg
        rs = map ($ arg) xs
    when (any (r /=) rs) $
        fail $ "Inconsistent results: " ++ show rs
    putStrLn "passed"

critConfig :: Config
critConfig = defaultConfig { reportFile = Just $ "report.html" }

main :: IO ()
main = do
    testSame [sumSquaresNaive, sumSquaresLikeGHC, sumSquaresLikeGHCWithRules, sumSquaresGHC, sumSquaresNoList,
              sumSquaresVector, sumSquaresUnboxedVector, sumSquaresStorableVector] (0, 10000)
    testSame [maximumTakeNaive, maximumTakeLikeGHC, maximumTakeLikeGHCWithRules, maximumTakeGHC, maximumTakeNoList,
              maximumTakeVector, maximumTakeUnboxedVector, maximumTakeStorableVector] (5000, 10000)
    testSame [maximumDropNaive, maximumDropLikeGHC, maximumDropLikeGHCWithRules, maximumDropGHC, maximumDropNoList,
              maximumDropVector, maximumDropUnboxedVector, maximumDropStorableVector] (5000, 10000)
    testSame [averageNaive, averageLikeGHC, averageLikeGHCWithRules, averageGHC, averageNoList,
              averageVector, averageUnboxedVector, averageStorableVector] (5, 10, 5000)
    defaultMainWith critConfig
        [ bgroup "sumSquares"
            [ bench "naive"               $ whnf sumSquaresNaive            (0, 10000)
            , bench "like GHC"            $ whnf sumSquaresLikeGHC          (0, 10000)
            , bench "like GHC with rules" $ whnf sumSquaresLikeGHCWithRules (0, 10000)
            , bench "GHC"                 $ whnf sumSquaresGHC              (0, 10000)
            , bench "no-list"             $ whnf sumSquaresNoList           (0, 10000)
            , bench "vector"              $ whnf sumSquaresVector           (0, 10000)
            , bench "u-vector"            $ whnf sumSquaresUnboxedVector    (0, 10000)
            , bench "s-vector"            $ whnf sumSquaresStorableVector   (0, 10000)
            ]
        , bgroup "maximumTake"
            [ bench "naive"               $ whnf maximumTakeNaive            (5000, 10000)
            , bench "like GHC"            $ whnf maximumTakeLikeGHC          (5000, 10000)
            , bench "like GHC with rules" $ whnf maximumTakeLikeGHCWithRules (5000, 10000)
            , bench "GHC"                 $ whnf maximumTakeGHC              (5000, 10000)
            , bench "no-list"             $ whnf maximumTakeNoList           (5000, 10000)
            , bench "vector"              $ whnf maximumTakeVector           (5000, 10000)
            , bench "u-vector"            $ whnf maximumTakeUnboxedVector    (5000, 10000)
            , bench "s-vector"            $ whnf maximumTakeStorableVector   (5000, 10000)
            ]
        , bgroup "maximumDrop"
            [ bench "naive"               $ whnf maximumDropNaive            (5000, 10000)
            , bench "like GHC"            $ whnf maximumDropLikeGHC          (5000, 10000)
            , bench "like GHC with rules" $ whnf maximumDropLikeGHCWithRules (5000, 10000)
            , bench "GHC"                 $ whnf maximumDropGHC              (5000, 10000)
            , bench "no-list"             $ whnf maximumDropNoList           (5000, 10000)
            , bench "vector"              $ whnf maximumDropVector           (5000, 10000)
            , bench "u-vector"            $ whnf maximumDropUnboxedVector    (5000, 10000)
            , bench "s-vector"            $ whnf maximumDropStorableVector   (5000, 10000)
            ]
        , bgroup "average"
            [ bench "naive"               $ whnf averageNaive            (5, 10, 5000)
            , bench "like GHC"            $ whnf averageLikeGHC          (5, 10, 5000)
            , bench "like GHC with rules" $ whnf averageLikeGHCWithRules (5, 10, 5000)
            , bench "GHC"                 $ whnf averageGHC              (5, 10, 5000)
            , bench "no-list"             $ whnf averageNoList           (5, 10, 5000)
            , bench "vector"              $ whnf averageVector           (5, 10, 5000)
            , bench "u-vector"            $ whnf averageUnboxedVector    (5, 10, 5000)
            , bench "s-vector"            $ whnf averageStorableVector   (5, 10, 5000)
            ]
        ]
