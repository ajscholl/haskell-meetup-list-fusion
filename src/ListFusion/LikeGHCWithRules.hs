{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListFusion.LikeGHCWithRules (
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

import Prelude (Num(..), Ord(..), id, otherwise, (.))
import GHC.Exts (Int(..), Int#, isTrue#, (+#), (>#), (==#))
import GHC.Magic (oneShot)
import GHC.List (errorEmptyList)
import Data.Coerce (coerce)

{-# ANN module "hlint: ignore" #-}

{-

Module containing the relevant chunks of code from base-4.9.1 as well
as the rewrite rules used by GHC.

-}

sum                     :: (Num a) => [a] -> a
{-# INLINE sum #-}
sum                     =  foldl (+) 0

-- | A list producer that can be fused with 'foldr'.
-- This function is merely
--
-- >    build g = g (:) []
--
-- but GHC's simplifier will transform an expression of the form
-- @'foldr' k z ('build' g)@, which may arise after inlining, to @g k z@,
-- which avoids producing an intermediate list.

build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
        -- The INLINE is important, even though build is tiny,
        -- because it prevents [] getting inlined in the version that
        -- appears in the interface file.  If [] *is* inlined, it
        -- won't match with [] appearing in rules in an importing module.
        --
        -- The "1" says to inline in phase 1

build g = g (:) []

-- | A list producer that can be fused with 'foldr'.
-- This function is merely
--
-- >    augment g xs = g (:) xs
--
-- but GHC's simplifier will transform an expression of the form
-- @'foldr' k z ('augment' g xs)@, which may arise after inlining, to
-- @g k ('foldr' k z xs)@, which avoids producing an intermediate list.

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE [1] augment #-}
augment g xs = g (:) xs

{-# RULES
"fold/build"    forall k z (g::forall b. (a->b->b) -> b -> b) .
                foldr k z (build g) = g k z

"foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) .
                foldr k z (augment g xs) = g k (foldr k z xs)

"foldr/id"                        foldr (:) [] = \x  -> x
"foldr/app"     [1] forall ys. foldr (:) ys = \xs -> xs ++ ys
        -- Only activate this from phase 1, because that's
        -- when we disable the rule that expands (++) into foldr

-- The foldr/cons rule looks nice, but it can give disastrously
-- bloated code when commpiling
--      array (a,b) [(1,2), (2,2), (3,2), ...very long list... ]
-- i.e. when there are very very long literal lists
-- So I've disabled it for now. We could have special cases
-- for short lists, I suppose.
-- "foldr/cons" forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)

"foldr/single"  forall k z x. foldr k z [x] = k x z
"foldr/nil"     forall k z.   foldr k z []  = z

"foldr/cons/build" forall k z x (g::forall b. (a->b->b) -> b -> b) .
                           foldr k z (x:build g) = k x (g k z)

"augment/build" forall (g::forall b. (a->b->b) -> b -> b)
                       (h::forall b. (a->b->b) -> b -> b) .
                       augment g (build h) = build (\c n -> g c (h c n))
"augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .
                        augment g [] = build g
 #-}

-- This rule is true, but not (I think) useful:
--      augment g (augment h t) = augment (\cn -> g c (h c n)) t

----------------------------------------------
--              map
----------------------------------------------

-- | 'map' @f xs@ is the list obtained by applying @f@ to each element
-- of @xs@, i.e.,
--
-- > map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- > map f [x1, x2, ...] == [f x1, f x2, ...]

map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [0] map #-}
  -- We want the RULEs "map" and "map/coerce" to fire first.
  -- map is recursive, so won't inline anyway,
  -- but saying so is more explicit, and silences warnings
map _ []     = []
map f (x:xs) = f x : map f xs

-- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-}
mapFB c f = \x ys -> c (f x) ys

-- The rules for map work like this.
--
-- Up to (but not including) phase 1, we use the "map" rule to
-- rewrite all saturated applications of map with its build/fold
-- form, hoping for fusion to happen.
-- In phase 1 and 0, we switch off that rule, inline build, and
-- switch on the "mapList" rule, which rewrites the foldr/mapFB
-- thing back into plain map.
--
-- It's important that these two rules aren't both active at once
-- (along with build's unfolding) else we'd get an infinite loop
-- in the rules.  Hence the activation control below.
--
-- The "mapFB" rule optimises compositions of map.
--
-- This same pattern is followed by many other functions:
-- e.g. append, filter, iterate, repeat, etc.

{-# RULES
"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
  #-}

-- See Breitner, Eisenberg, Peyton Jones, and Weirich, "Safe Zero-cost
-- Coercions for Haskell", section 6.5:
--   http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf

{-# RULES "map/coerce" [1] map coerce = coerce #-}


----------------------------------------------
--              append
----------------------------------------------

-- | Append two lists, i.e.,
--
-- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
-- > [x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]
--
-- If the first list is not finite, the result is the first list.

(++) :: [a] -> [a] -> [a]
{-# NOINLINE [1] (++) #-}    -- We want the RULE to fire first.
                             -- It's recursive, so won't inline anyway,
                             -- but saying so is more explicit
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

{-# RULES
"++"    [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}



-- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
-- of length @n@, or @xs@ itself if @n > 'length' xs@:
--
-- > take 5 "Hello World!" == "Hello"
-- > take 3 [1,2,3,4,5] == [1,2,3]
-- > take 3 [1,2] == [1,2]
-- > take 3 [] == []
-- > take (-1) [1,2] == []
-- > take 0 [1,2] == []
--
-- It is an instance of the more general 'Data.List.genericTake',
-- in which @n@ may be of any integral type.
take                   :: Int -> [a] -> [a]
{- We always want to inline this to take advantage of a known length argument
sign. Note, however, that it's important for the RULES to grab take, rather
than trying to INLINE take immediately and then letting the RULES grab
unsafeTake. Presumably the latter approach doesn't grab it early enough; it led
to an allocation regression in nofib/fft2. -}
{-# INLINE [1] take #-}
take n xs | 0 < n     = unsafeTake n xs
          | otherwise = []

-- A version of take that takes the whole list if it's given an argument less
-- than 1.
{-# NOINLINE [1] unsafeTake #-}
unsafeTake :: Int -> [a] -> [a]
unsafeTake !_  []     = []
unsafeTake 1   (x: _) = [x]
unsafeTake m   (x:xs) = x : unsafeTake (m - 1) xs

{-# RULES
"take"     [~1] forall n xs . take n xs =
  build (\c nil -> if 0 < n
                   then foldr (takeFB c nil) (flipSeqTake nil) xs n
                   else nil)
"unsafeTakeList"  [1] forall n xs . foldr (takeFB (:) []) (flipSeqTake []) xs n
                                        = unsafeTake n xs
 #-}

{-# INLINE [0] flipSeqTake #-}
-- Just flip seq, specialized to Int, but not inlined too early.
-- It's important to force the numeric argument here, even though
-- it's not used. Otherwise, take n [] doesn't force n. This is
-- bad for strictness analysis and unboxing, and leads to increased
-- allocation in T7257.
flipSeqTake :: a -> Int -> a
flipSeqTake x !_n = x

{-# INLINE [0] takeFB #-}
takeFB :: (a -> b -> b) -> b -> a -> (Int -> b) -> Int -> b
-- The \m accounts for the fact that takeFB is used in a higher-order
-- way by takeFoldr, so it's better to inline.  A good example is
--     take n (repeat x)
-- for which we get excellent code... but only if we inline takeFB
-- when given four arguments
takeFB c n x xs
  = \ m -> case m of
            1 -> x `c` n
            _ -> x `c` xs (m - 1)

-- | 'drop' @n xs@ returns the suffix of @xs@
-- after the first @n@ elements, or @[]@ if @n > 'length' xs@:
--
-- > drop 6 "Hello World!" == "World!"
-- > drop 3 [1,2,3,4,5] == [4,5]
-- > drop 3 [1,2] == []
-- > drop 3 [] == []
-- > drop (-1) [1,2] == [1,2]
-- > drop 0 [1,2] == [1,2]
--
-- It is an instance of the more general 'Data.List.genericDrop',
-- in which @n@ may be of any integral type.
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



{-# NOINLINE [1] iterate #-}
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)

{-# NOINLINE [0] iterateFB #-}
iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x0 = go x0
  where go x = x `c` go (f x)

{-# RULES
"iterate"    [~1] forall f x.   iterate f x = build (\c _n -> iterateFB c f x)
"iterateFB"  [1]                iterateFB (:) = iterate
 #-}


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

-----------------------------------------------------
-- eftInt and eftIntFB deal with [a..b], which is the
-- most common form, so we take a lot of care
-- In particular, we have rules for deforestation

{-# RULES
"eftInt"        [~1] forall x y. eftInt x y = build (\ c n -> eftIntFB c n x y)
"eftIntList"    [1] eftIntFB  (:) [] = eftInt
 #-}

{- Note [How the Enum rules work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Phase 2: eftInt ---> build . eftIntFB
* Phase 1: inline build; eftIntFB (:) --> eftInt
* Phase 0: optionally inline eftInt
-}

{-# NOINLINE [1] eftInt #-}
eftInt :: Int# -> Int# -> [Int]
-- [x1..x2]
eftInt x0 y | isTrue# (x0 ># y) = []
            | otherwise         = go x0
               where
                 go x = I# x : if isTrue# (x ==# y)
                               then []
                               else go (x +# 1#)

{-# INLINE [0] eftIntFB #-}
eftIntFB :: (Int -> r -> r) -> r -> Int# -> Int# -> r
eftIntFB c n x0 y | isTrue# (x0 ># y) = n
                  | otherwise         = go x0
                 where
                   go x = I# x `c` if isTrue# (x ==# y)
                                   then n
                                   else go (x +# 1#)
                        -- Watch out for y=maxBound; hence ==, not >
        -- Be very careful not to have more than one "c"
        -- so that when eftInfFB is inlined we can inline
        -- whatever is bound to "c"

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

-- | 'repeat' @x@ is an infinite list, with @x@ the value of every element.
repeat :: a -> [a]
{-# INLINE [0] repeat #-}
-- The pragma just gives the rules more chance to fire
repeat x = xs where xs = x : xs

{-# INLINE [0] repeatFB #-}     -- ditto
repeatFB :: (a -> b -> b) -> a -> b
repeatFB c x = xs where xs = x `c` xs

{-# RULES
"repeat"    [~1] forall x. repeat x = build (\c _n -> repeatFB c x)
"repeatFB"  [1]  repeatFB (:)       = repeat
 #-}


-- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- every element.
-- It is an instance of the more general 'Data.List.genericReplicate',
-- in which @n@ may be of any integral type.
{-# INLINE replicate #-}
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)
