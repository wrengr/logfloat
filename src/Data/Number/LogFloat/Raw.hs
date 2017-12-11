{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# OPTIONS_GHC -O2 -fexcess-precision -fenable-rewrite-rules #-}

----------------------------------------------------------------
--                                                  ~ 2017.12.10
-- |
-- Module      :  Data.Number.LogFloat.Raw
-- Copyright   :  Copyright (c) 2007--2017 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable (with CPP, FFI)
--
-- This module provides implementations for computing various
-- logarithmic and exponential functions without losing precision
-- (as the naive implementations do). Since we can't rely on types
-- to clarify things, we use the traditional baroque names for
-- things. The design considerations behind (most of) these
-- implementations are documented at:
-- <https://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf>
--
-- In base-4.9.0.0 GHC added some of these to the 'Floating' class
-- exported from "Numeric". Alas, they provide default definitions
-- using the naive implementations, so one can't really rely on the
-- 'Floating' methods being precision presrving. Overall, the
-- specific instance for 'Double' looks fine (though they use
-- different cutoffs for 'log1pexp' for some reason); but it's easy
-- enough to reimplement here, to make absolutely sure we're getting
-- the right thing.
--
-- /Since: 0.14.0/
----------------------------------------------------------------
module LogDomain
    ( expm1
    , log1p
    , log1mexp
    , log1pexp
    , sigmoid
    , logit
    , logitExp
    , logSumExp
    , kahanSum
    , neumaierSum
    , logSoftmax
    , softmax
    ) where

import Data.List (foldl')

----------------------------------------------------------------
-- | Compute @exp x - 1@ without losing precision.
foreign import ccall unsafe "math.h expm1"
    expm1 :: Double -> Double


-- TODO: verify that the Haddock comes out as intended...
-- | Compute @log (1 + x)@ without losing precision.
foreign import ccall unsafe "math.h log1p"
    log1p
        :: Double -- ^ N.B., only defined on the @[-1,infty]@ interval.
        -> Double


-- | Compute @log (1 - exp x)@ without losing precision.
log1mexp
    :: Double -- ^ N.B., only defined on the @[-infty,0]@ interval.
    -> Double
log1mexp x
    | x <= log 2 = (log . negate . expm1) x
    | otherwise  = (log1p . negate . exp) x
{-# INLINE log1mexp #-}


-- | Compute @log (1 + exp x)@ without losing precision.
log1pexp :: Double -> Double
log1pexp x
    | x <= -37  = exp x
    | x <= 18   = log1p (exp x)
    | x <= 33.3 = x + exp (negate x)
    | otherwise = x
{-# INLINE log1pexp #-}


-- | The logistic function; aka, the inverse of 'logit'.
-- > sigmoid x = 1 / (1 + exp (-x))
-- > sigmoid x = exp x / (exp x + 1)
-- > sigmoid x = (1 + tanh (x/2)) / 2
sigmoid :: Double -> Double
sigmoid x = (1 + tanh (x/2)) / 2
{-# INLINE sigmoid #-}
-- We prefer the 'tanh'-based definition because it's (exactly!)
-- symmetric about zero, whereas the naive version isn't (due to
-- floating-point fuzz).
-- TODO(b/68203642): Properly analyze the accuracy and precision
-- of the 'tanh' version.


-- | The quantile function; aka, the inverse of 'sigmoid'.
-- > logit x = log (x / (1 - x))
-- > logit x = 2 * atanh (2*x - 1)
logit
    :: Double -- ^ N.B., only defined on the @[0,1]@ interval.
    -> Double
logit x = 2 * atanh (2*x - 1)
{-# INLINE logit #-}
-- TODO(b/68203642): properly analyze the precision of the 'atanh' version.


-- | A variant of 'logit' for when the argument is already in the
-- log-domain; hence, @logitExp = logit . exp@
logitExp
    :: Double -- ^ N.B., only defined on the @[-infty,0]@ interval.
    -> Double
logitExp x = x - log1mexp x
{-# INLINE logitExp #-}
-- TODO(b/68203642): properly analyze the precision of this
-- implementation with respect to the 'logit' implementation.


-- | A helper type for 'logSumExp'. As a semigroup, this is isomorphic to:
-- @(WrappedMonoid (Sum Int), Max Double)@; however, we strictify and
-- flatten everything to improve performance.
data LSE = LSE
    {-# UNPACK #-}!Int    -- The length, minus one.
    {-# UNPACK #-}!Double -- The maximum.

-- | Compute the length and maximum of a list. This is a semigroup
-- reduction. However we roll it ourselves rather than using the
-- semigroup class: since that would incur an otherwise unnecessary
-- dependency on @base >= 4.9.0.0@.
foldLSE :: Double -> [Double] -> LSE
foldLSE = foldl' step . LSE 0
    where
    step (LSE lm1 m) x = LSE (l + 1) (m `max` x)


-- | /O(n)/. Log-domain summation, aka: @(log . sum . fmap exp)@.
--
-- N.B., this requires two passes over the data: one for computing the
-- length and maximum, and one for the summation itself.
logSumExp :: [Double] -> Double
logSumExp []         = (-1)/0
logSumExp xs0@(x:xs) =
    case foldLSE x xs of
    LSE lm1 m
        | isInfinite m -> m
        | otherwise    ->
            -- TODO: push the addition of @lm1@ into the 'kahanSum',
            -- but making sure to add it in only at the very end.
            -- TODO: would using 'neumaierSum' be better? Should
            -- we factor the summation function out as an argument?
            -- TODO: is using 'log1p' here /really/ any better than
            -- just using 'log'?
            -- TODO: does that 'fmap' properly fuse into the
            -- 'kahanSum', or need we inline it ourselves?
            m + log1p (fromIntegral lm1 + kahanSum (fmap (expm1 . subtract m) xs0))


{-
-- TODO(wrengr): Compare precision of the following implementations.
-- We need to make sure to structure it in such a way that the @m@
-- doesn't obliterate the whole purpose of using @exp (x - m)@ in
-- the first place; but supposing we can do that, then it might
-- could help

sumExp = exp . logSumExp

sumExp []         = 0
sumExp xs0@(x:xs) =
    case foldLSE x xs of
    LSE lm1 m
        | isInfinite m -> m
        | otherwise    ->
            exp m * kahanSum (fromIntegral lm1 : fmap (expm1 . subtract m) xs0)
-}


-- | /O(n)/. Log-domain softmax, aka: @(fmap log . softmax)@.
--
-- N.B., this requires three passes over the data: two for the
-- 'logSumExp', and another for the normalization of the vector.
logSoftmax :: [Double] -> [Double]
logSoftmax xs = let z = logSumExp xs in z `seq` fmap (subtract z) xs
-- TODO(wrengr): alternatively we could use a variant of 'logSumExp'
-- which doesn't add the maximum back in, and do the final rescaling
-- by subtracting both the maximum and the summation; that is, a more
-- efficient\/straightforward variant of:
-- > logSoftmax xs =
-- >   subtract z <$> xs' -- aka @subtract (m + z) <$> xs@
-- >   where
-- >   m   = maximum xs
-- >   xs' = subtract m <$> xs
-- >   z   = logSumExp xs'
-- This works because for any constant @c@, @softmax xs == softmax ((+c)
-- <$> xs)@. Of course, I don't know that doing that would really help
-- precision by much (given the improved performance of using 'logSumExp'
-- in the first place), and saving a single add won't really matter
-- performance-wise. Perhaps if instead of the thing just proposed
-- about avoiding adding the max back in, what if instead we did things
-- exactly as written above: so we subtract off the maximum, but then
-- also do 'logSumExp' such that it subtracts off the maximum of those
-- differences. We could get the top-2 maxima in a single pass without
-- much extra work; but again, unclear whether it'd really help...


-- | /O(n)/. Normal-domain softmax:
-- > softmax xs = [ exp x / sum [ exp y | y <- xs] | x <- xs ]
--
-- N.B., this requires three passes over the data: same as 'logSoftmax'.
softmax :: [Double] -> [Double]
softmax = fmap exp . logSoftmax


-- TODO: double check that everything inlines away, so this data
-- type doesn't introduce any slowdown.
--
-- | A helper type for 'kahanSum'. As a data type, this is really
-- just so we can phrase things as using 'foldl''.
data Kahan = Kahan
    {-# UNPACK #-}!Double -- The total.
    {-# UNPACK #-}!Double -- The error correction.

kahanZero :: Kahan
kahanZero = Kahan 0 0
{-# INLINE kahanZero #-}

kahanPlus :: Kahan -> Double -> Kahan
kahanPlus (Kahan t c) x = Kahan t' c'
    where
    -- Beware this getting incorrectly optimized away by constant folding!
    x' = x - c
    t' = t + x'
    c' = (t' - t) - x'
{-# INLINE kahanPlus #-}

fromKahan :: Kahan -> Double
fromKahan (Kahan t _) = t
{-# INLINE fromKahan #-}

-- | /O(n)/. Floating-point summation, via Kahan's algorithm. This
-- is nominally equivalent to 'sum', but greatly mitigates the
-- problem of losing precision.
kahanSum :: [Double] -> Double
kahanSum = fromKahan . foldl' kahanPlus kahanZero

-- TODO: bring back the 'neumaierSum'
-- TODO: bring back 'expm1c' and 'log1pc'
