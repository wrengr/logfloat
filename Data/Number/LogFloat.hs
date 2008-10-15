-- TODO: Add QuickCheck-ness, though beware of the fuzz.
-- TODO: Make sure rewrite rules really fire
-- TODO: profile to make sure we don't waste too much time constructing
--       dictionaries
-- TODO: write strict variant to unpack into registers
-- TODO: write the signed variant

-- Needed by our RealToFrac contexts
{-# LANGUAGE FlexibleContexts #-}

-- To turn on optimizations and look at the optimization records, cf:
-- http://www.haskell.org/ghc/docs/latest/html/users_guide/rewrite-rules.html
-- http://www.randomhacks.net/articles/2007/02/10/map-fusion-and-haskell-performance

-- {-# OPTIONS_GHC -ddump-rules -ddump-simpl-stats #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

-- Unfortunately we need -fglasgow-exts in order to actually pick
-- up on the rules (see -ddump-rules). The -frewrite-rules flag
-- doesn't do what you want.
-- cf <http://hackage.haskell.org/trac/ghc/ticket/2213>
-- cf <http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg14313.html>
{-# OPTIONS_GHC -O2 -fvia-C -optc-O3 -fexcess-precision -fglasgow-exts #-}

-- Version History
-- (v0.9.1) Fixed some PartialOrd stuff and sanitized documentation
-- (v0.9.0) s/toFractional/realToFrac/g.
--          Also moved realToFrac and log to Transfinite
-- (v0.8.6) Removed buggy RULES
-- (v0.8.5) Gave up and converted from lhs to hs so Hackage docs work
-- (v0.8.4) Broke out Transfinite
-- (v0.8.3) Documentation updates
-- (v0.8.2) Announced release
-- (v0.8) Did a bunch of tweaking. Things should be decent now
-- (v0.7) Haddockified
-- (v0.6) Fixed monomorphism.
-- (v0.5) Added optimization rules.
-- (v0.4) Translated to Haskell at revision 2007.12.20.
-- (v0.3) Converted extensive comments to POD format.
-- (v0.2) Did a bunch of profiling, optimizing, and debugging.
-- (v0.1) Initial version created for hw5 for NLP with Jason Eisner.
--
----------------------------------------------------------------
--                                                  ~ 2008.08.29
-- |
-- Module      :  Data.Number.LogFloat
-- Copyright   :  Copyright (c) 2007--2008 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module presents a type for storing numbers in the log-domain.
-- The main reason for doing this is to prevent underflow when
-- multiplying many small probabilities as is done in Hidden Markov
-- Models and other statistical models often used for natural
-- language processing. The log-domain also helps prevent overflow
-- when multiplying many large numbers. In rare cases it can speed
-- up numerical computation (since addition is faster than
-- multiplication, though logarithms are exceptionally slow), but
-- the primary goal is to improve accuracy of results. A secondary
-- goal has been to maximize efficiency since these computations
-- are frequently done within a /O(n^3)/ loop.
--
-- The 'LogFloat' of this module is restricted to non-negative
-- numbers for efficiency's sake, see the forthcoming
-- "Data.Number.LogFloat.Signed" for doing signed log-domain
-- calculations.
----------------------------------------------------------------

module Data.Number.LogFloat
    (
    -- * Exceptional numeric values
      module Data.Number.Transfinite
    
    -- * @LogFloat@ data type and conversion functions
    , LogFloat
    , logFloat,     logToLogFloat
    , fromLogFloat, logFromLogFloat
    ) where

import Prelude hiding    (log, isNaN, realToFrac)
import qualified Prelude (isNaN)

import Data.Number.Transfinite
import Data.Number.PartialOrd

----------------------------------------------------------------
--
-- Try to add in some optimizations. Why these need to be
-- down here and localized to the module, I don't know. We don't
-- do anything foolish like this, but our clients might, or they
-- might be generated by other code transformations. Note that due
-- to the fuzz, these equations are not strictly true, even though
-- they are mathematically correct.

{-# RULES
"log/exp"  forall x. log (exp x) = x
"log.exp"            log . exp   = id

"exp/log"  forall x. exp (log x) = x
"exp.log"            exp . log   = id
    #-}

-- We'd like to be able to take advantage of general rule versions
-- of our operators for 'LogFloat', with rules like @log x + log y
-- = log (x * y)@ and @log x - log y = log (x / y)@. However the
-- problem is that those equations could be driven in either direction
-- depending on whether we think time performance or non-underflow
-- performance is more important, and the answers may be different
-- at every call site.
--
-- Since we implore users to do normal-domain computations whenever
-- it would not degenerate accuracy, we should not rewrite their
-- decisions in any way. The log\/exp fusion strictly improves both
-- time and accuracy, so those are safe. But the buck stops with
-- them.


-- These should only fire when it's type-safe
-- This should already happen, but...
-- TODO: Check the logs to see if it ever fires
-- N.B. these are orphaned
{-# RULES
"toRational/fromRational"  forall x. toRational (fromRational x) = x
"toRational.fromRational"            toRational . fromRational   = id
    #-}


----------------------------------------------------------------
--
-- | Reduce the number of constant string literals we need to store.

errorOutOfRange    :: String -> a
errorOutOfRange fun = error $! "Data.Number.LogFloat."++fun
                            ++ ": argument out of range"


-- | We need these guards in order to ensure some invariants.

guardNonNegative      :: String -> Double -> Double
guardNonNegative fun x | x >= 0    = x
                       | otherwise = errorOutOfRange fun


-- |  It's unfortunate that 'notANumber' is not equal to itself, but
-- we can hack around that. GHC gives NaN for the log of negatives
-- and so we could ideally take advantage of @log . guardNonNegative
-- fun = guardIsANumber fun . log@ to simplify things, but Hugs
-- raises an error so that's non-portable.

guardIsANumber        :: String -> Double -> Double
guardIsANumber   fun x | Prelude.isNaN x = errorOutOfRange fun
                       | otherwise       = x

----------------------------------------------------------------
--
-- | A @LogFloat@ is just a 'Double' with a special interpretation.
-- The 'logFloat' function is presented instead of the constructor,
-- in order to ensure semantic conversion. At present the 'Show'
-- instance will convert back to the normal-domain, and so will
-- underflow at that point. This behavior may change in the future.
--
-- Performing operations in the log-domain is cheap, prevents
-- underflow, and is otherwise very nice for dealing with miniscule
-- probabilities. However, crossing into and out of the log-domain
-- is expensive and should be avoided as much as possible. In
-- particular, if you're doing a series of multiplications as in
-- @lp * logFloat q * logFloat r@ it's faster to do @lp * logFloat
-- (q * r)@ if you're reasonably sure the normal-domain multiplication
-- won't underflow, because that way you enter the log-domain only
-- once, instead of twice.
--
-- Even more particularly, you should /avoid addition/ whenever
-- possible. Addition is provided because it's necessary at times
-- and the proper implementation is not immediately transparent.
-- However, between two @LogFloat@s addition requires crossing the
-- exp\/log boundary twice; with a @LogFloat@ and a regular number
-- it's three times since the regular number needs to enter the
-- log-domain first. This makes addition incredibly slow. Again,
-- if you can parenthesize to do plain operations first, do it!

newtype LogFloat = LogFloat Double
    deriving (Eq, Ord) -- Should we really perpetuate the Ord lie?

instance PartialOrd LogFloat where
    cmp (LogFloat x) (LogFloat y) 
        | isNaN x || isNaN y = Nothing
        | otherwise          = Just $! x `compare` y


-- | A constructor which does semantic conversion from normal-domain
-- to log-domain.

{-# SPECIALIZE logFloat :: Double -> LogFloat #-}
logFloat :: (Real a, RealToFrac a Double) => a -> LogFloat
logFloat  = LogFloat . log . guardNonNegative "logFloat" . realToFrac


-- This is simply a polymorphic version of the 'LogFloat' data
-- constructor. We present it mainly because we hide the constructor
-- in order to make the type a bit more opaque. If the polymorphism
-- turns out to be a performance liability because the rewrite rules
-- can't remove it, then we need to rethink all four
-- constructors\/destructors.
--
-- | Constructor which assumes the argument is already in the
-- log-domain.

{-# SPECIALIZE logToLogFloat :: Double -> LogFloat #-}
logToLogFloat :: (Real a, RealToFrac a Double) => a -> LogFloat
logToLogFloat  = LogFloat . guardIsANumber "logToLogFloat" . realToFrac


-- | Return our log-domain value back into normal-domain. Beware
-- of overflow\/underflow.

{-# SPECIALIZE fromLogFloat :: LogFloat -> Double #-}
fromLogFloat :: (Fractional a, Transfinite a, RealToFrac Double a)
             => LogFloat -> a
fromLogFloat (LogFloat x) = realToFrac (exp x)


-- | Return the log-domain value itself without costly conversion

{-# SPECIALIZE logFromLogFloat :: LogFloat -> Double #-}
logFromLogFloat :: (Fractional a, Transfinite a, RealToFrac Double a)
                => LogFloat -> a
logFromLogFloat (LogFloat x) = realToFrac x


-- These are our module-specific versions of "log\/exp" and "exp\/log";
-- They do the same things but also have a @LogFloat@ in between
-- the logarithm and exponentiation.
--
-- In order to ensure these rules fire we may need to delay inlining
-- of the four con-\/destructors, like we do for 'realToFrac'.
-- Unfortunately, I'm not entirely sure whether they will be inlined
-- already or not (and whether they are may be fragile) and I don't
-- want to inline them excessively and lead to code bloat in the
-- off chance that we could prune some of it away.
-- TODO: thoroughly investigate this.

{-# RULES
-- Out of log-domain and back in
"log/fromLogFloat"       forall x. log (fromLogFloat x) = logFromLogFloat x
"log.fromLogFloat"                 log . fromLogFloat   = logFromLogFloat

"logFloat/fromLogFloat"  forall x. logFloat (fromLogFloat x) = x
"logFloat.fromLogFloat"            logFloat . fromLogFloat   = id

-- Into log-domain and back out
"fromLogFloat/logFloat"  forall x. fromLogFloat (logFloat x) = x
"fromLogFloat.logFloat"            fromLogFloat . logFloat   = id
    #-}

----------------------------------------------------------------
-- To show it, we want to show the normal-domain value rather than
-- the log-domain value. Also, if someone managed to break our
-- invariants (e.g. by passing in a negative and noone's pulled on
-- the thunk yet) then we want to crash before printing the
-- constructor, rather than after.  N.B. This means the show will
-- underflow\/overflow in the same places as normal doubles since
-- we underflow at the @exp@. Perhaps this means we should show the
-- log-domain value instead.

instance Show LogFloat where
    show (LogFloat x) = let y = exp x
                        in  y `seq` "LogFloat "++show y


----------------------------------------------------------------
-- These all work without causing underflow. However, do note that
-- they tend to induce more of the floating-point fuzz than using
-- regular floating numbers because @exp . log@ doesn't really equal
-- @id@. In any case, our main aim is for preventing underflow when
-- multiplying many small numbers (and preventing overflow for
-- multiplying many large numbers) so we're not too worried about
-- +\/- 4e-16.

instance Num LogFloat where 
    (*) (LogFloat x) (LogFloat y) = LogFloat (x+y)

    (+) (LogFloat x) (LogFloat y)
        | x >= y    = LogFloat (x + log (1 + exp (y - x)))
        | otherwise = LogFloat (y + log (1 + exp (x - y)))

    -- Without the guard this would return NaN instead of error
    (-) (LogFloat x) (LogFloat y)
        | x >= y    = LogFloat (x + log (1 - exp (y - x)))
        | otherwise = errorOutOfRange "(-)"

    signum (LogFloat x)
        | x == negativeInfinity = 0
        | x >  negativeInfinity = 1
        | otherwise             = errorOutOfRange "signum"
        -- The extra guard protects against NaN, in case someone
        -- broke the invariant. That shouldn't be possible and
        -- so noone else bothers to check, but we check here just
        -- in case.

    negate _    = errorOutOfRange "negate"

    abs         = id

    fromInteger = LogFloat . log
                . guardNonNegative "fromInteger" . fromInteger


instance Fractional LogFloat where
    -- n/0 is handled seamlessly for us; we must catch 0/0 though
    (/) (LogFloat x) (LogFloat y)
        |    x == negativeInfinity
          && y == negativeInfinity = errorOutOfRange "(/)" -- protect vs NaN
        | otherwise                = LogFloat (x-y)
    
    fromRational = LogFloat . log
                 . guardNonNegative "fromRational" . fromRational


-- Just for fun. The more coersion functions the better. Though
-- Rationals are very buggy when it comes to transfinite values
instance Real LogFloat where
    toRational (LogFloat x) = toRational (exp x)

----------------------------------------------------------------
----------------------------------------------------------- fin.
