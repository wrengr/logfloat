-- FlexibleContexts needed by our RealToFrac contexts
-- CPP needed for IArray UArray instance
-- FFI is for log1p
--     N.B. can't mix FFI and -fvia-C under ghc==6.10.1
--     <http://hackage.haskell.org/trac/ghc/ticket/3117>
--     TODO: figure out how to conditionally use "-fvia-C -optc-O3"
--     for ghc6.8 when not using Cabal to compile.
{-# LANGUAGE FlexibleContexts
           , CPP
           , ForeignFunctionInterface
           #-}

-- Removed -Wall because -fno-warn-orphans was removed in GHC 6.10
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

-- Unfortunately we need -fglasgow-exts in order to actually pick
-- up on the rules (see -ddump-rules). The -frewrite-rules flag
-- doesn't do what you want.
-- cf <http://hackage.haskell.org/trac/ghc/ticket/2213>
-- cf <http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg14313.html>
{-# OPTIONS_GHC -O2 -fexcess-precision -fglasgow-exts #-}

----------------------------------------------------------------
--                                                  ~ 2010.03.19
-- |
-- Module      :  Data.Number.LogFloat
-- Copyright   :  Copyright (c) 2007--2010 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable (with CPP, FFI)
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
-- numbers for efficiency's sake, see "Data.Number.LogFloat.Signed"
-- for doing signed log-domain calculations.
----------------------------------------------------------------

module Data.Number.LogFloat
    (
    -- * Exceptional numeric values
      module Data.Number.Transfinite
    , module Data.Number.RealToFrac
    
    -- * @LogFloat@ data type
    , LogFloat
    -- ** Isomorphism to normal-domain
    , logFloat
    , fromLogFloat
    -- ** Isomorphism to log-domain
    , logToLogFloat
    , logFromLogFloat
    
    -- * Accurate versions of logarithm\/exponentiation
    , log1p, expm1
    ) where

import Prelude hiding (log, realToFrac, isInfinite, isNaN)

import Data.Number.RealToFrac
import Data.Number.Transfinite
import Data.Number.PartialOrd


-- GHC can derive (IArray UArray LogFloat), but Hugs needs to coerce
-- TODO: see about nhc98/yhc, jhc/lhc
import Data.Array.Base    (IArray(..))
import Data.Array.Unboxed (UArray)

-- Hugs (Sept 2006) doesn't use the generic wrapper in base:Unsafe.Coerce
-- so we'll just have to go back to the original source.
#ifdef __HUGS__
import Hugs.IOExts (unsafeCoerce)
#elif __NHC__
import NonStdUnsafeCoerce (unsafeCoerce)
#endif

#ifdef __GLASGOW_HASKELL__
import Foreign.Storable (Storable)
#endif

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
    deriving
    ( Eq
    , Ord -- Should we really perpetuate the Ord lie?
#ifdef __GLASGOW_HASKELL__
    -- At least GHC 6.8.2 can derive these (without
    -- GeneralizedNewtypeDeriving). The H98 Report doesn't include
    -- them among the options for automatic derivation though.
    , IArray UArray
    , Storable
#endif
    )


#if __HUGS__ || __NHC__
-- TODO: Storable instance. Though Foreign.Storable isn't in Hugs(Sept06)

-- These two operators make it much easier to read the instance.
-- Hopefully inlining everything will get rid of the eta overhead.
-- <http://matt.immute.net/content/pointless-fun>
{-# INLINE (~>) #-}
infixr 2 ~>
f ~> g = (. f) . (g .)

{-# INLINE ($.) #-}
infixl 1 $.
($.) = flip ($)


{-# INLINE logFromLFAssocs #-}
logFromLFAssocs :: [(Int, LogFloat)] -> [(Int, Double)]
logFromLFAssocs = unsafeCoerce

{-# INLINE logFromLFUArray #-}
logFromLFUArray :: UArray a LogFloat -> UArray a Double
logFromLFUArray = unsafeCoerce

-- Named unsafe because it could allow injecting NaN if misused
{-# INLINE unsafeLogToLFUArray #-}
unsafeLogToLFUArray :: UArray a Double -> UArray a LogFloat
unsafeLogToLFUArray = unsafeCoerce

-- Named unsafe because it could allow injecting NaN if misused
{-# INLINE unsafeLogToLFFunc #-}
unsafeLogToLFFunc :: (LogFloat -> a -> LogFloat) -> (Double -> a -> Double)
unsafeLogToLFFunc = ($. unsafeLogToLogFloat ~> id ~> logFromLogFloat)

-- | Remove the extranious 'isNaN' test of 'logToLogFloat', when
-- we know it's safe.
{-# INLINE unsafeLogToLogFloat #-}
unsafeLogToLogFloat :: Double -> LogFloat
unsafeLogToLogFloat = LogFloat


instance IArray UArray LogFloat where
    {-# INLINE bounds #-}
    bounds = bounds . logFromLFUArray
    
-- Apparently this method was added in base-2.0/GHC-6.6 but Hugs
-- (Sept 2006) doesn't have it. Not sure about NHC's base
#if __HUGS__ > 200609
    {-# INLINE numElements #-}
    numElements = numElements . logFromLFUArray
#endif
    
    {-# INLINE unsafeArray #-}
    unsafeArray =
        unsafeArray $. id ~> logFromLFAssocs ~> unsafeLogToLFUArray
    
    {-# INLINE unsafeAt #-}
    unsafeAt =
        unsafeAt $. logFromLFUArray ~> id ~> unsafeLogToLogFloat
    
    {-# INLINE unsafeReplace #-}
    unsafeReplace =
        unsafeReplace $. logFromLFUArray ~> logFromLFAssocs ~> unsafeLogToLFUArray
    
    {-# INLINE unsafeAccum #-}
    unsafeAccum =
        unsafeAccum $. unsafeLogToLFFunc ~> logFromLFUArray ~> id ~> unsafeLogToLFUArray
    
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray =
        unsafeAccumArray $. unsafeLogToLFFunc ~> logFromLogFloat ~> id ~> id ~> unsafeLogToLFUArray
#endif

-- TODO: the Nothing branch should never be reachable. Once we get
-- a test suite up and going to *verify* the never-NaN invariant,
-- we should be able to eliminate the branch and the isNaN checks.
instance PartialOrd LogFloat where
    cmp (LogFloat x) (LogFloat y) 
        | isNaN x || isNaN y = Nothing
        | otherwise          = Just $! x `compare` y


----------------------------------------------------------------
-- | Reduce the number of constant string literals we need to store.
errorOutOfRange :: String -> a
{-# NOINLINE errorOutOfRange #-}
errorOutOfRange fun = error $! "Data.Number.LogFloat."++fun
                            ++ ": argument out of range"

-- Both guards are redundant due to the subsequent call to
-- 'Data.Number.Transfinite.log' at all use sites. However we use
-- this function to give local error messages. Perhaps we should
-- catch the exception and throw the new message instead? Portability?

guardNonNegative      :: String -> Double -> Double
guardNonNegative fun x | isNaN x || x < 0 = errorOutOfRange fun
                       | otherwise        = x

guardIsANumber        :: String -> Double -> Double
guardIsANumber   fun x | isNaN x   = errorOutOfRange fun
                       | otherwise = x

----------------------------------------------------------------
-- | Constructor which does semantic conversion from normal-domain
-- to log-domain. Throws errors on negative input.
logFloat :: (Real a, RealToFrac a Double) => a -> LogFloat
{-# SPECIALIZE logFloat :: Double -> LogFloat #-}
logFloat  = LogFloat . log . guardNonNegative "logFloat" . realToFrac


-- This is simply a polymorphic version of the 'LogFloat' data
-- constructor. We present it mainly because we hide the constructor
-- in order to make the type a bit more opaque. If the polymorphism
-- turns out to be a performance liability because the rewrite rules
-- can't remove it, then we need to rethink all four
-- constructors\/destructors.
--
-- | Constructor which assumes the argument is already in the
-- log-domain. Throws errors on @notANumber@ input.
logToLogFloat :: (Real a, RealToFrac a Double) => a -> LogFloat
{-# SPECIALIZE logToLogFloat :: Double -> LogFloat #-}
logToLogFloat  = LogFloat . guardIsANumber "logToLogFloat" . realToFrac


-- | Return our log-domain value back into normal-domain. Beware
-- of overflow\/underflow.
fromLogFloat :: (Fractional a, Transfinite a, RealToFrac Double a)
             => LogFloat -> a
{-# SPECIALIZE fromLogFloat :: LogFloat -> Double #-}
fromLogFloat (LogFloat x) = realToFrac (exp x)


-- | Return the log-domain value itself without conversion.
logFromLogFloat :: (Fractional a, Transfinite a, RealToFrac Double a)
                => LogFloat -> a
{-# SPECIALIZE logFromLogFloat :: LogFloat -> Double #-}
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
-- Technically these should use 'Foreign.C.CDouble' however there's
-- no isomorphism provided to normal 'Double'. The former is
-- documented as being a newtype of the later, and so this should
-- be safe.

#ifdef __USE_FFI__
#define LOG1P_WHICH_VERSION specialized version.
#else
#define LOG1P_WHICH_VERSION naive version! \
    Contact the maintainer with any FFI difficulties.
#endif


-- | Definition: @log1p == log . (1+)@. The C language provides a
-- special definition for 'log1p' which is more accurate than doing
-- the naive thing, especially for very small arguments. For example,
-- the naive version underflows around @2 ** -53@, whereas the
-- specialized version underflows around @2 ** -1074@. This function
-- is used by ('+') and ('-') on @LogFloat@.
--
-- /This installation was compiled to use the LOG1P_WHICH_VERSION/

#ifdef __USE_FFI__
foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double
#else
log1p :: Double -> Double
{-# INLINE log1p #-}
log1p x = log (1 + x)
#endif


-- | Definition: @expm1 == (subtract 1) . exp@. The C language
-- provides a special definition for 'expm1' which is more accurate
-- than doing the naive thing, especially for very small arguments.
-- This function isn't needed internally, but is provided for
-- symmetry with 'log1p'.
--
-- /This installation was compiled to use the LOG1P_WHICH_VERSION/

#ifdef __USE_FFI__
foreign import ccall unsafe "math.h expm1"
    expm1 :: Double -> Double
#else
expm1 :: Double -> Double
{-# INLINE expm1 #-}
expm1 x = exp x - 1
#endif

----------------------------------------------------------------
-- These all work without causing underflow. However, do note that
-- they tend to induce more of the floating-point fuzz than using
-- regular floating numbers because @exp . log@ doesn't really equal
-- @id@. In any case, our main aim is for preventing underflow when
-- multiplying many small numbers (and preventing overflow for
-- multiplying many large numbers) so we're not too worried about
-- +\/- 4e-16.

instance Num LogFloat where 
    -- N.B. In Hugs (Sept2006) the (>=) always returns True if
    --      either isNaN. This does not constitute a bug since we
    --      maintain the invariant that values wrapped by 'LogFloat'
    --      are not NaN.
    
    (*) (LogFloat x) (LogFloat y)
        |    isInfinite x
          && isInfinite y
          && x == negate y = LogFloat negativeInfinity -- @0*infinity == 0@
        | otherwise        = LogFloat (x+y)
    
    (+) (LogFloat x) (LogFloat y)
        | x == y
          && isInfinite x
          && isInfinite y = LogFloat x -- @0+0 == 0@, @infinity+infinity == infinity@
        | x >= y          = LogFloat (x + log1p (exp (y - x)))
        | otherwise       = LogFloat (y + log1p (exp (x - y)))
    
    (-) (LogFloat x) (LogFloat y)
        |    x == negativeInfinity
          && y == negativeInfinity = LogFloat negativeInfinity -- @0-0 == 0@
        | otherwise =
            -- Will throw error if x < y or if (x,y) is (infinity,infinity)
            LogFloat (guardIsANumber "(-)" (x + log1p (negate (exp (y - x)))))
    
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
    -- n/0 == infinity is handled seamlessly for us. We must catch 0/0 and infinity/infinity NaNs, and handle 0/infinity.
    (/) (LogFloat x) (LogFloat y)
        | x == y
          && isInfinite x
          && isInfinite y       = errorOutOfRange "(/)"
        | x == negativeInfinity = LogFloat negativeInfinity -- @0/infinity == 0@
        | otherwise             = LogFloat (x-y)
    
    fromRational = LogFloat . log
                 . guardNonNegative "fromRational" . fromRational


-- Just for fun. The more coersion functions the better. Though
-- Rationals are very buggy when it comes to transfinite values
instance Real LogFloat where
    toRational (LogFloat x)
        | isInfinite ex || isNaN ex = errorOutOfRange "toRational"
        | otherwise                 = toRational ex
        where
        ex = exp x


{- -- Commented out because I'm not sure about requiring MPTCs. Of course, those are already required by "Data.Number.Transfinite" so it's pretty moot...

-- LogFloat->LogFloat is already given via generic (a->a)
-- No LogFloat->Rational since LogFloat can have 'infinity'
-- Can't have LogFloat->a using fromLogFloat because Hugs dislikes incoherence. Adding an explicit LogFloat->LogFloat instance doesn't help like it does for GHC.

instance RealToFrac LogFloat Double where
    realToFrac = fromLogFloat
    
instance RealToFrac LogFloat Float where
    realToFrac = fromLogFloat
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
