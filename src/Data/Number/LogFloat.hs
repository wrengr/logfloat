-- CPP and GeneralizedNewtypeDeriving are needed for IArray UArray instance
-- FFI is for log1p
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- We don't put these in LANGUAGE, because it's CPP guarded for GHC only
{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

{-# OPTIONS_GHC -O2 -fexcess-precision -fenable-rewrite-rules #-}

----------------------------------------------------------------
--                                                  ~ 2015.02.17
-- |
-- Module      :  Data.Number.LogFloat
-- Copyright   :  Copyright (c) 2007--2015 wren gayle romano
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
    
    -- * @LogFloat@ data type
    , LogFloat
    -- ** Isomorphism to normal-domain
    , logFloat
    , fromLogFloat
    -- ** Isomorphism to log-domain
    , logToLogFloat
    , logFromLogFloat
    -- ** Additional operations
    , sum, product
    , pow
    
    -- * Accurate versions of logarithm\/exponentiation
    , log1p, expm1
    ) where

import Prelude hiding (log, sum, product, isInfinite, isNaN)
import Data.List (foldl')

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
-- instance will convert back to the normal-domain, and hence will
-- underflow at that point. This behavior may change in the future.
--
-- Because 'logFloat' performs the semantic conversion, we can use
-- operators which say what we *mean* rather than saying what we're
-- actually doing to the underlying representation. That is,
-- equivalences like the following are true[1] thanks to type-class
-- overloading:
--
-- > logFloat (p + q) == logFloat p + logFloat q
-- > logFloat (p * q) == logFloat p * logFloat q
--
-- (Do note, however, that subtraction can and negation will throw
-- errors: since @LogFloat@ can only represent the positive half of
-- 'Double'. 'Num' is the wrong abstraction to put at the bottom
-- of the numeric type-class hierarchy; but alas, we're stuck with
-- it.)
--
-- Performing operations in the log-domain is cheap, prevents
-- underflow, and is otherwise very nice for dealing with miniscule
-- probabilities. However, crossing into and out of the log-domain
-- is expensive and should be avoided as much as possible. In
-- particular, if you're doing a series of multiplications as in
-- @lp * logFloat q * logFloat r@ it's faster to do @lp * logFloat
-- (q * r)@ if you're reasonably sure the normal-domain multiplication
-- won't underflow; because that way you enter the log-domain only
-- once, instead of twice. Also note that, for precision, if you're
-- doing more than a few multiplications in the log-domain, you
-- should use 'product' rather than using '(*)' repeatedly.
--
-- Even more particularly, you should /avoid addition/ whenever
-- possible. Addition is provided because sometimes we need it, and
-- the proper implementation is not immediately apparent. However,
-- between two @LogFloat@s addition requires crossing the exp\/log
-- boundary twice; with a @LogFloat@ and a 'Double' it's three
-- times, since the regular number needs to enter the log-domain
-- first. This makes addition incredibly slow. Again, if you can
-- parenthesize to do normal-domain operations first, do it!
--
-- [1] That is, true up-to underflow and floating point fuzziness.
-- Which is, of course, the whole point of this module.

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

{-# INLINE ($::) #-}
infixl 1 $::
($::) = flip ($)


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
unsafeLogToLFFunc = ($:: unsafeLogToLogFloat ~> id ~> logFromLogFloat)

-- | Remove the extraneous 'isNaN' test of 'logToLogFloat', when
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
        unsafeArray $:: id ~> logFromLFAssocs ~> unsafeLogToLFUArray
    
    {-# INLINE unsafeAt #-}
    unsafeAt =
        unsafeAt $:: logFromLFUArray ~> id ~> unsafeLogToLogFloat
    
    {-# INLINE unsafeReplace #-}
    unsafeReplace =
        unsafeReplace $:: logFromLFUArray ~> logFromLFAssocs ~> unsafeLogToLFUArray
    
    {-# INLINE unsafeAccum #-}
    unsafeAccum =
        unsafeAccum $:: unsafeLogToLFFunc ~> logFromLFUArray ~> id ~> unsafeLogToLFUArray
    
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray =
        unsafeAccumArray $:: unsafeLogToLFFunc ~> logFromLogFloat ~> id ~> id ~> unsafeLogToLFUArray
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
errorOutOfRange fun =
    error $! "Data.Number.LogFloat."++fun++ ": argument out of range"

-- Both guards are redundant due to the subsequent call to
-- 'Data.Number.Transfinite.log' at all use sites. However we use
-- this function to give local error messages. Perhaps we should
-- catch the exception and throw the new message instead? Portability?

guardNonNegative :: String -> Double -> Double
guardNonNegative fun x
    | isNaN x || x < 0 = errorOutOfRange fun
    | otherwise        = x

guardIsANumber :: String -> Double -> Double
guardIsANumber fun x
    | isNaN x   = errorOutOfRange fun
    | otherwise = x

----------------------------------------------------------------
-- | Constructor which does semantic conversion from normal-domain
-- to log-domain. Throws errors on negative and NaN inputs. If @p@
-- is non-negative, then following equivalence holds:
--
-- > logFloat p == logToLogFloat (log p)
--
-- If @p@ is NaN or negative, then the two sides differ only in
-- which error is thrown.
logFloat :: Double -> LogFloat
{-# INLINE [0] logFloat #-}
-- TODO: should we use NOINLINE or [~0] to avoid the possibility of code bloat?
logFloat = LogFloat . log . guardNonNegative "logFloat"


-- | Constructor which assumes the argument is already in the
-- log-domain. Throws errors on @notANumber@ inputs.
logToLogFloat :: Double -> LogFloat
logToLogFloat = LogFloat . guardIsANumber "logToLogFloat"


-- | Semantically convert our log-domain value back into the
-- normal-domain. Beware of overflow\/underflow. The following
-- equivalence holds (without qualification):
--
-- > fromLogFloat == exp . logFromLogFloat
--
fromLogFloat :: LogFloat -> Double
{-# INLINE [0] fromLogFloat #-}
-- TODO: should we use NOINLINE or [~0] to avoid the possibility of code bloat?
fromLogFloat (LogFloat x) = exp x


-- | Return the log-domain value itself without conversion.
logFromLogFloat :: LogFloat -> Double
logFromLogFloat (LogFloat x) = x


-- These are our module-specific versions of "log\/exp" and "exp\/log";
-- They do the same things but also have a @LogFloat@ in between
-- the logarithm and exponentiation. In order to ensure these rules
-- fire, we have to delay the inlining on two of the four
-- con-\/destructors.

{-# RULES
-- Out of log-domain and back in
"log/fromLogFloat"       forall x. log (fromLogFloat x) = logFromLogFloat x
"logFloat/fromLogFloat"  forall x. logFloat (fromLogFloat x) = x

-- Into log-domain and back out
"fromLogFloat/logFloat"  forall x. fromLogFloat (logFloat x) = x
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
    show (LogFloat x) =
        let y = exp x
        in  y `seq` "LogFloat "++show y


----------------------------------------------------------------
-- Technically these should use 'Foreign.C.CDouble' however there's
-- no isomorphism provided to normal 'Double'. The former is
-- documented as being a newtype of the later, and so this should
-- be safe.

#ifdef __USE_FFI__
#define LOG1P_WHICH_VERSION FFI version.
#else
#define LOG1P_WHICH_VERSION naive version! \
    Contact the maintainer with any FFI difficulties.
#endif


-- | Definition: @log1p == log . (1+)@. Standard C libraries provide
-- a special definition for 'log1p' which is more accurate than
-- doing the naive thing, especially for very small arguments. For
-- example, the naive version underflows around @2 ** -53@, whereas
-- the specialized version underflows around @2 ** -1074@. This
-- function is used by ('+') and ('-') on @LogFloat@.
--
-- N.B. The @statistics:Statistics.Math@ module provides a pure
-- Haskell implementation of @log1p@ for those who are interested.
-- We do not copy it here because it relies on the @vector@ package
-- which is non-portable. If there is sufficient interest, a portable
-- variant of that implementation could be made. Contact the
-- maintainer if the FFI and naive implementations are insufficient
-- for your needs.
--
-- /This installation was compiled to use the LOG1P_WHICH_VERSION/

#ifdef __USE_FFI__
foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double
#else
-- See statistics:Statistics.Math for a more accurate Haskell
-- implementation.
log1p :: Double -> Double
{-# INLINE [0] log1p #-}
log1p x = log (1 + x)
#endif


-- | Definition: @expm1 == subtract 1 . exp@. Standard C libraries
-- provide a special definition for 'expm1' which is more accurate
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
{-# INLINE [0] expm1 #-}
expm1 x = exp x - 1
#endif


{-# RULES
-- Into log-domain and back out
"expm1/log1p"    forall x. expm1 (log1p x) = x

-- Out of log-domain and back in
"log1p/expm1"    forall x. log1p (expm1 x) = x
    #-}

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
            -- BUG: Will throw error if x < y
            -- TODO: flip @x@ and @y@ when @y > x@.
            -- Also, will throw error if (x,y) is (infinity,infinity)
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
    -- n/0 == infinity is handled seamlessly for us. We must catch
    -- 0/0 and infinity/infinity NaNs, and handle 0/infinity.
    (/) (LogFloat x) (LogFloat y)
        | x == y
          && isInfinite x
          && isInfinite y       = errorOutOfRange "(/)"
        | x == negativeInfinity = LogFloat negativeInfinity -- @0/infinity == 0@
        | otherwise             = LogFloat (x-y)
    
    fromRational = LogFloat . log
                 . guardNonNegative "fromRational" . fromRational


-- Just for fun. The more coercion functions the better. Though
-- Rationals are very buggy when it comes to transfinite values
instance Real LogFloat where
    toRational (LogFloat x)
        | isInfinite ex || isNaN ex = errorOutOfRange "toRational"
        | otherwise                 = toRational ex
        where
        ex = exp x


----------------------------------------------------------------
-- | /O(1)/. Compute powers in the log-domain; that is, the following
-- equivalence holds (modulo underflow and all that):
--
-- > logFloat (p ** m) == logFloat p `pow` m
--
-- /Since: 0.13/
pow :: LogFloat -> Double -> LogFloat
{-# INLINE pow #-}
infixr 8 `pow`
pow (LogFloat x) m
    | isNaN mx  = LogFloat 0
    | otherwise = LogFloat mx
    where
    -- N.B., will be NaN when @x == negativeInfinity && m == 0@
    -- (which is true when m is -0 as well as +0). We check for NaN
    -- after multiplying, rather than checking this precondition
    -- before multiplying, in an attempt to simplify/optimize the
    -- generated code.
    -- TODO: benchmark.
    mx = m * x


-- N.B., the default implementation of (**) for Complex is wrong.
-- It can be fixed by using the definition:
-- > x ** y = if x == 0 then 0 else exp (log x * y)
-- cf., <https://ghc.haskell.org/trac/ghc/ticket/8539>
-- TODO: Is this relevant to us at all?


-- TODO: check out ekmett's compensated library.


-- Some good test cases:
-- for @logsumexp == log . sum . map exp@:
--     logsumexp[0,1,0] should be about 1.55
-- for correctness of avoiding underflow:
--     logsumexp[1000,1001,1000]   ~~ 1001.55 ==  1000 + 1.55
--     logsumexp[-1000,-999,-1000] ~~ -998.45 == -1000 + 1.55
--
-- | /O(n)/. Compute the sum of a finite list of 'LogFloat's, being
-- careful to avoid underflow issues. That is, the following
-- equivalence holds (modulo underflow and all that):
--
-- > logFloat . sum == sum . map logFloat
--
-- /N.B./, this function requires two passes over the input. Thus,
-- it is not amenable to list fusion, and hence will use a lot of
-- memory when summing long lists.
--
-- /Since: 0.13/
sum :: [LogFloat] -> LogFloat
sum xs = LogFloat (theMax + theSum)
    where
    theMax = logFromLogFloat (maximum xs)
    
    -- compute @\log \sum_{x \in xs} \exp(x - theMax)@
    theSum = foldl' (\ acc x -> acc + exp (logFromLogFloat x - theMax)) 0 xs

-- TODO: expose a single-pass version for the special case where
-- the first element of the list is (promised to be) the maximum
-- element?



-- | /O(n)/. Compute the product of a finite list of 'LogFloat's,
-- being careful to avoid numerical error due to loss of precision.
-- That is, the following equivalence holds (modulo underflow and
-- all that):
--
-- > logFloat . product == product . map logFloat
--
-- /Since: 0.13/
product :: [LogFloat] -> LogFloat
product = kahan 0 0
    where
    kahan t c _ | t `seq` c `seq` False = undefined
    kahan t _ []                = LogFloat t
    kahan t c (LogFloat x : xs) =
        -- Beware this getting incorrectly optimized away by constant folding!
        let y  = x - c
            t' = t + y
            c' = (t' - t) - y
        in kahan t' c' xs

-- This version *completely* eliminates rounding errors and loss
-- of significance due to catastrophic cancellation during summation.
-- <http://code.activestate.com/recipes/393090/> Also see the other
-- implementations given there. For Python's actual C implementation,
-- see math_fsum in
-- <http://svn.python.org/view/python/trunk/Modules/mathmodule.c?view=markup>
--
-- For merely *mitigating* errors rather than completely eliminating
-- them, see <http://code.activestate.com/recipes/298339/>.
--
-- A good test case is @msum([1, 1e100, 1, -1e100] * 10000) == 20000.0@
{-
-- For proof of correctness, see
-- <www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps>
def msum(xs):
    partials = [] # sorted, non-overlapping partial sums
    # N.B., the actual C implementation uses a 32 array, doubling size as needed
    for x in xs:
        i = 0
        for y in partials: # for(i = j = 0; j < n; j++)
            if abs(x) < abs(y):
                x, y = y, x
            hi = x + y
            lo = y - (hi - x)
            if lo != 0.0:
                partials[i] = lo
                i += 1
            x = hi
        # does an append of x while dropping all the partials after
        # i. The C version does n=i; and leaves the garbage in place
        partials[i:] = [x]
    # BUG: this last step isn't entirely correct and can lose
    # precision <http://stackoverflow.com/a/2704565/358069>
    return sum(partials, 0.0)
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
