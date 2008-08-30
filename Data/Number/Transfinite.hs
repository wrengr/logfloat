
-- Needed to ensure correctness, because we can't guarantee that rules fire
{-# LANGUAGE MultiParamTypeClasses
           , OverlappingInstances
           #-}

-- Glasgow extensions needed to enable the # kind
{-# OPTIONS_GHC -cpp -fglasgow-exts #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

----------------------------------------------------------------
--                                                  ~ 2008.08.29
-- |
-- Module      :  Data.Number.Transfinite
-- Copyright   :  Copyright (c) 2007--2008 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  beta
-- Portability :  non-portable (CPP, MPTC, OverlappingInstances)
-- 
-- This module presents a type class for numbers which have
-- representations for transfinite values. The idea originated from
-- the IEEE-754 floating-point special values, used by
-- "Data.Number.LogFloat". However not all 'Fractional' types
-- necessarily support transfinite values. In particular, @Ratio@
-- types including 'Rational' do not have portable representations.
-- 
-- For the Glasgow compiler (GHC 6.8.2), "GHC.Real" defines @1%0@
-- and @0%0@ as representations for 'infinity' and 'notANumber',
-- but most operations on them will raise exceptions. If 'toRational'
-- is used on an infinite floating value, the result is a rational
-- with a numerator sufficiently large that it will overflow when
-- converted back to a @Double@. If used on NaN, the result would
-- buggily convert back as 'negativeInfinity'.
-- 
-- Hugs (September 2006) stays closer to the haskell98 spec and
-- offers no way of constructing those values, raising arithmetic
-- overflow errors if attempted.
----------------------------------------------------------------
module Data.Number.Transfinite
    ( Transfinite(..)
    , log
    , RealToFrac(..)
    ) where

import Prelude hiding    (isInfinite, isNaN, log, realToFrac)
import qualified Prelude (isInfinite, isNaN, log, realToFrac)

#ifdef __GLASGOW_HASKELL__
import GHC.Prim
    ( int2Double#
    , int2Float#
    , double2Float#
    , float2Double#
    )
import GHC.Exts (Int(..), Integer(..), Float(..), Double(..))
#endif

----------------------------------------------------------------
-- | Many numbers are not 'Bounded' yet, even though they can
-- represent arbitrarily large values, they are not necessarily
-- able to represent transfinite values such as infinity itself.
-- This class is for types which are capable of representing such
-- values. Notably, this class does not require the type to be
-- 'Fractional' nor 'Floating' since integral types could also have
-- representations for transfinite values. By popular demand the
-- 'Num' restriction has been lifted as well, due to complications
-- of defining 'Show' or 'Eq' for some types.
--
-- In particular, this class extends the ordered projection to have
-- a maximum value 'infinity' and a minimum value 'negativeInfinity',
-- as well as an exceptional value 'notANumber'. All the natural
-- laws regarding @infinity@ and @negativeInfinity@ should pertain.
-- (Some of these are discussed below.)

class (Ord a) => Transfinite a where
    
    -- | A transfinite value which is greater than all finite values.
    -- Adding or subtracting any finite value is a no-op. As is
    -- multiplying by any non-zero positive value (including
    -- @infinity@), and dividing by any positive finite value. Also
    -- obeys the law @negate infinity = negativeInfinity@ with all
    -- appropriate ramifications.
    
    infinity :: a
    
    
    -- | A transfinite value which is less than all finite values.
    -- Obeys all the same laws as @infinity@ with the appropriate
    -- changes for the sign difference.
    
    negativeInfinity :: a
    
    
    -- | An exceptional transfinite value for dealing with undefined
    -- results when manipulating infinite values. The following
    -- operations must return @notANumber@, where @inf@ is any value
    -- which @isInfinite@:
    --
    -- * @inf + inf@
    -- * @inf - inf@
    -- * @inf * 0@
    -- * @0 * inf@
    -- * @inf \/ inf@
    -- * @inf `div` inf@
    -- * @0 \/ 0@
    -- * @0 `div` 0@
    --
    -- Additionally, any mathematical operations on @notANumber@
    -- must also return @notANumber@, and any equality or ordering
    -- comparison on @notANumber@ must return @False@. Since it
    -- returns false for equality, there may be more than one machine
    -- representation of this `value'.
    
    notANumber :: a
    
    
    -- | Return true for both @infinity@ and @negativeInfinity@,
    -- false for all other values.
    isInfinite :: a -> Bool
    
    -- | Return true only for @notANumber@.
    isNaN      :: a -> Bool


instance Transfinite Double where
    infinity         = 1/0
    negativeInfinity = negate (1/0)
    notANumber       = 0/0
    isInfinite       = Prelude.isInfinite
    isNaN            = Prelude.isNaN


instance Transfinite Float where
    infinity         = 1/0
    negativeInfinity = negate (1/0)
    notANumber       = 0/0
    isInfinite       = Prelude.isInfinite
    isNaN            = Prelude.isNaN


----------------------------------------------------------------
-- | Since the normal 'Prelude.log' throws an error on zero, we
-- have to redefine it in order for things to work right. Arguing
-- from limits we can see that @log 0 == negativeInfinity@. Newer
-- versions of GHC have this behavior already, but older versions
-- and Hugs do not.
--
-- This function will raise an error when taking the log of negative
-- numbers, rather than returning 'notANumber' as the newer GHC
-- implementation does. The reason being that typically this is a
-- logical error, and @notANumber@ allows the error to propegate
-- silently.
--
-- In order to improve portability, the 'Transfinite' class is
-- required to indicate that the 'Floating' type does in fact have
-- a representation for negative infinity. Both native floating
-- types ('Double' and 'Float') are supported. If you define your
-- own instance of @Transfinite@, verify the above equation holds
-- for your @0@ and @negativeInfinity@. If it doesn't, then you
-- should avoid importing our @log@ and will probably want converters
-- to handle the discrepancy.

{-# SPECIALIZE log :: Double -> Double #-}
{-# SPECIALIZE log :: Float  -> Float  #-}
log  :: (Floating a, Transfinite a) => a -> a
log x = case compare x 0 of
        GT -> Prelude.log x
        EQ -> negativeInfinity
        LT -> error "Data.Number.Transfinite.log: argument out of range"


----------------------------------------------------------------
-- | The 'Prelude.realToFrac' function is defined to pivot through
-- a 'Rational' according to the haskell98 spec. This is non-portable
-- and problematic as discussed above. Since there is some resistance
-- to breaking from the spec, this class defines a reasonable variant
-- which deals with transfinite values appropriately.
--
-- N.B. The generic instance for transfinite types uses expensive
-- checks to ensure correctness. On GHC there are specialized
-- versions which use primitive converters instead. These instances
-- are hidden from other compilers by the CPP. Be warned that the
-- instances are overlapped, so you'll need to give type signatures
-- if the arguments to 'realToFrac' are polymorphic.
--
-- If any of these restrictions (CPP, GHC-only, OverlappingInstances)
-- are onerous to you, contact the maintainer (we like patches :)
--
-- * <http://www.haskell.org/pipermail/haskell-prime/2006-February/000791.html>
-- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/rewrite-rules.html>

class RealToFrac a b where
    realToFrac :: (Real a, Fractional b) => a -> b

instance RealToFrac a a where
    realToFrac = id

instance (Transfinite a, Transfinite b) => RealToFrac a b where
    realToFrac x
        | isNaN      x = notANumber
        | isInfinite x = if x > 0 then infinity
                                  else negativeInfinity
        | otherwise    = Prelude.realToFrac x


#ifdef __GLASGOW_HASKELL__
instance RealToFrac Int Integer where
    {-# INLINE realToFrac #-}
    realToFrac (I# i) = S# i

instance RealToFrac Int Float where
    {-# INLINE realToFrac #-}
    realToFrac (I# i) = F# (int2Float# i)

instance RealToFrac Int Double where
    {-# INLINE realToFrac #-}
    realToFrac (I# i) = D# (int2Double# i)


instance RealToFrac Integer Float where
    -- TODO: is there a more primitive way?
    realToFrac j = Prelude.realToFrac j

instance RealToFrac Integer Double where
    -- TODO: is there a more primitive way?
    realToFrac j = Prelude.realToFrac j


instance RealToFrac Float Double where
    {-# INLINE realToFrac #-}
    realToFrac (F# f) = D# (float2Double# f)
    
instance RealToFrac Double Float where
    {-# INLINE realToFrac #-}
    realToFrac (D# d) = F# (double2Float# d)
#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
