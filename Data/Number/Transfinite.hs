
{-# OPTIONS_GHC -Wall -Werror #-}

----------------------------------------------------------------
--                                                  ~ 2008.08.16
-- |
-- Module      :  Data.Number.Transfinite
-- Copyright   :  Copyright (c) 2007--2008 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable
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
-- convert back as 'negativeInfinity'.
-- 
-- Hugs (September 2006) stays closer to the haskell98 spec and
-- offers no way of constructing those values, raising arithmetic
-- overflow errors if attempted.
----------------------------------------------------------------
module Data.Number.Transfinite (Transfinite(..)) where

import Prelude hiding    (isInfinite, isNaN)
import qualified Prelude (isInfinite, isNaN)

----------------------------------------------------------------
-- | Many numbers are not 'Bounded' yet, even though they can
-- represent arbitrarily large values, they are not necessarily
-- able to represent transfinite values such as infinity itself.
-- This class is for types which are able to represent such values.
-- Notably, this class does not require the type to be 'Fractional'
-- nor 'Floating' since integral types could also have representations
-- for transfinite values.
--
-- In particular, this class extends the 'Ord' projection to have
-- a maximum value 'infinity' and a minimum value 'negativeInfinity',
-- as well as an exceptional value 'notANumber'. All the natural
-- laws regarding @infinity@ and @negativeInfinity@ should pertain
-- (e.g. @negate infinity = negativeInfinity@, @infinity + x =
-- infinity@ when @x@ is finite, etc.). Additionally, @infinity -
-- infinity@ should return @notANumber@ (as should @0\/0@ and
-- @infinity\/infinity@ if the type is @Fractional@). Any operations
-- on @notANumber@ should also return @notANumber@, and any equality
-- or ordering comparison on @notANumber@ must return @False@.
--
-- Minimum complete definition is @infinity@, @isInfinite@, and
-- @isNaN@.

class (Num a, Ord a) => Transfinite a where
    
    -- | A transfinite value which is greater than all finite values.
    -- Adding or subtracting any finite value is a no-op. As is
    -- multiplying by any non-zero positive value (including
    -- @infinity@), and dividing by any non-zero positive finite
    -- value.
    infinity         :: a
    
    -- | A transfinite value which is less than all finite values.
    -- Obeys all the same laws as @infinity@ with the appropriate
    -- changes for the sign difference.
    negativeInfinity :: a
    negativeInfinity  = negate infinity
    
    -- | An exceptional transfinite value for dealing with undefined
    -- results when manipulating infinite values. Since NaN shall
    -- return false for all ordering and equality operations, there
    -- may be more than one machine representation of this `value'.
    notANumber       :: a
    notANumber        = infinity - infinity
    
    -- | Return true for both @infinity@ and @negativeInfinity@,
    -- false for all other values.
    isInfinite :: a -> Bool
    
    -- | Return true only for @notANumber@.
    isNaN      :: a -> Bool


instance Transfinite Double where
    infinity   = 1 / 0
    isInfinite = Prelude.isInfinite
    isNaN      = Prelude.isNaN


instance Transfinite Float where
    infinity   = 1 / 0
    isInfinite = Prelude.isInfinite
    isNaN      = Prelude.isNaN

----------------------------------------------------------------
----------------------------------------------------------- fin.
