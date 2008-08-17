
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
-- necessarily support transfinite values.
-- 
-- In particular, @Ratio@ types including 'Rational' do not have
-- portable representations. For the Glasgow compiler (GHC 6.8.2),
-- "GHC.Real" defines @1%0@ and @0%0@ as representations for
-- 'infinity' and 'notANumber', but most operations on them will
-- yield exceptions. Hugs (September 2006) stays closer to the
-- haskell98 spec and offers no way of constructing those values,
-- yielding exceptions if attempted.
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
-- since integral types could also have representations for transfinite
-- values.
--
-- In particular, this class extends the 'Ord' projection to have
-- a maximum value 'infinity' and a minimum value 'negativeInfinity',
-- as well as an exceptional value 'notANumber'. All the natural
-- laws regarding @infinity@ and @negativeInfinity@ should pertain
-- (e.g. @negate infinity = negativeInfinity@, @infinity + x =
-- infinity@ when @x@ is finite, etc.). Additionally, @infinity -
-- infinity@ (@0\/0@, and @infinity\/infinity@ if @Fractional@)
-- should return @notANumber@. Any operations on @notANumber@ should
-- also return @notANumber@, and any ordering comparison on
-- @notANumber@ should return @False@.
--
-- Minimum complete definition is @infinity@, @isInfinite@, and
-- @isNaN@.

class (Num a, Ord a) => Transfinite a where
    
    infinity         :: a
    
    negativeInfinity :: a
    negativeInfinity  = negate infinity
    
    notANumber       :: a
    notANumber        = infinity - infinity
    
    -- | This should return true for both @infinity@ and
    -- @negativeInfinity@
    isInfinite :: a -> Bool
    
    -- | This should return true only for @notANumber@
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
