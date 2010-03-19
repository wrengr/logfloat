{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

-- Unfortunately we need -fglasgow-exts in order to actually pick
-- up on the rules (see -ddump-rules). The -frewrite-rules flag
-- doesn't do what you want.
-- <http://hackage.haskell.org/trac/ghc/ticket/2213>
-- <http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg14313.html>
{-# OPTIONS_GHC -fglasgow-exts #-}

----------------------------------------------------------------
--                                                  ~ 2009.03.09
-- |
-- Module      :  Data.Number.Transfinite
-- Copyright   :  Copyright (c) 2007--2010 wren ng thornton
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
-- buggily convert back as 'negativeInfinity'. For more discussion
-- on why this approach is problematic, see:
--
-- * <http://www.haskell.org/pipermail/haskell-prime/2006-February/000791.html>
--
-- * <http://www.haskell.org/pipermail/haskell-prime/2006-February/000821.html>
-- 
-- Hugs (September 2006) stays closer to the haskell98 spec and
-- offers no way of constructing those values, raising arithmetic
-- overflow errors if attempted.
----------------------------------------------------------------
module Data.Number.Transfinite
    ( Transfinite(..)
    , log
    ) where

import Prelude hiding    (log, isInfinite, isNaN)
import qualified Prelude (log)
import qualified Hugs.RealFloat as Prelude (isInfinite, isNaN)

import Data.Number.PartialOrd

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
--
-- Hugs (September 2006) has buggy Prelude definitions for
-- 'Prelude.isNaN' and 'Prelude.isInfinite' on Float and Double.
-- This module provides correct definitions, so long as "Hugs.RealFloat"
-- is compiled correctly.

class (PartialOrd a) => Transfinite a where
    
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
    -- * @infinity + negativeInfinity@
    --
    -- * @negativeInfinity + infinity@
    --
    -- * @infinity - infinity@
    --
    -- * @negativeInfinity - negativeInfinity@
    --
    -- * @inf * 0@
    --
    -- * @0 * inf@
    --
    -- * @inf \/ inf@
    --
    -- * @inf `div` inf@
    --
    -- * @0 \/ 0@
    --
    -- * @0 `div` 0@
    --
    -- Additionally, any mathematical operations on @notANumber@
    -- must also return @notANumber@, and any equality or ordering
    -- comparison on @notANumber@ must return @False@ (violating
    -- the law of the excluded middle, often assumed but not required
    -- for 'Eq'; thus, 'eq' and 'ne' are preferred over ('==') and
    -- ('/=')). Since it returns false for equality, there may be
    -- more than one machine representation of this `value'.
    
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
--
-- For GHC, this version of @log@ has rules for fusion with @exp@.
-- These can give different behavior by preventing overflow to
-- @infinity@ and preventing errors for taking the logarithm of
-- negative values. For 'Double' and 'Float' they can also give
-- different answers due to eliminating floating point fuzz. The
-- rules strictly improve mathematical accuracy, however they should
-- be noted in case your code depends on the implementation details.

log  :: (Floating a, Transfinite a) => a -> a
{-# SPECIALIZE log :: Double -> Double #-}
{-# SPECIALIZE log :: Float  -> Float  #-}
log x = case x `cmp` 0 of
        Just GT -> Prelude.log x
        Just EQ -> negativeInfinity
        Just LT -> err "argument out of range"
        Nothing -> err "argument not comparable to 0"
        where
        err e = error $! "Data.Number.Transfinite.log: "++e

-- Note, Floating ultimately requires Num, but not Ord. If PartialOrd
-- proves to be an onerous requirement on Transfinite, we could
-- hack our way around without using PartialOrd by using isNaN, (==
-- 0), ((>0).signum) but that would be less efficient.

----------------------------------------------------------------
-- These rules moved here from "LogFloat" in v0.11.2
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
----------------------------------------------------------------
----------------------------------------------------------- fin.
