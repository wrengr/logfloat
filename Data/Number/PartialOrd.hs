{-# LANGUAGE OverlappingInstances
           , FlexibleInstances
           , UndecidableInstances
           #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

----------------------------------------------------------------
--                                                  ~ 2008.08.29
-- |
-- Module      :  Data.Number.PartialOrd
-- Copyright   :  Copyright (c) 2007--2008 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
-- 
-- The Prelude's 'Ord' class for dealing with ordered types is often
-- onerous to use because it requires 'Eq' as well as a total
-- ordering. While such total orderings are common, partial orderings
-- are moreso. This module presents a class for partially ordered
-- types.
----------------------------------------------------------------
module Data.Number.PartialOrd (PartialOrd(..)) where

----------------------------------------------------------------
-- | This class defines a partially ordered type. The method names
-- were chosen so as not to conflict with 'Ord' and 'Eq'. We use
-- 'Maybe' instead of defining new types @PartialOrdering@ and
-- @FuzzyBool@ because this way should make the class easier to
-- use.

class PartialOrd a where
    cmp :: a -> a -> Maybe Ordering
    gt  :: a -> a -> Maybe Bool
    ge  :: a -> a -> Maybe Bool
    eq  :: a -> a -> Maybe Bool
    ne  :: a -> a -> Maybe Bool
    le  :: a -> a -> Maybe Bool
    lt  :: a -> a -> Maybe Bool

infix 4 `gt`, `ge`, `eq`, `ne`, `le`, `lt`

instance (Ord a) => PartialOrd a where
    cmp x y = Just (compare x y)
    gt  x y = Just (x >  y)
    ge  x y = Just (x >= y)
    eq  x y = Just (x == y)
    ne  x y = Just (x /= y)
    le  x y = Just (x <= y)
    lt  x y = Just (x <  y)

----------------------------------------------------------------
----------------------------------------------------------- fin.
