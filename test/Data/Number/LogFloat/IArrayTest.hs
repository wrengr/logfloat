{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.03.07
-- |
-- Module      :  Data.Number.LogFloat.IArrayTest
-- Copyright   :  Copyright (c) 2007--2009 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable (with CPP)
--
-- This module tests the 'unsafeCoerce' version of @IArray UArray LogFloat@
-- instance. That instance and these tests were provided by Felipe
-- Lessa. For GHC 6.8 and above, the instance is automatically
-- derived and so these tests are unnecessary (but should still
-- pass).
--
-- Since SmallCheck isn't included in Hugs (Sept 2006), pass a flag
-- to enable SmallCheck tests. An invocation like the following
-- should suffice:
--
-- @hugs -98 +o -F'cpp -P -traditional -D__HUGS__=200609 -D__USE_SMALLCHECK__'@
----------------------------------------------------------------

module Data.Number.LogFloat.IArrayTest where

import Data.Number.LogFloat

import Data.Array.Unboxed as U

import Test.QuickCheck
#ifdef __USE_SMALLCHECK__
import Test.SmallCheck
#endif

----------------------------------------------------------------
prop_listArray :: [Double] -> Bool
prop_listArray xs  =  xs' == U.elems arr
    where
    xs' = map (logFloat . abs) xs
    
    arr  :: UArray Int LogFloat
    arr   = U.listArray (1, length xs') xs'


prop_accumArray :: [Double] -> Bool
prop_accumArray xs  =  product xs' == arr U.! 1
    where
    xs' = map (logFloat . abs) xs
    
    arr  :: UArray Int LogFloat
    arr   = U.accumArray (*) 1 (1, 1) [(1,x) | x <- xs']


main :: IO ()
main = do
    quickCheck prop_listArray
    quickCheck prop_accumArray
    
    -- Trying to guard on the length of the list won't work. Using
    -- SmallCheck instead
#ifdef __USE_SMALLCHECK__
    smallCheck 5 prop_listArray
    smallCheck 5 prop_accumArray
#endif
    
    checkMore 1000 prop_listArray
    checkMore 1000 prop_accumArray
    where
    checkMore n = check (defaultConfig
                        { configMaxTest = n
                        , configMaxFail = n `div` 10
                        })

----------------------------------------------------------------
----------------------------------------------------------- fin.
