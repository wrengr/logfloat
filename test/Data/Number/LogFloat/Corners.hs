{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2021.10.17
-- |
-- Module      :  Data.Number.LogFloat.Corners
-- Copyright   :  Copyright (c) 2007--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  stable
-- Portability :  portable (with CPP)
--
-- This module tests the numeric operations for corner cases, namely
-- involving transfinite values.
----------------------------------------------------------------

module Data.Number.LogFloat.Corners where

import Data.Number.LogFloat

-- normal-space 0        is represented by negativeInfinity
-- normal-space 1        is represented by 0
-- normal-space infinity is represented by infinity
cornersD :: [Double]
cornersD = [0,1,2,infinity]

cornersLF :: [LogFloat]
cornersLF = map logFloat cornersD

main :: IO ()
main = do
    -- TODO: make these verify the answers, instead of needing to do it manually
    prints "*" [(x,y,x*y) | x <- cornersLF, y <- cornersLF]
    prints "+" [(x,y,x+y) | x <- cornersLF, y <- cornersLF]
    prints "-" [(x,y,x-y) | x <- cornersLF, y <- cornersLF
                          , x >= y, not(x==inf && y==inf)]
    prints "/" [(x,y,x/y) | x <- cornersLF, y <- cornersLF
                          , not(x==0 && y==0), not(x==inf && y==inf)]
    where
    prints op = putStrLn . unlines . map (show3 . fromLFs)
        where
        show3 (x,y,z) = sh x ++" "++op++" "++ sh y ++" == "++ sh z
        sh = take 3 . show

    fromLFs :: (LogFloat,LogFloat,LogFloat) -> (Double,Double,Double)
    fromLFs (x,y,z) = (fromLogFloat x, fromLogFloat y, fromLogFloat z)

    inf = logFloat (infinity::Double)

----------------------------------------------------------------
----------------------------------------------------------- fin.
