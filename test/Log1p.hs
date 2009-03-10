-- ForeignFunctionInterface requires running `ffihugs` before `hugs`
-- in order to compile a DLL/.so
{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

-- With just -O2 we see marginal differences, O(0.01ns), but with
-- all of these flags they're identical.
{-# OPTIONS_GHC -O2 -fvia-C -optc-O3 -fexcess-precision #-}

module Log1p where

-- The module documents CDouble as a newtype over Double. But since
-- no isomorphism is provided, we'll just call it a Double
-- import Foreign.C (CDouble)

import Microbench
----------------------------------------------------------------

-- This is more accurate for dealing with underflow. 'hs_log1p'
-- underflows around @2 ** negate 53@, whereas c_log1p underflows
-- around @2 ** negate 1074@
foreign import ccall unsafe "math.h log1p"
    c_log1p :: Double -> Double

-- Just for fun
foreign import ccall unsafe "math.h log"
    c_log :: Double -> Double
c_log_1p :: Double -> Double
c_log_1p = \x -> c_log (1+x)

hs_log1p :: Double -> Double
hs_log1p = \x -> log (1+x)

----------------------------------------------------------------
c_log1m :: Double -> Double
c_log1m x = c_log1p (negate x)

c_log_1m :: Double -> Double
c_log_1m = \x -> c_log (1-x)

hs_log1m :: Double -> Double
hs_log1m = \x -> log (1-x)

----------------------------------------------------------------
main :: IO ()
main = do
    microbench "hs_log1p" (run hs_log1p)
    microbench "c_log_1p" (run c_log_1p)
    microbench "c_log1p"  (run c_log1p)
    putStrLn ""
    putStrLn ""
    microbench "hs_log1m" (run hs_log1m)
    microbench "c_log_1m" (run c_log_1m)
    microbench "c_log1m"  (run c_log1m)
    where
    run :: (Double -> Double) -> Int -> Double
    run f = go 1
        where
        go !a  0 = a
        go !a !i = go (a * f (2 ** negate (fromIntegral i))) (i-1)
