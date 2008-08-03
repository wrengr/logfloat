#!/usr/bin/env runhaskell
-- A trivial converter from literate Haskell to plain Haskell
-- wren ng thornton <wren@commuity.haskell.org>     ~ 2008.08.01
----------------------------------------------------------------
--
-- N.B. This is intended for heavily Haddocked files and may do
-- strange things to LaTeX literate Haskell. In particular it only
-- respects the birdtrack style of literacy not the TeX style.

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -O2 -fvia-C -optc-O3 #-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (Handle, hIsEOF, stdin, stdout, withFile, IOMode(..))
import Data.Char          (isSpace)
import Control.Monad      (unless)
import qualified Data.ByteString.Char8 as S
----------------------------------------------------------------

-- TODO: get filenames from getArgs, etc
main :: IO ()
main  = do
    args <- getArgs
    case args of
         []         -> runLhs2hs "-" "-"
         [inF]      -> runLhs2hs inF "-"
         [inF,outF] -> runLhs2hs inF outF
         _          -> do putStrLn "Usage: lhs2hs [infile [outfile]]"
                          exitFailure
    where
    {-# INLINE runLhs2hs #-}
    runLhs2hs inF outF = withFH inF ReadMode stdin (\inH -> 
                             withFH outF WriteMode stdout (\outH ->
                                 lhs2hs inH outH))
{-# INLINE withFH #-}
withFH :: FilePath -> IOMode -> Handle -> (Handle -> IO r) -> IO r
withFH "-"  _    fh act = act fh
withFH path mode _  act = withFile path mode act


----------------------------------------------------------------
-- | The main routine
lhs2hs         :: Handle -> Handle -> IO ()
lhs2hs inH outH = start
    where
    aComment     = S.pack "-- "
    aNewline     = S.pack "\n"
    aCommentLine = S.pack "--\n"
    
    prints  bs = S.hPut outH bs
    println bs = S.hPut outH bs >> S.hPut outH aNewline
    
    
    start   = go 0 aCommentLine
    go i nl =
        hTryGetLine inH $ \line ->
            case S.uncons line of
                 Nothing                             -> doBlank
                 Just ('>',xs) | S.null xs           -> doCode xs
                               | isSpace (S.head xs) -> doCode (S.tail xs)
                 Just _        | S.all isSpace line  -> doBlank
                               | otherwise           -> doText line
        where
        doBlank     =    (go $! i+1) nl
        
        doCode line = do prints  (S.replicate i '\n')
                         println line
                         go 0 aNewline
        
        doText line = do prints (recyclicate i nl)
                         unless (S.all ('-' ==) line)
                                (prints aComment)
                         println line
                         go 0 aCommentLine

-- Perlish while(<>), but we need to tie the loop ourselves
-- since we're passing state
hTryGetLine    :: Handle -> (S.ByteString -> IO ()) -> IO ()
hTryGetLine h m = do b <- hIsEOF h ; unless b (S.hGetLine h >>= m)


-- cycle + replicate
recyclicate     :: Int -> S.ByteString -> S.ByteString
recyclicate i xs | i > 0     = xs `S.append` (recyclicate $! i-1) xs
                 | otherwise = S.empty

----------------------------------------------------------------
----------------------------------------------------------- fin.
