#!/usr/bin/env runhaskell

module Main (main) where

-- <http://www.haskell.org/ghc/docs/latest/html/libraries/Cabal/Distribution-Simple.html>
import Distribution.Simple
import Distribution.Simple.Setup          (CleanFlags, HaddockFlags)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.PackageDescription    (HookedBuildInfo
                                          , emptyHookedBuildInfo
                                          , PackageDescription
                                          )
import System.Cmd                         (system)

main :: IO ()
main  = defaultMainWithHooks defaultUserHooks
      { preHaddock = preHaddockScript
      , postClean  = postCleanScript
      }


preHaddockScript    :: Args -> HaddockFlags -> IO HookedBuildInfo
preHaddockScript _ _ = do 
    putStrLn "Building lhs2hs..."
    system "ghc --make lhs2hs.hs -o lhs2hs"
    putStrLn "Illiterating Data.Number.LogFloat for Haddock..."
    system "cat Data/Number/LogFloat.lhs | ./lhs2hs > Data/Number/LogFloat.hs"
    return emptyHookedBuildInfo


postCleanScript :: Args
                -> CleanFlags
                -> PackageDescription
                -> Maybe LocalBuildInfo
                -> IO ()
postCleanScript _ _ _ _ = do 
    putStrLn $ "removing files: " ++ commafy files
    removeAll files
    where
    files     = ["Data/Number/LogFloat.hs", "lhs2hs", "lhs2hs.hi", "lhs2hs.o"]
    
    removeAll = sequence_ . map (system . (++) "rm -f ")
    
    commafy []           = ""
    commafy [x]          = x
    commafy (x:xs@(_:_)) = x++", "++commafy xs
