%% This module should be run through lhs2hs.pl before running through
%% Haddock. (N.B. rember to include a copy in the cabalized)
%%
%% This module was originally translated from my Perl module
%% Math::LogFloat (version 0.3; revision 2007.12.20)
%% 
%% N.B. Can't have `#' in the first column in GHC, not even if lhs

TODO: Add QuickCheck-ness, though beware of the fuzz.
TODO: Make sure rewrite rules really fire
TODO: profile to make sure we don't waste too much time constructing dictionaries

To turn on optimizations and look at the optimization records, cf:
http://www.haskell.org/ghc/docs/latest/html/users_guide/rewrite-rules.html
http://www.randomhacks.net/articles/2007/02/10/map-fusion-and-haskell-performance

> -- {-# OPTIONS_GHC -ddump-simpl-stats #-}
>
> {-# OPTIONS_GHC -O2 -fvia-C -optc-O3 #-}

Version History
(v0.8) Did a bunch of tweaking. Things should be decent now
(v0.7) Haddockified
(v0.6) Fixed monomorphism.
(v0.5) Added optimization rules.
(v0.4) Translated to Haskell at revision 2007.12.20.
(v0.3) Converted extensive comments to POD format.
(v0.2) Did a bunch of profiling, optimizing, and debugging.
(v0.1) Initial version created for hw5 for NLP with Jason Eisner.

----------------------------------------------------------------
                                                    ~ 2008.08.15
|
Module      :  Data.Number.LogFloat
Copyright   :  Copyright (c) 2007--2008 wren ng thornton
License     :  BSD3
Maintainer  :  wren@community.haskell.org
Stability   :  stable
Portability :  portable

This module presents a class for storing numbers in the log-domain.
The main reason for doing this is to prevent underflow when multiplying
many small probabilities as is done in Hidden Markov Models and
other statistical models often used for natural language processing.
The log-domain also helps prevent overflow when multiplying many
large numbers. In rare cases it can speed up numerical computation
(since addition is faster than multiplication, though logarithms
are exceptionally slow), but the primary goal is to improve accuracy
of results. A secondary goal has been to maximize efficiency since
these computations are frequently done within a /O(n^3)/ loop.

The 'LogFloat' of this module is restricted to non-negative numbers
for efficiency's sake, see the forthcoming "Data.Number.LogFloat.Signed"
for doing signed log-domain calculations.
----------------------------------------------------------------

> module Data.Number.LogFloat
>     (
>     -- * IEEE floating-point special values
>     -- | "GHC.Real" defines 'infinity' and 'notANumber' as
>     -- 'Rational'. We export variants which are polymorphic because
>     -- that can be more helpful at times.
>
>       infinity, negativeInfinity, notANumber
>
>     -- * Basic functions
>     , log, toFractional
>
>     -- * @LogFloat@ data type and conversion functions
>     , LogFloat
>     , logFloat,     logToLogFloat
>     , fromLogFloat, logFromLogFloat
>     ) where
> 
> import Prelude hiding (log)
> import qualified Prelude (log)
>
> -- Not portable, and we can do it ourselves.
> -- import qualified GHC.Real (infinity, notANumber)

----------------------------------------------------------------

Try to add in some optimizations. Why the first few need to be down
here and localized to the module, I don't know. We don't do anything
foolish like this, but our clients might or they might be generated
by other code transformations.

> {-# RULES
> "log/exp"  forall x. log (exp x) = x
> "log.exp"            log . exp   = id
>
> "exp/log"  forall x. exp (log x) = x
> "exp.log"            exp . log   = id
>     #-}

These are general rule versions of our operators for 'LogFloat'. I
had some issues inducing 'Ord' on @x@ and @y@, even though they're
'Num' so I can't do "(+)/log" and "(-)/log" so easily.

> {-# RULES
> "(*)/log"  forall x y. log x * log y = log (x + y)
> "(/)/log"  forall x y. log x / log y = log (x - y)
>     #-}


----------------------------------------------------------------

The type signature is necessary for them not to default to Double.

> infinity, negativeInfinity, notANumber :: (Fractional a) => a
> infinity         = 1 / 0               -- == fromRational GHC.Real.infinity
> {-# SPECIALIZE negativeInfinity :: Double #-}
> negativeInfinity = negate infinity
> notANumber       = infinity - infinity -- == fromRational GHC.Real.notANumber

The dictionaries for these are really ugly in core.
TODO: be sure to check that these don't give eggregious performance hits

----------------------------------------------------------------

| Since the normal 'Prelude.log' throws an error on zero, we have
to redefine it in order for things to work right. Arguing from
limits it's obvious that @log 0 == negativeInfinity@.

If you're using some 'Floating' type that's not built in, verify
this equation holds for your @0@ and @negativeInfinity@. If it
doesn't, then you should avoid importing our 'log' and will probably
want converters to handle the discrepency.

> {-# SPECIALIZE log :: Double -> Double #-}
> log  :: (Floating a) => a -> a
> log 0 = negativeInfinity
> log x = Prelude.log x


| The most generic numeric converter I can come up with. All the
built-in numeric types are 'Real', though 'Int' and 'Integer' aren't
'Fractional'.

> {-# SPECIALIZE toFractional :: (Real a)       => a -> Double #-}
> {-# SPECIALIZE toFractional :: (Fractional b) => Double -> b #-}
> toFractional :: (Real a, Fractional b) => a -> b
> toFractional  = fromRational . toRational
>
> -- This should only fire when it's type-safe
> {-# RULES "toFractional/id" toFractional = id #-}
>
> -- This should happen already, but who knows
> -- TODO: see if it ever fires
> {-# RULES
> "toFractional/toFractional"  forall x.
>                              toFractional (toFractional x) = toFractional x
> "toFractional.toFractional"  toFractional . toFractional   = toFractional
>     #-}


----------------------------------------------------------------

| Reduce the number of constant string literals we need to store.

> errorOutOfRange    :: String -> a
> errorOutOfRange fun = error $ "Data.Number.LogFloat."++fun
>                            ++ ": argument out of range"


| We need these guards in order to ensure some invariants.

> guardNonNegative      :: String -> Double -> Double
> guardNonNegative fun x | x >= 0    = x
>                        | otherwise = errorOutOfRange fun

|  It's unfortunate that notANumber is not equal to itself, but we
can hack around that. Is there any efficiency difference between
these two tests? If not, then we could use @log . guardNonNegative
fun = guardIsANumber fun . log@ in order to remove guardNonNegative.

> guardIsANumber        :: String -> Double -> Double
> guardIsANumber   fun x | x >= negativeInfinity = x
>                        | otherwise             = errorOutOfRange fun

----------------------------------------------------------------

| A @LogFloat@ is just a 'Double' with a special interpretation.
The 'logFloat' function is presented instead of the constructor,
in order to ensure semantic conversion. At present the 'Show'
instance will convert back to the normal-domain, and so will underflow
at that point. This behavior may change in the future.

Performing operations in the log-domain is cheap, prevents underflow,
and is otherwise very nice for dealing with miniscule probabilities.
However, crossing into and out of the log-domain is expensive and
should be avoided as much as possible. In particular, if you're
doing a series of multiplications as in @lp * logFloat q * logFloat
r@ it's faster to do @lp * logFloat (q * r)@ if you're reasonably
sure the normal-domain multiplication won't underflow, because that
way you enter the log-domain only once, instead of twice.

Even more particularly, you should /avoid addition/ whenever possible.
Addition is provided because it's necessary at times and the proper
implementation is not immediately transparent. However, between two
@LogFloat@s addition requires crossing the exp/log boundary twice;
with a @LogFloat@ and a regular number it's three times since the
regular number needs to enter the log-domain first. This makes addition
incredibly slow. Again, if you can parenthesize to do plain operations
first, do it!

> newtype LogFloat = LogFloat Double
>     deriving (Eq, Ord)


| A constructor which does semantic conversion from normal-domain
to log-domain.

> {-# SPECIALIZE logFloat :: Double -> LogFloat #-}
> logFloat :: (Real a) => a -> LogFloat
> logFloat  = LogFloat . log . guardNonNegative "logFloat" . toFractional


This is simply a polymorphic version of the 'LogFloat' data
constructor. We present it mainly because we hide the constructor
in order to make the type a bit more opaque. If the polymorphism
turns out to be a performance liability because the rewrite rules
can't remove it, then we need to rethink all four constructors/destructors.

| Constructor which assumes the argument is already in the log-domain.

> {-# SPECIALIZE logToLogFloat :: Double -> LogFloat #-}
> logToLogFloat :: (Real a) => a -> LogFloat
> logToLogFloat  = LogFloat . guardIsANumber "logToLogFloat" . toFractional


| Return our log-domain value back into normal-domain. Beware of
overflow/underflow.

> {-# SPECIALIZE fromLogFloat :: LogFloat -> Double #-}
> fromLogFloat :: (Floating a) => LogFloat -> a
> fromLogFloat (LogFloat x) = toFractional (exp x)


| Return the log-domain value itself without costly conversion

> {-# SPECIALIZE logFromLogFloat :: LogFloat -> Double #-}
> logFromLogFloat :: (Floating a) => LogFloat -> a
> logFromLogFloat (LogFloat x) = toFractional x


These are our module-specific versions of "log/exp" and "exp/log";
They do the same things but also have a @LogFloat@ in between the
logarithm and exponentiation.

> {-# RULES
> -- Out of log-domain and back in
> "log/fromLogFloat"       forall x. log (fromLogFloat x) = logFromLogFloat x
> "log.fromLogFloat"                 log . fromLogFloat   = logFromLogFloat
>
> "logFloat/fromLogFloat"  forall x. logFloat (fromLogFloat x) = x
> "logFloat.fromLogFloat"            logFloat . fromLogFloat   = id
>
> -- Into log-domain and back out
> "fromLogFloat/logFloat"  forall x. fromLogFloat (logFloat x) = x
> "fromLogFloat.logFloat"            fromLogFloat . logFloat   = id
>     #-}

----------------------------------------------------------------
To show it, we want to show the normal-domain value rather than the
log-domain value. Also, if someone managed to break our invariants
(e.g. by passing in a negative and noone's pulled on the thunk yet)
then we want to crash before printing the constructor, rather than
after.  N.B. This means the show will underflow/overflow in the
same places as normal doubles since we underflow at the exp. Perhaps
this means we should show the log-domain value instead.

> instance Show LogFloat where
>     show (LogFloat x) = let y = exp x
>                         in  y `seq` "LogFloat "++show y


----------------------------------------------------------------
These all work without causing underflow. However, do note that
they tend to induce more of the floating-point fuzz than using
regular floating numbers because @exp . log@ doesn't really equal
@id@. In any case, our main aim is for preventing underflow when
multiplying many small numbers (and preventing overflow for multiplying
many large numbers) so we're not too worried about +/- 4e-16.

> instance Num LogFloat where 
>     (*) (LogFloat x) (LogFloat y) = LogFloat (x+y)
>
>     (+) (LogFloat x) (LogFloat y)
>         | x >= y    = LogFloat (x + log (1 + exp (y - x)))
>         | otherwise = LogFloat (y + log (1 + exp (x - y)))
>
>     -- Without the guard this would return NaN instead of error
>     (-) (LogFloat x) (LogFloat y)
>         | x >= y    = LogFloat (x + log (1 - exp (y - x)))
>         | otherwise = errorOutOfRange "(-)"
>
>     signum (LogFloat x)
>         | x == negativeInfinity = 0
>         | x >  negativeInfinity = 1
>         | otherwise             = errorOutOfRange "signum"
>         -- The extra guard protects against NaN, in case someone
>         -- broke the invariant. That shouldn't be possible and
>         -- so noone else bothers to check, but we check here just
>         -- in case.
>
>     negate _    = errorOutOfRange "negate"
>
>     abs         = id
>
>     fromInteger = LogFloat . log
>                 . guardNonNegative "fromInteger" . fromInteger
>
>
> instance Fractional LogFloat where
>     -- n/0 is handled seamlessly for us; we must catch 0/0 though
>     (/) (LogFloat x) (LogFloat y)
>         |    x == negativeInfinity
>           && y == negativeInfinity = errorOutOfRange "(/)" -- protect vs NaN
>         | otherwise                = LogFloat (x-y)
>     
>     fromRational = LogFloat . log
>                  . guardNonNegative "fromRational" . fromRational
>
>
> -- Just for fun. The more coersion functions the better. Though
> -- it can underflow...
> instance Real LogFloat where
>     toRational (LogFloat x) = toRational (exp x)

----------------------------------------------------------------
----------------------------------------------------------- fin.
