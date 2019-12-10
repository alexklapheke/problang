#!/usr/bin/env runhaskell
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module      :  Probability
-- Copyright   :  Alex Klapheke 2017
-- License     :  MIT
--
-- Maintainer  :  klapheke@fas.harvard.edu
-- Stability   :  alpha
-- Portability :  unknown
--
-- Description :  Very simple probability monad
--                    P(a) :: Probable a
--                    P(a|b) :: b -> Probable a
--
--                    Example:
--                        prior :: Probable a
--                        evidence :: Probable b
--                        update :: a -> Probable b
--
--                        bayes :: b -> Probable a
--                        bayes = update >=> conditionalize evidence prior

{- -- This satisfies the monad laws. Proof:

let f y = Prob p (f' y)
let g z = Prob q (g' z)
let h w = Prob r (h' w)

-- 1. Left identity: return >=> f == f
return >=> f                                                    --  by assumption
(Prob 1) >=> f                                                  --  def. return
\x -> (Prob 1 x) >>= f                                          --  def. (>=>)
\x -> conjoinProbs 1 (f x)                                      --  def. (>>=)
\x -> conjoinProbs 1 (Prob p (f' x)))                           --  def. f, β-reduction
\x -> Prob (1 * p) (f' x)                                       --  def. conjoinProbs
\x -> Prob p (f' x)                                             --  mult. identity
f                                                               --  α-conversion, def. f

-- 2. Right identity: f >=> return == f
f >=> return                                                    --  by assumption
f >=> (Prob 1)                                                  --  def. return
\x -> f x >>= Prob 1                                            --  def. (>=>)
\x -> Prob p (f' x) >>= Prob 1                                  --  def. f, β-reduction
\x -> conjoinProbs p (Prob 1 (f' x))                            --  def. (>>=)
\x -> Prob (1 * p) (f' x)                                       --  def. conjoinProbs
\x -> Prob p (f' x)                                             --  mult. identity
f                                                               --  α-conversion, def. f

-- 3. Associativity: (f >=> g) >=> h == f >=> (g >=> h)
(f >=> g) >=> h                                                 --  by assumption
(\x -> f x >>= g) >=> h                                         --  def. (>=>)
\y -> ((\x -> f x >>= g) y >>= h)                               --  def. (>=>), α-conversion
\y -> ((f y >>= g) >>= h)                                       --  β-reduction
\y -> ((Prob p (f' y) >>= g) >>= h)                             --  def. f, β-reduction
\y -> ((conjoinProbs p (g (f' y))) >>= h)                       --  def. (>>=)
\y -> ((conjoinProbs p (Prob q (g' (f' y)))) >>= h)             --  def. g, β-reduction
\y -> ((Prob (p * q) (g' (f' y))) >>= h)                        --  def. conjoinProbs
\y -> ((Prob (p * q) ((g' . f') y)) >>= h)                      --  def. (.)
\y -> ((Prob (p * q) ((g' . f') y)) >>= (\w -> Prob r (h' w)))  --  def. h
\y -> (conjoinProbs (p * q) (Prob r (h' ((g' . f') y))))        --  def. (>>=), β-reduction
\y -> (Prob ((p * q) * r) (h' ((g' . f') y)))                   --  def. conjoinProbs
\y -> (Prob ((p * q) * r) ((h' . (g' . f')) y))                 --  def. (.)
\y -> (Prob (p * (q * r)) (((h' . g') . f') y))                 --  assoc.
\y -> (Prob (p * (q * r)) (((h' . g') (f' y))))                 --  def. (.)
\y -> (conjoinProbs p (Prob (q * r) ((h' . g') (f' y))))        --  def. conjoinProbs
\y -> (Prob p (f' y) >>= (\x -> Prob (q * r) ((h' . g') x)))    --  def. (>>=)
\y -> (f y >>= (\x -> Prob (q * r) ((h' . g') x)))              --  def. f
\y -> (f y >>= (\x -> Prob (q * r) (h' (g'  x))))               --  def. (.)
\y -> (f y >>= (\x -> conjoinProbs q (Prob r (h' (g' x)))))     --  def. conjoinProbs
\y -> (f y >>= (\x -> conjoinProbs q (h (g' x))))               --  def. h
\y -> (f y >>= (\x -> (Prob q (g' x) >>= h)))                   --  def. (>>=)
\y -> (f y >>= (\x -> (g x >>= h)))                             --  def. g
\y -> (f y >>= (g >=> h))                                       --  def. (>=>)
f >=> (g >=> h)                                                 --  α-conversion, def. (>=>)

-- Q.E.D. -}

module Probability where

import Data.List (foldl', foldl1')
import Control.Monad (join, liftM)
import Control.Monad.Trans

data Probable a = Prob { prob :: Double, event :: a }

instance (Show a) => Show (Probable a) where
    show (Prob p x) = "Prob " ++ (show p) ++ " " ++ (show x)

type Discrete a = [Probable a]
type Continuous a = a -> Probable a

-- instance Functor (Discrete a) where
--     fmap = map . fmap

conjoinProbs :: Double -> Probable b -> Probable b
conjoinProbs q (Prob p x) = Prob (p * q) x

conditionalize :: (Eq a) => Probable a -> Probable b -> a -> Probable b
conditionalize (Prob condition a) (Prob event b) x
    | x == a = Prob (event/condition) b
    | otherwise = Prob 0 b

-- takes effort function and returns function over distributions
mapProbs :: (Probable a -> Double) -> Discrete a -> Discrete a
mapProbs f = fmap (join $ conjoinProbs . f)

-- remove Prob 0 events and make remaining probabilities add to 1
normalizeProbs :: (Eq a) => Discrete a -> Discrete a
normalizeProbs ps = fmap (conjoinProbs (1/total)) . combineDupes $ filter nonzero ps
    where total = foldl' (\q (Prob p _) -> p + q) 0 ps
          nonzero (Prob p _) = p > 0
          combineDupes [] = []
          combineDupes (x:xs)
              | x `elem` xs = map (addProbs x) $ combineDupes xs
              | otherwise   = x:(combineDupes xs)
          addProbs (Prob p x) (Prob q y) = if x == y then (Prob (p+q) y) else (Prob q y)

-- return the cartesian product of lists of probable events
cross :: (Eq a, Eq b) => Discrete a -> Discrete b -> Discrete (a, b)
cross xs ys = normalizeProbs [Prob (p*q) (x,y) | (Prob p x) <- xs, (Prob q y) <- ys]

-- two objects with different probabilities are *still equal*
instance (Eq a) => Eq (Probable a) where
    (Prob _ x) == (Prob _ y) = x == y

instance Functor Probable where
    fmap f (Prob p x) = Prob p (f x)

instance Monad Probable where
    return = Prob 1
    (>>=) (Prob p x) f = conjoinProbs p (f x)

instance Applicative Probable where
    pure = Prob 1
    (<*>) (Prob p f) (Prob q x) = Prob (p*q) $ f x

-- Probability distributions

uniform :: (Eq a) => [a] -> Discrete a
uniform = normalizeProbs . fmap return

normal :: Double -> Double -> Continuous Double
normal mu sigma x = Prob (exp (-(x - mu)^2 / (2 * sigma^2)) / sqrt (2 * pi * sigma^2)) x

binomial :: (Integral a) => Double -> a -> Discrete a
binomial p n = normalizeProbs $ map binomial' [0..n]
    where
          binomial' k = Prob ((n `choose` k) * (p^k) * (1-p)^(n-k)) k

hypergeometric :: (Integral a) => a -> a -> a -> Discrete a
hypergeometric n' k' n = normalizeProbs $ map hypergeometric' [0..n]
    where
        hypergeometric' k = Prob ((k' `choose` k) * ((n'-k') `choose` (n-k)) / (n' `choose` n)) k

poisson :: Integral a => Double -> Continuous a
poisson lambda k = Prob ((lambda^k * exp (-lambda))/(fromIntegral $ factorial k)) k
    where factorial 0 = 1
          factorial n = foldl1' (*) [1..n]

beta :: Double -> Double -> Continuous Double
beta g d x = Prob ((x ** (g*d)-1) * ((1-x) ** ((1-g)*d)-1)) x

-- turn a continuous distribution into a discrete one
sample :: (Eq a, Enum a, Fractional a) => a -> a -> a -> Continuous a -> Discrete a
sample min max step dist = map (dist . (*step)) [min..(max-min)/step]

-- should test for equality
klDivergence :: Discrete a -> Discrete a -> Double
klDivergence p q = sum $ zipWith (\p' q' -> log ((prob p')/(prob q'))) p q

-- binomial helper function
choose :: (Integral a, Num b) => a -> a -> b
choose n k = fromIntegral $ product [1+n-k..n] `div` product [1..k]
