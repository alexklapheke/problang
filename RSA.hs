#!/usr/bin/env runhaskell
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module      :  RSA
-- Copyright   :  Alex Klapheke 2017
-- License     :  MIT
--
-- Maintainer  :  klapheke@fas.harvard.edu
-- Stability   :  alpha
-- Portability :  unknown
--
-- Description :  RSA model described in M. Frank & N. Goodman (2012)
--                “Predicting Pragmatic Reasoning in Language Games.”
--                Science 336(6084):998. doi:10.1126/science.1218633
--

module RSA where

import Probability
import Control.Monad (join)
import Data.List (find)

data Utterance = Cost { cost :: Int, utterance :: String }

instance Eq Utterance where
    (==) (Cost _ u) (Cost _ v) = u == v

instance Show Utterance where
    show (Cost c u) = "Cost " ++ (show c) ++ " " ++ u

type QUD = String

-- Since different utterances can pick out different semantic types,
-- we fake a polymorphic meaning function with a typeclass.
class Describable a where
    meaning :: a -> Utterance -> Bool

-- The same goes for things that can be answers to the QUD
class Answerable a where
    answer :: QUD -> a -> a

softmaxNormalizeCosts :: [Utterance] -> [Utterance]
softmaxNormalizeCosts us = undefined

-- helper function for printing output to terminal
printProbs :: (Eq a, Show a) => Discrete a -> String
printProbs = unlines . map show . normalizeProbs

-- L0 (function over Discrete o)
literalListener :: (Describable o, Answerable o, Eq o)
                => Discrete o -> QUD -- background info
                -> String -> Discrete o

-- P(s|u) ∝ ⟦u⟧(s) · P(s)
literalListener objs qud utt = normalizeProbs . map (fmap (answer qud)) -- P(s)
                             $ filter ((flip meaning) (Cost 0 utt) . event) objs -- ⟦u⟧(s)

-- S1 (function over Discrete Utterance)
pragmaticSpeaker :: (Describable o, Answerable o, Eq o)
                 => Double -- optimality
                 -> Discrete o -> Discrete Utterance -> QUD -- background info
                 -> o -> Discrete Utterance

-- P(u|s) ∝ exp(α · log(L0(s|u)) - C(u))
pragmaticSpeaker opt objs utts qud obj = mapProbs effort utts
    where effort (Prob _ utt) = maybe 0 (exp . (*opt) . (subtract . fromIntegral $ cost utt) . log . prob)
                              . find (== return obj) . literalListener objs qud $ utterance utt

-- L1 (function over Discrete o)
pragmaticListener :: (Describable o, Answerable o, Eq o)
                  => Double -- assumed optimality of speaker
                  -> Discrete o -> Discrete Utterance -> Discrete QUD -- background info
                  -> String -> Discrete (QUD, o)

-- P(s|u) = S1(u|s) · P(s)
-- P(s∣u,a) ∝ S1(u∣s,a) ⋅ P(s)
pragmaticListener opt objs utts quds utt = normalizeProbs . mapProbs bayesianUpdate $ quds `cross` objs
    where bayesianUpdate (Prob _ (qud, obj)) = maybe 0 prob $ find (== return (Cost 0 utt)) . normalizeProbs $ pragmaticSpeaker opt objs utts qud obj

