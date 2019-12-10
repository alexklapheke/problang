#!/usr/bin/env runhaskell
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

import Probability
import RSA

main :: IO ()
main = do
    let states = uniform [0..3]
    let scopes = uniform [Surface, Inverse]
    let objects = cross states scopes :: Discrete Object

    -- let objects = [Prob (1/6) (0, Surface), Prob (1/6) (0, Inverse), Prob (1/3) (1, Inverse), Prob (1/3) (2, Inverse)] :: Discrete Object

    let utterances = uniform $ map (Cost 0) ["every-not", "null"]

    let alpha = 1
    let qud = ""

    putStrLn "Literal listener hears \"every not\":"
    putStrLn . printProbs $ literalListener objects "" "every-not"

    putStrLn "Pragmatic speaker sees 3 objects:"
    putStrLn . printProbs $ mapDist (pragmaticSpeaker alpha objects utterances "") (uniform [(3, Surface), (3, Inverse)])
    putStrLn "Pragmatic speaker sees 2 objects:"
    putStrLn . printProbs $ mapDist (pragmaticSpeaker alpha objects utterances "") (uniform [(2, Surface), (2, Inverse)])
    putStrLn "Pragmatic speaker sees 1 objects:"
    putStrLn . printProbs $ mapDist (pragmaticSpeaker alpha objects utterances "") (uniform [(1, Surface), (1, Inverse)])
    putStrLn "Pragmatic speaker sees 0 objects:"
    putStrLn . printProbs $ mapDist (pragmaticSpeaker alpha objects utterances "") (uniform [(0, Surface), (0, Inverse)])

    putStrLn "Pragmatic listener hears \"every not\":"
    putStrLn . printProbs $ pragmaticListener alpha objects utterances [Prob 1 qud] "every-not"

data Scope = Surface | Inverse deriving (Eq, Show)

type Object = (Int, Scope)

instance Describable Object where
    meaning (n, scope) utt
        | utterance utt == "every-not" = if scope == Inverse then n < 3 else n == 0
        | otherwise = True

instance Answerable Object where
    answer _ = id


