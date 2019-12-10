#!/usr/bin/env runhaskell
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Probability
import RSA
import Control.Monad (join)
import Data.List (find)

main :: IO ()
main = do
    let objects = uniform [0, 1, 2, 3] :: [Probable Object]
    let utterances = uniform $ map (Cost 0) ["all", "some", "none"]
    let alpha = 1.0
    let qud = ""

    putStrLn "Literal listener hears \"some\":"
    putStrLn . printProbs $ literalListener objects qud "some"

    putStrLn "Pragmatic speaker sees \"2\":"
    putStrLn . printProbs $ pragmaticSpeaker alpha objects utterances qud 2

    putStrLn "Pragmatic speaker sees \"3\":"
    putStrLn . printProbs $ pragmaticSpeaker alpha objects utterances qud 3

    putStrLn "Pragmatic listener hears \"some\":"
    putStrLn . printProbs $ pragmaticListener alpha objects utterances [Prob 1 qud] "some"

    -- Adding belief states:
    let baserate = 0.8
    let objects = [sequence [a,b,c] | a <- prior baserate
                                    , b <- prior baserate
                                    , c <- prior baserate
                  ]
                  where prior baserate = [ Prob baserate True
                                         , Prob (1-baserate) False ]

    let access = [True, True, False]
    let state  = [True, True, True]

    let objectSpace = filterObjects access state objects

    putStrLn "Speaker's observation:"
    putStrLn . printProbs $ objectSpace

    putStrLn "Literal listener hears \"some\":"
    putStrLn . printProbs $ literalListener objects qud "some"

    putStrLn "Literal listener hears \"all\":"
    putStrLn . printProbs $ literalListener objects qud "all"

    putStrLn "Partial-access speaker sees [Red, Red, ??]:"
    putStrLn . printProbs $ mapDist (pragmaticSpeaker alpha objects utterances qud) objectSpace

    -- putStrLn "Pragmatic listener hears \"some\":"
    -- putStrLn . unlines . map show $ pragmaticListener alpha objects utterances [Prob 1 qud] (Cost 0 "some")

-- Without belief states
type Object = Int

instance Describable Object where
    meaning obj utt
        | utterance utt == "all"  = obj == 3
        | utterance utt == "some" = obj > 0
        | utterance utt == "none" = obj == 0

instance Answerable Object where
    answer _ = id

-- With belief states
type ObjectList = [Bool]

instance Describable ObjectList where
    meaning objlist utt
        | utterance utt == "all"  = count objlist == 3
        | utterance utt == "some" = count objlist > 0
        | utterance utt == "none" = count objlist == 0
        where
            count = length . filter id

instance Answerable ObjectList where
    answer _ = id

-- take an access state (which apples can be seen) and the actual state (which apples are red)
-- and return a function that filters for states compatible with the speaker's access
filterObjects :: [Bool] -> [Bool] -> Discrete ObjectList -> Discrete ObjectList
filterObjects access state = normalizeProbs . filter (and . zipWith3 (\a s t -> not a || s==t) access state . event)

