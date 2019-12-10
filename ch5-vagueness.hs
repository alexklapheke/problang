#!/usr/bin/env runhaskell
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Probability
import RSA
import Control.Monad (join)
import Data.List (find)

main :: IO ()
main = do
    let objects theta = normalizeProbs
                    [ Prob 1 (2, theta)
                    , Prob 2 (6, theta)
                    , Prob 3 (10, theta)
                    , Prob 4 (14, theta)
                    , Prob 4 (18, theta)
                    , Prob 3 (22, theta)
                    , Prob 2 (26, theta)
                    , Prob 1 (30, theta)
                    ] :: Discrete Object
    let utterances = uniform ["expensive", ""]

    let objectsThetas = concatMap objects [1..30]

    let alpha = 1.0
    let qud = ""

    putStrLn "Literal listener hears \"expensive\":"
    putStrLn . printProbs $ literalListener (objects 20) qud "expensive"

    putStrLn "Pragmatic speaker paid $18:"
    putStrLn . printProbs $ pragmaticSpeaker alpha (objectsThetas) utterances qud (30, 22)

    -- putStrLn "Pragmatic listener hears \"some\":"
    -- putStrLn . show $ pragmaticListener alpha objects utterances [Prob 1 qud] "some"

data Item = CoffeeMaker | Headphones | Laptop | Sweater | Watch

type Object = (Int, Int)

-- instance Show Object where
--     show (price, theta) = "$" ++ (show price) ++ " with threshold $" ++ (show theta)

instance Describable Object where
    meaning (price, theta) utt
        | utt == "expensive" = price >= theta
        | otherwise = True

instance Answerable Object where
    answer _ = id
