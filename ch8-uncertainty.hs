#!/usr/bin/env runhaskell
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Probability
import RSA
import Control.Monad (join)
import Data.List (find)

main :: IO ()
main = do
    let objects = undefined

    let alpha = 1
    let qud = ""

    putStrLn "Literal listener hears \"...\":"
    putStrLn . show $ literalListener objects qud ""

    putStrLn "Pragmatic speaker sees ..."
    putStrLn . show $ pragmaticSpeaker alpha objects utterances qud ""

    putStrLn "Pragmatic listener hears \"...\":"
    putStrLn . show $ pragmaticListener alpha objects utterances [Prob 1 qud] ""

type Object = 

instance Describable Object where
    meaning obj utt

instance Answerable Object where
    answer _ = id
