#!/usr/bin/env runhaskell
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

import Probability
import RSA

main :: IO ()
main = do
    let states = [ (Prob 0.4205 50,    Prob 0.3173 True)
                 , (Prob 0.3865 51,    Prob 0.3173 True)
                 , (Prob 0.0533 500,   Prob 0.7920 True)
                 , (Prob 0.0538 501,   Prob 0.7920 True)
                 , (Prob 0.0223 1000,  Prob 0.8933 True)
                 , (Prob 0.0211 1001,  Prob 0.8933 True)
                 , (Prob 0.0112 5000,  Prob 0.9524 True)
                 , (Prob 0.0111 5001,  Prob 0.9524 True)
                 , (Prob 0.0083 10000, Prob 0.9864 True)
                 , (Prob 0.0120 10001, Prob 0.9864 True)
                 ]

    let utterances = uniform $ map (Cost 0) ["50", "500", "1000", "5000", "10000"]
                            ++ map (Cost 1) ["51", "501", "1001", "5001", "10001"] :: [Probable Utterance]

    let quds = uniform ["price", "valence", "priceValence", "approxPrice", "approxPriceValence"]

    let objects = [Prob (p*q) (PriceValence (st, True))      | (Prob p st, Prob q _) <- states] ++
                  [Prob (p*(1-q)) (PriceValence (st, False)) | (Prob p st, Prob q _) <- states] ++
                  [Prob p (Price st)                         | (Prob p st, _)        <- states] ++
                  [Prob q (Valence True)                     | (Prob p st, Prob q _) <- states] ++
                  [Prob (1-q) (Valence False)                | (Prob p st, Prob q _) <- states] :: [Probable Object]

    let alpha = 1

    -- debug
    -- putStrLn . printProbs $ objects

    putStrLn "Literal listener hears \"10,000\":"
    putStrLn . printProbs $ literalListener objects "priceValence" "10000"

    putStrLn "Pragmatic speaker paid $500:"
    putStrLn . printProbs $ pragmaticSpeaker alpha objects utterances "price" (Price 500)

    putStrLn "Pragmatic speaker is unhappy:"
    putStrLn . printProbs $ pragmaticSpeaker alpha objects utterances "valence" (Valence True)

    putStrLn "Pragmatic listener hears \"10,000\":"
    putStrLn . printProbs $ pragmaticListener alpha objects utterances quds "10000"

    putStrLn "Pragmatic listener hears \"10,001\":"
    putStrLn . printProbs $ pragmaticListener alpha objects utterances quds "10001"

data Object = Price Int
            | Valence Bool -- True = unhappy, False = happy
            | PriceValence (Int, Bool)
            deriving (Eq)

instance Show Object where
    show (Price s) = "$" ++ show s
    show (Valence v) = if v then "☹" else "☺"
    show (PriceValence (s,v)) = show (Price s) ++ show (Valence v)

instance Describable Object where
    meaning (Price s)            utt = utterance utt == show s
    meaning (Valence v)          utt = False
    meaning (PriceValence (s,v)) utt = utterance utt == show s

instance Answerable Object where
    answer qud (PriceValence (s,v))
        | qud == "price"              = Price s
        | qud == "valence"            = Valence v
        | qud == "priceValence"       = PriceValence (s,v)
        | qud == "approxPrice"        = Price $ approx s
        | qud == "approxPriceValence" = PriceValence (approx s,v)
    answer qud (Price s)
        | qud == "price"              = Price s
        | qud == "valence"            = Valence False
        | qud == "priceValence"       = PriceValence (s, False)
        | qud == "approxPrice"        = Price $ approx s
        | qud == "approxPriceValence" = PriceValence (approx s, False)

approx :: Int -> Int
approx x = 10 * (round $ (fromIntegral x)/10)
