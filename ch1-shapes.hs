#!/usr/bin/env runhaskell
import Probability
import RSA

main :: IO ()
main = do
    let objectSpace = uniform [ Object "blue" "square"
                              , Object "blue" "circle"
                              , Object "green" "square"
                              ]
    let utteranceSpace = uniform $ map (Cost 0) ["blue", "green", "square", "circle"]

    let alpha = 1
    let qud = ""

    putStrLn "Literal listener hears \"blue\":"
    putStrLn . printProbs $ literalListener objectSpace qud "blue"

    putStrLn "Pragmatic speaker sees \"blue circle\":"
    putStrLn . printProbs $ pragmaticSpeaker alpha objectSpace utteranceSpace qud (Object "blue" "circle")

    putStrLn "Pragmatic listener hears \"blue\":"
    putStrLn . printProbs $ pragmaticListener alpha objectSpace utteranceSpace [Prob 1 qud] "blue"



data Object = Object { color :: String, shape :: String } deriving (Read, Eq)

instance Show Object where
    show (Object c s) = c ++ " " ++ s

instance Describable Object where
    meaning obj utt = (utterance utt == color obj) || (utterance utt == shape obj)

instance Answerable Object where
    answer _ = id

