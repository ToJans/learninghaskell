module MedicalAdvice.Illnesses where

import MedicalAdvice.Lib

data Illness = Illness String deriving Show

getRandomIllness :: IO Illness
getRandomIllness = pickRandom $ map Illness [ "cold"
                                            , "flue"
                                            , "rash"
                                            , "allergy"
                                            ]
