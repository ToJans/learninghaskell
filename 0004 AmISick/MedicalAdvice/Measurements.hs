module MedicalAdvice.Measurements where

import MedicalAdvice.Lib
import System.Random(randomRIO)
import Control.Monad(join)

data Measurement = Length Double
                 | Weight Double
                 | Temperature Double
                 | Pulse Int Int
                 deriving Show

getRandomMeasurement :: IO Measurement
getRandomMeasurement = join $ pickRandom  [ Length      <$> randomRIO (140,210)
                                          , Weight      <$> randomRIO (50,200)
                                          , Temperature <$> randomRIO (35,42)
                                          , Pulse       <$> randomRIO (10,20) <*> randomRIO (5,10)
                                          ]
