module MedicalAdvice.Lib where

import System.Random(randomRIO)

pickRandom :: [a] -> IO a
pickRandom l = do
  idx <- randomRIO (0,length l - 1)
  return $ l !! idx
