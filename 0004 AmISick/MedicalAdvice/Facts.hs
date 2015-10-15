module MedicalAdvice.Facts where

import           Control.Monad              (join, replicateM)
import           Data.Time                  (Day, addDays, getCurrentTime,
                                             utctDay)
import           MedicalAdvice.Advice       (Advice, getRandomAdvice)
import           MedicalAdvice.Illnesses    (Illness, getRandomIllness)
import           MedicalAdvice.Lib
import           MedicalAdvice.Measurements (Measurement, getRandomMeasurement)
import           MedicalAdvice.Questions    (QuestionAnswer,
                                             getRandomQuestionAnswer)
import           System.Random              (randomRIO)

type Date = Day

data Fact = Fact Occurance FactType deriving Show

data Occurance = Occurance
               { fromDate :: Maybe Date
               , toDate   :: Maybe Date
               } deriving Show


data FactType = Measurement    Measurement
              | Diagnosis      Illness
              | Advice         Advice
              | QuestionAnswer QuestionAnswer
              deriving Show

getRandomOccurance :: IO Occurance
getRandomOccurance = do
    now       <- utctDay <$> getCurrentTime
    deltaFrom <- randomRIO (1,365*10)
    deltaTo   <- randomRIO (0,deltaFrom)
    fromD     <- pickRandom [Nothing, Just $ addDays (-deltaFrom) now]
    toD       <- pickRandom [Nothing, Just $ addDays (-deltaTo) now]
    return $ Occurance fromD toD

getRandomFact :: IO Fact
getRandomFact = do
        f <- join $ pickRandom [ Measurement    <$> getRandomMeasurement
                            , Diagnosis      <$> getRandomIllness
                            , Advice         <$> getRandomAdvice
                            , QuestionAnswer <$> getRandomQuestionAnswer
                            ]
        occ <- getRandomOccurance
        return $ Fact occ f

getRandomFacts :: IO [Fact]
getRandomFacts = do
        amnt <- randomRIO (30,50)
        replicateM amnt  getRandomFact
