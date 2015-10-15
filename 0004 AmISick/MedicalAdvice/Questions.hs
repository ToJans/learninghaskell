module MedicalAdvice.Questions where

import MedicalAdvice.Lib
import System.Random(randomRIO)

data Question = Question String QuestionType
                deriving Show

data QuestionType = OccuranceQuestion
                  | BoolQuestion
                  | ChoiceQuestion [String]
                  | NumberQuestion { minRange :: Int, maxRange :: Int}
                  | TextQuestion
                  deriving Show

data Answer = OccuranceAnswer
            | BoolAnswer Bool
            | TextAnswer String
            | NumberAnswer Int
            deriving Show

data QuestionAnswer  = QuestionAnswer Question Answer
                       deriving Show


questionType :: Question -> QuestionType
questionType (Question _ t) = t

getRandomQuestion :: IO Question
getRandomQuestion = pickRandom [ Question "When do you take your pills" OccuranceQuestion
                               , Question "Do you feel tired in the evening" BoolQuestion
                               , Question "What is your favorite color" $ ChoiceQuestion ["red","green","blue"]
                               , Question "How much beers do you drink per day" $ NumberQuestion 0 40
                               , Question "How do you feel about your partner" TextQuestion
                               ]


getRandomQuestionAnswer :: IO QuestionAnswer
getRandomQuestionAnswer = do
            q <- getRandomQuestion
            qa <- case questionType q of
              OccuranceQuestion        -> return OccuranceAnswer
              BoolQuestion             -> pickRandom $ map BoolAnswer [True,False]
              ChoiceQuestion choices   -> pickRandom $ map TextAnswer choices
              NumberQuestion minv maxv -> NumberAnswer <$> randomRIO (minv,maxv)
              TextQuestion             -> pickRandom $ map TextAnswer [ "Aaahw!!!! I feel good!"
                                                                      , "She's always a woman to me"
                                                                      , "I'm in love with an alien"
                                                                      , "Billy Jean is not my love"
                                                                      ]
            return $ QuestionAnswer q qa
