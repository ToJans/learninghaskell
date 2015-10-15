module MedicalAdvice.Advice where

import MedicalAdvice.Lib

data Advice = Suggestion   String
            | SuggestMedicalStaffVisit
            | ContactMedicalStaff
            deriving Show


getRandomAdvice :: IO Advice
getRandomAdvice = pickRandom
              [ Suggestion "Eat more"
              , Suggestion "Eat less"
              , Suggestion "Exercise more"
              , Suggestion "Exercise less"
              , SuggestMedicalStaffVisit
              , ContactMedicalStaff
              ]
