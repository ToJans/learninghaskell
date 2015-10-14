-- taken from http://architecturalkatas.site44.com/kata.html?kata=AmISick.json
--
-- 1-800-AMI-SICK
-- Your company wants to build a software system supporting call center nurses
-- (advice nurse) answering questions from customers about potential health problems.
--
-- Projected users: 250+ nurses worldwide
--
-- Requirements: access patient medical histories; assist nurses in providing medical
-- diagnosis; enable client customers to reach local medical staff (if necessary),
--   contacting the local medical staff directly ahead of time (if necessary)
--
-- Later phase requirements: enable parts of the system for direct client customer use

module AmISick where

type Name = String

type Date = Double

data ContactData = ContactData String

data Nurse = Nurse Name ContactData

data Customer = Customer Name ContactData

data MedicalStaff = MedicalStaff Name ContactData

data Patient = Patient Name

data Symptom = Illness Name
             | Allergy Name
             | Rash
             | Moodswings

data Measurement = Length Double
                 | Weight Double
                 | BodyTemperature Double
                 | PulseRate Double

data Occurance = Since Date
               | Between {fromDate::Date, toDate:: Date}
               | Before Date
               | On Date
               | Never
               | Once
               | AtLeastOnce

data QuestionType = OccuranceQuestion
                  | BoolQuestion
                  | ChoiceQuestion [String]
                  | NumberQuestion { minRange :: Double, maxRange :: Double}
                  | TextQuestion

data Question = Question String QuestionType

data Answer = OccuranceAnswer
            | BoolAnswer Bool
            | TextAnswer String
            | NumberAnswer Double

data MedicalFactType = MeasureMent    Measurement
                     | Symptom        Symptom
                     | QuestionAnswer Question Answer

data MedicalFact = MedicalFact Occurance MedicalFactType

data MedicalHistory = MedicalHistory [MedicalFact]

data AdviceRequirement = AdviceRequirement Occurance (MedicalFact -> Bool)

data MedicalAdvice = Suggestion   String
                   | SuggestVisit MedicalStaff
                   | ConcatStaff  MedicalStaff

data PotentialAdvice = PotentialAdvice [AdviceRequirement] MedicalAdvice

data Call = IncomingCall Nurse
          | CustomerCall Customer
          | DiagnosedCall Nurse Patient (Maybe MedicalAdvice)
