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


-- BC : Callcenter

data Call = IncomingCall Nurse
          | CustomerCall Customer
          | DiagnosedCall Nurse Patient (Maybe Advice)

-- AR : contactData
data ContactData = Contact Name String

data Nurse = Nurse ContactData

data Customer = Customer ContactData

data MedicalStaff = MedicalStaff ContactData

-- BC : Advice

-- AR : Patient
data Patient = Patient [MedicalFact]

-- VO : MedicalFact
data MedicalFact = MedicalFact Occurance MedicalFactType

data Occurance = Since Date
               | Between {fromDate::Date, toDate:: Date}
               | Before Date
               | On Date
               | Never
               | Once
               | AtLeastOnce

data MedicalFactType = Measurement    Measurement
                     | Diagnosis      Illness
                     | Advice         Advice
                     | QuestionAnswer Question Answer

data Measurement = Length Double
                 | Weight Double
                 | BodyTemperature Double
                 | PulseRate Double

data Illness = Illness Name

data Advice = Suggestion   String
            | SuggestVisit MedicalStaff
            | ContactStaff MedicalStaff

-- VO Question
data Question = Question String QuestionType

data QuestionType = OccuranceQuestion
                  | BoolQuestion
                  | ChoiceQuestion [String]
                  | NumberQuestion { minRange :: Double, maxRange :: Double}
                  | TextQuestion

-- VO answer
data Answer = OccuranceAnswer
            | BoolAnswer Bool
            | TextAnswer String
            | NumberAnswer Double

-- AR : Advisor
data Advisor = Advisor [AdviceRequirement] Advice

data AdviceRequirement = AdviceRequirement Occurance (MedicalFact -> Bool)
