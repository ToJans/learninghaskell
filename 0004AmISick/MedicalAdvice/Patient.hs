module MedicalAdvice.Patient where

import           MedicalAdvice.Facts

newtype PatientId = PatientId String

data Patient = Patient PatientId [Fact]
