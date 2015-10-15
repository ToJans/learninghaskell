module CallCenter.Main where


type Name = String

data Call = IncomingCall Nurse
          | CustomerCall Customer
          | DiagnosedCall Nurse Patient
          | StaffCall Nurse Patient MedicalStaff

-- AR : contactData
data ContactData = Contact Name String

data Nurse = Nurse ContactData

data Customer = Customer ContactData

data MedicalStaff = MedicalStaff ContactData

data Patient = Patient ContactData
