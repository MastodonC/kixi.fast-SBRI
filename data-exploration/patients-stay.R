require(dplyr)
require(reshape2)
require(ggplot2)


readPatientsRecords <- function(filePath){
  patientsData <- read.csv(filePath, stringsAsFactors = F,na.strings=c("","NA")) %>%
    mutate(
      Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d/%m/%Y"),
      Date.of.Discharge = as.Date(Date.of.Discharge.with.Time,format="%d/%m/%Y"),
      Age = as.integer(Age),
      Sex = as.factor(Sex),
      Method.of.Admission.Category = as.factor(Method.of.Admission.Category)
    )
  patientsWithIDs <- patientsData[ which(patientsData$H.C.Encrypted != "NA" & patientsData$Sex != "I"),]
  return(patientsWithIDs)
}


getPatientsStays <- function(patientsData){
  patientsStays <- patientsData[, c("H.C.Encrypted", "Age", "Sex", "Date.of.Admission", "Date.of.Discharge")]
  patientsStays$Length.of.stay <- difftime(patientsStays$Date.of.Discharge, patientsStays$Date.of.Admission, 
                                           units="days")
  return(patientsStays)
}


# Patients stays by sex 
gender.stays <- group_by(stays, Sex) %>% summarize(count = n())

# Patients stays by age 
age.stays <- group_by(stays, Age) %>% summarize(count = n())