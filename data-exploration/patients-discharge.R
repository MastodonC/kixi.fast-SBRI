require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
source("paths.R")

patient.data <- read.csv(patient.data.path, stringsAsFactors = F, na.strings=c("","NA")) %>%
  mutate(
    Date.of.Discharge = as.Date(Date.of.Discharge.with.Time,format="%d-%b-%Y %H:%M"),
    Year.of.Discharge = format(Date.of.Discharge, "%Y"),
    Month.of.Discharge = format(Date.of.Discharge, "%m"),
    Week.of.Discharge = format(Date.of.Discharge, "%U"),
    Time.of.Discharge = strftime(as.POSIXct(Date.of.Discharge.with.Time,format="%d-%b-%Y %H:%M"), format="%H:%M"),
    Hour.of.Discharge = hour(hm(Time.of.Discharge)),
    age.num = as.integer(Age),
    Sex = as.factor(Sex),
    Method.of.Admission.Category = as.factor(Method.of.Admission.Category),
    age.group = as.character(cut(Age, breaks=c(-1,10,20,30,40,50,60,70,80,90,Inf),
                                 labels=c('0-10', '10-20', '20-30', '30-40', '40-50', '50-60', '60-70', '70-80', '80-90', 'Over 90')))
  )

patients_discharge <- group_by(patient.data, H.C.Encrypted, age.num, age.group, Sex, Year.of.Discharge,
                               Month.of.Discharge, Week.of.Discharge, Date.of.Discharge, Method.of.Discharge)

## Yearly
patients_yearly_discharge <- summarise(group_by(patients_discharge, Year.of.Discharge), count = n())
## Monthly
patients_monthly_discharge <- summarise(group_by(patients_discharge, Year.of.Discharge, Month.of.Discharge), count = n())
## Weekly
patients_weekly_discharge <- summarise(group_by(patients_discharge, Year.of.Discharge, Month.of.Discharge, Week.of.Discharge), count = n())
