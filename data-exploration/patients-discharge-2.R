require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
source("paths.R")

## This script will look at patients exiting the hospital at the end of a stay.
## Will create three groups: 1) normal discharge (going home), 2) external transfer
## (to another hospital), 3) palliative care or deceased patient.

patient.data <- read.csv(patient.data.path, stringsAsFactors = F, na.strings=c("","NA")) %>%
  mutate(
    Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    DateTime.of.Admission = as.POSIXct(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
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

last_patients_discharge <- patient.data %>%
  group_by(H.C.Encrypted, DateTime.of.Admission) %>%
  arrange(Ward.Episode.Number) %>%
  filter(row_number()==n() & is.na(Date.of.Discharge)==0)