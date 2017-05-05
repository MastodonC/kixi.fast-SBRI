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

# Last discharge for each stay = patient exit
patients_exit_hospital <- patient.data %>%
                          group_by(H.C.Encrypted, DateTime.of.Admission) %>%
                          arrange(Ward.Episode.Number) %>%
                          filter(row_number()==n() & is.na(Date.of.Discharge)==0)

# Try matching specialities -> all specialities match a resource pool =)
specialities_match <- read.csv(resource_pool.specialties.2.path, stringsAsFactors = F, 
                               na.strings=c("","NA"))

exits_per_speciality <- merge(patients_exit_hospital, specialities_match, 
                              by.x="Specialty.on.Exit.of.Ward", by.y="PAS.name")

# For the exit types I don't expect internal transfers, but still there are some
exit_types <- unique(patients_exit_hospital$Method.of.Discharge)

exits_transfers <- filter(patients_exit_hospital, Method.of.Discharge %in% c("Nurse Internal Disc",
                                                                             "Internal Discharge"))
# those are 821 records out of 67,659 records and they have a Ward.Episode.Number==1!
patient1 <- filter(patients_exit_hospital, H.C.Encrypted == "15FGJQR8CH") # patient actually sent home
patient2 <- filter(patients_exit_hospital, H.C.Encrypted == "15WB9QA4OH")

# Does `Method.of.Discharge` generally match `Destination.Discharge.Description`?
disch_method_and_destination <- patients_exit_hospital[,c("H.C.Encrypted", "Method.of.Discharge", 
                                                          "Destination.Discharge.Description",
                                                          "DateTime.of.Admission", "Date.of.Discharge.with.Time")]
# 14 different types of destinations after discharge
disch_destinations <- unique(disch_method_and_destination$Destination.Discharge.Description)

# try to filter out the "Normal Discharge" group
normal_disch <- filter(patients_exit_hospital, Method.of.Discharge %in% c("Normal Discharge", 
                                                                          "Nurse Led Discharge",
                                                                          "Self/Relative Disch.")
                       | Destination.Discharge.Description %in% c("HOME / USUAL ADDRESS", "NURSING HOME",
                                                                  "RELATIVE'S HOME"))

# Add exit group for all records
patients_exit_hospital$discharge_group <- case_when(patients_exit_hospital$Method.of.Discharge %in% c("Normal Discharge", 
                                                                             "Nurse Led Discharge",
                                                                             "Self/Relative Disch.")
                                                    | patients_exit_hospital$Destination.Discharge.Description %in% c("HOME / USUAL ADDRESS", 
                                                                                               "NURSING HOME",
                                                                                               "NURSING HOME        ",
                                                                                               "RELATIVE'S HOME",
                                                                                               "PRIVATE RESID HOME",
                                                                                               "RESIDENTIAL HOME",
                                                                                               "STATUTORY RESID HOME")
                                                    ~ "Normal Discharge",
                                                    
                                                    patients_exit_hospital$Method.of.Discharge %in% c("Transfer-Other Hosp", "Nurse Transfer-O H")
                                                    | patients_exit_hospital$Destination.Discharge.Description %in% c("GREAT BRITAIN HOSP", 
                                                                                               "NHS HOSPITAL-GENERAL",
                                                                                               "NON-UK HOSPITAL",
                                                                                               "NHS HOSP- MENTAL ILL",
                                                                                               "NHS HOSP. -MATERNITY")
                                                    ~ "External Transfer",
                                                    
                                                    patients_exit_hospital$Method.of.Discharge %in% c("Died-Post Mortem", "Died-No Post Mortem", 
                                                                                                      "Stillbirth")
                                                    ~ "Deceased"
  
)

# what records have `discharge_group`==NA
disch_na <- filter(patients_exit_hospital, is.na(discharge_group))

## Data Exploration
## Yearly
patients_yearly_exit <- group_by(patients_exit_hospital, Year.of.Discharge, discharge_group) %>%
                                 summarise(count = n())

ggplot(data=patients_yearly_exit, aes(x=Year.of.Discharge, y=count)) + geom_col(aes(fill=discharge_group))

## Monthly
patients_monthly_exit <- patients_exit_hospital %>%
                         group_by(Year.of.Discharge, Month.of.Discharge, discharge_group) %>%
                         arrange(Year.of.Discharge, Month.of.Discharge) %>%
                         summarise(count = n())

ggplot(data=patients_monthly_exit, aes(x=paste(Year.of.Discharge, Month.of.Discharge, sep="-"), y=count, 
                                            group=discharge_group)) + geom_line(aes(color=discharge_group)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
acf(patients_monthly_exit$count)
plot.ts(patients_monthly_exit$count)
acf(diff(patients_monthly_exit$count))
plot.ts(diff(patients_monthly_exit$count))
# -> seasonality? 3 months cycle?

## Weekly
patients_weekly_exit <- patients_exit_hospital %>%
                        group_by(Year.of.Discharge, Week.of.Discharge, discharge_group) %>%
                        arrange(Year.of.Discharge, Week.of.Discharge) %>%
                        summarise(count = n())

ggplot(data=patients_weekly_exit, aes(x=paste(Year.of.Discharge, Week.of.Discharge, sep="-"), y=count, 
                                           group=discharge_group)) + geom_line(aes(color=discharge_group)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

acf(patients_weekly_exit$count)
plot.ts(patients_weekly_exit$count)
acf(diff(patients_weekly_exit$count))
plot.ts(diff(patients_weekly_exit$count))
# -> seasonality? 3 months cycle?