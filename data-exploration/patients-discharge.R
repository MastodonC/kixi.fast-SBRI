require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
source("paths.R")

remove_first_and_last <- function(dataframe) {
  head(dataframe[-1,], -1) # Modify to remove the first/last week or month
}

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

patients_discharge <- group_by(patient.data, H.C.Encrypted, age.num, age.group, Sex, Year.of.Discharge,
                               Month.of.Discharge, Week.of.Discharge, Date.of.Discharge, Method.of.Discharge) 
# %>%
#                       filter(Method.of.Discharge %in% c("Normal Discharge", "Self/Relative Disch.", "Transfer-Other Hosp",  
#                                                         "Internal Discharge", "Nurse Led Discharge", "Nurse Transfer-O H",
#                                                         "Nurse Internal Disc"))

### Exploring patients discharge data
## Yearly
patients_yearly_discharge <- group_by(patients_discharge_last_episode, Year.of.Discharge, Method.of.Discharge) %>%
                             summarise(count = n())

ggplot(data=patients_yearly_discharge, aes(x=Year.of.Discharge, y=count)) + geom_col(aes(fill=Method.of.Discharge))
                             
## Monthly
patients_monthly_discharge <- patients_discharge_last_episode %>%
                              group_by(Year.of.Discharge, Month.of.Discharge, Method.of.Discharge) %>%
                              arrange(Year.of.Discharge, Month.of.Discharge) %>%
                              summarise(count = n())

ggplot(data=patients_monthly_discharge, aes(x=paste(Year.of.Discharge, Month.of.Discharge, sep="-"), y=count, 
                                            group=Method.of.Discharge)) + geom_line(aes(color=Method.of.Discharge)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
acf(patients_monthly_discharge$count)
pacf(patients_monthly_discharge$count)
plot.ts(patients_monthly_discharge$count)
acf(diff(patients_monthly_discharge$count))
plot.ts(diff(patients_monthly_discharge$count))

## Weekly
patients_weekly_discharge <- patients_discharge_last_episode %>%
                             group_by(Year.of.Discharge, Week.of.Discharge, Method.of.Discharge) %>%
                             arrange(Year.of.Discharge, Week.of.Discharge) %>%
                             summarise(count = n())

ggplot(data=patients_weekly_discharge, aes(x=paste(Year.of.Discharge, Week.of.Discharge, sep="-"), y=count, 
                                           group=Method.of.Discharge)) + geom_line(aes(color=Method.of.Discharge)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

acf(patients_weekly_discharge$count)
plot.ts(patients_weekly_discharge$count)
acf(diff(patients_weekly_discharge$count))
plot.ts(diff(patients_weekly_discharge$count))

## Daily
patients_daily_discharge <- patients_discharge_last_episode %>%
                            group_by(Date.of.Discharge, Method.of.Discharge) %>%
                            summarise(count = n())

ggplot(data=patients_daily_discharge, aes(x=Date.of.Discharge, y=count, group=Method.of.Discharge)) + geom_line(aes(color=Method.of.Discharge)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

acf(patients_daily_discharge$count)
plot.ts(patients_daily_discharge$count)
acf(diff(patients_daily_discharge$count))
plot.ts(diff(patients_daily_discharge$count))

### Modelling monthly discharge data
## Normal discharge
prediction.length <- 20

monthly_normal_disch <- filter(patients_monthly_discharge, Method.of.Discharge == "Normal Discharge")
monthly_normal_disch <- monthly_normal_disch[,c("Year.of.Discharge", "Month.of.Discharge", "count")]

monthly.normal.model <- arima(monthly_normal_disch$count, order = c(1,0,0))
monthly.normal.prediction <-predict(monthly.normal.model,n.ahead=prediction.length)
monthly.normal.admissions.pred <- monthly.normal.prediction$pred[1:prediction.length]
plot.ts(c(monthly_normal_disch$count,monthly.normal.admissions.pred))

### Modelling weekly discharge data
## Normal discharge
weekly_normal_disch <- filter(patients_weekly_discharge, Method.of.Discharge == "Normal Discharge")
weekly_normal_disch <- weekly_normal_disch[,c("Year.of.Discharge", "Week.of.Discharge", "count")]

weekly.normal.model <- arima(weekly_normal_disch$count, order = c(1,0,0))
weekly.normal.prediction <-predict(weekly.normal.model,n.ahead=prediction.length)
weekly.normal.admissions.pred <- weekly.normal.prediction$pred[1:prediction.length]
plot.ts(c(weekly_normal_disch$count, weekly.normal.admissions.pred))

# see if the last record of hospital stay is a discharge
# Mode.of.Exit.from.Ward == "DSC"
# (there are 130 patients who didn't leave, so that would be the expected number)
last.record.patient.data <- group_by(patient.data, H.C.Encrypted,DateTime.of.Admission) %>%
                            arrange(Ward.Episode.Number) %>%
                            filter(row_number()==n()) %>%
                            filter(Mode.of.Exit.from.Ward != "DSC")

## Look at specialities and resource pools
specialities_match <- read.csv(resource_pool.specialties.path, stringsAsFactors = F, 
                               na.strings=c("","NA"))

monthly_discharge_by_speciality <- patients_discharge_last_episode %>%
                                   group_by(Year.of.Discharge, Month.of.Discharge, 
                                            Specialty.on.Exit.of.Ward) %>%
                                   arrange(Year.of.Discharge, Month.of.Discharge) %>%
                                   summarise(count = n())

monthly_discharge_by_resource_pool <- merge(monthly_discharge_by_speciality, specialities_match,
                                            by.x = "Specialty.on.Exit.of.Ward",
                                            by.y = "PAS.name")

weekly_discharge_by_speciality <- patients_discharge_last_episode %>%
                                  group_by(Year.of.Discharge, Week.of.Discharge, 
                                  Specialty.on.Exit.of.Ward) %>%
                                  arrange(Year.of.Discharge, Week.of.Discharge) %>%
                                  summarise(count = n())

weekly_discharge_by_resource_pool <- merge(weekly_discharge_by_speciality, specialities_match,
                                           by.x = "Specialty.on.Exit.of.Ward",
                                           by.y = "PAS.name")
