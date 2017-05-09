require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
source("paths.R")

## This script will look at patients exiting the hospital at the end of a stay.
## Will create three groups: 1) normal discharge (going home), 2) external transfer
## (to another hospital), 3) palliative care or deceased patient.

### Functions
calculate.weekly.disch.from.proportions <- function(weekly.data, proportions.data) {
  bind_rows(mutate(weekly.data, day = 1),
            mutate(weekly.data, day = 2),
            mutate(weekly.data, day = 3),
            mutate(weekly.data, day = 4),
            mutate(weekly.data, day = 5),
            mutate(weekly.data, day = 6),
            mutate(weekly.data, day = 7)) %>%
    arrange(Year.of.Discharge,Week.of.Discharge, day) %>%
    mutate(date = as.Date(paste(Year.of.Discharge, Week.of.Discharge, (day-1), sep="-"), 
                          format="%Y-%U-%w"))  %>%
    mutate(date = if_else(is.na(date),
                          as.Date(paste((as.integer(Year.of.Discharge)+1), 0, (day-1), sep="-"), 
                                  format="%Y-%U-%w"),
                          date))  %>% # remove the last days of the last week
    mutate(Discharge.weekdays = weekdays(date, abbreviate = FALSE)) %>%
    inner_join(proportions.data) %>%
    mutate(daily_count = weekly_count * proportions)
}

### Data
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

# For the exit types I don't expect internal transfers, but still there are some
exit_types <- unique(patients_exit_hospital$Method.of.Discharge)

exits_transfers <- filter(patients_exit_hospital, Method.of.Discharge %in% c("Nurse Internal Disc",
                                                                             "Internal Discharge"))

# Does `Method.of.Discharge` generally match `Destination.Discharge.Description`?
disch_method_and_destination <- patients_exit_hospital[,c("H.C.Encrypted", "Method.of.Discharge", 
                                                          "Destination.Discharge.Description",
                                                          "DateTime.of.Admission", "Date.of.Discharge.with.Time")]
# 14 different types of destinations after discharge
disch_destinations <- unique(disch_method_and_destination$Destination.Discharge.Description)

# Add a discharge group column
patients_exit_hospital$discharge_group <- case_when(patients_exit_hospital$Method.of.Discharge %in% c("Died-Post Mortem", "Died-No Post Mortem", 
                                                                                                      "Stillbirth")
                                                    | patients_exit_hospital$Specialty.on.Exit.of.Ward == "Palliative Medicine (C)"
                                                    ~ "Palliative/Deceased",
                                                    
                                                    patients_exit_hospital$Method.of.Discharge %in% c("Normal Discharge", 
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
                                                    ~ "External Transfer"
)

# what records have `discharge_group`==NA
disch_na <- filter(patients_exit_hospital, is.na(discharge_group)) 
# just 1 record I can't classify -> let's drop it

patients_exit_hospital <- filter(patients_exit_hospital, !is.na(discharge_group))

### Discharges per weekday
## Total discharges
total_discharges <- nrow(patients_exit_hospital)

weekday.list <- c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
patients_exit_hospital$Discharge.weekdays <- weekdays(patients_exit_hospital$Date.of.Discharge, 
                                                      abbreviate = FALSE)

discharge_per_weekday <- patients_exit_hospital %>%
                         group_by(Discharge.weekdays) %>%
                         summarize(weekday_count = n()) %>%
                         mutate(weekday.ordered = factor(Discharge.weekdays, levels = weekday.list)) %>%
                         arrange(weekday.ordered)

ggplot(data=discharge_per_weekday, aes(x=weekday.ordered, y=weekday_count)) + geom_col()

weekday_discharge_proportions <- discharge_per_weekday %>%
                                 mutate(proportions = weekday_count / total_discharges)

## Discharge groups
disch_grps_per_weekday <- patients_exit_hospital %>%
                          group_by(Discharge.weekdays, discharge_group) %>%
                          summarize(weekday_count = n()) %>%
                          mutate(weekday.ordered = factor(Discharge.weekdays, levels = weekday.list)) %>%
                          arrange(weekday.ordered)
ggplot(data=disch_grps_per_weekday, aes(x=weekday.ordered, y=weekday_count)) + geom_col(aes(fill=discharge_group))

## Resource pools
# Specialities data
specialities_match <- read.csv(resource_pool.specialties.2.path, stringsAsFactors = F, 
                               na.strings=c("","NA"))

exits_per_speciality <- merge(patients_exit_hospital, specialities_match, 
                              by.x="Specialty.on.Exit.of.Ward", by.y="PAS.name")

exit_spe_per_weekday <- exits_per_speciality %>%
                        group_by(Discharge.weekdays, Resource.Pool.name) %>%
                        summarize(weekday_count = n()) %>%
                        mutate(weekday.ordered = factor(Discharge.weekdays, levels = weekday.list)) %>%
                        arrange(weekday.ordered)

ggplot(data=exit_spe_per_weekday, aes(x=weekday.ordered, y=weekday_count)) + geom_col(aes(fill=Resource.Pool.name))

### Data Exploration + Arima Model
prediction.length <- 20
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

# Monthly Normal Discharge group
monthly_normal <- filter(patients_monthly_exit, discharge_group == "Normal Discharge")
# acf
acf(monthly_normal$count)
plot.ts(monthly_normal$count)
acf(diff(monthly_normal$count))
plot.ts(diff(monthly_normal$count))
monthly_normal <- monthly_normal[,c("Year.of.Discharge", "Month.of.Discharge", "count")]
# Monthly External Transfer Discharge group
monthly_transfer <- filter(patients_monthly_exit, discharge_group == "External Transfer")
# acf
acf(monthly_transfer$count)
plot.ts(monthly_transfer$count)
acf(diff(monthly_transfer$count))
plot.ts(diff(monthly_transfer$count))
monthly_transfer <- monthly_transfer[,c("Year.of.Discharge", "Month.of.Discharge", "count")]
# Monthly Deaths or Palliative Care Discharge group
monthly_palliative <- filter(patients_monthly_exit, discharge_group == "Palliative/Deceased")
# acf
acf(monthly_palliative$count)
plot.ts(monthly_palliative$count)
acf(diff(monthly_palliative$count))
plot.ts(diff(monthly_palliative$count))

## Weekly
# All discharge types
weekly_exits <- patients_exit_hospital %>%
                group_by(Year.of.Discharge, Week.of.Discharge) %>%
                arrange(Year.of.Discharge, Week.of.Discharge) %>%
                summarise(weekly_count = n())

ggplot(data=weekly_exits, aes(x=paste(Year.of.Discharge, Week.of.Discharge, sep="-"), y=weekly_count, group=0)) + geom_line() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# acf
acf(weekly_exits$weekly_count)
plot.ts(weekly_exits$weekly_count)
acf(diff(weekly_exits$weekly_count))
plot.ts(diff(weekly_exits$weekly_count))
# arima
weekly_exits_model <- arima(weekly_exits$weekly_count, order = c(1,0,0))
weekly_exits_prediction <-predict(weekly_exits_model,n.ahead=prediction.length)
weekly_exits_pred <- weekly_exits_prediction$pred[1:prediction.length]
plot.ts(c(weekly_exits$weekly_count, weekly_exits_pred))
# create predictions df
last.year <- max(weekly_exits$Year.of.Discharge)
last.week <- as.integer(last(weekly_exits$Week.of.Discharge))
disch_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                               Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                               weekly_count = weekly_exits_pred),
                    weekly_count = as.integer(weekly_count),
                    Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                    Year.of.Discharge = as.character(Year.of.Discharge))
# using proportions to calculate counts for days of week
discharges_days <- calculate.weekly.disch.from.proportions(disch_pred, 
                                                           weekday_discharge_proportions)
disch_per_day <- discharges_days[,c("Year.of.Discharge", "Week.of.Discharge", 
                                    "date", "daily_count")]
# join historical and predictions df
daily_hist_disch <- patients_exit_hospital %>%
                    group_by(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                    arrange(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                    summarise(daily_count = n()) %>%
                    rename(date = Date.of.Discharge)
last_discharge_pred <- bind_rows(daily_hist_disch,disch_per_day)
ggplot(data=last_discharge_pred, aes(x=date, y=daily_count, group=0)) + geom_line() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# Looking at discharge groups
patients_weekly_exit <- patients_exit_hospital %>%
                        group_by(Year.of.Discharge, Week.of.Discharge, discharge_group) %>%
                        arrange(Year.of.Discharge, Week.of.Discharge) %>%
                        summarise(weekly_count = n())

ggplot(data=patients_weekly_exit, aes(x=paste(Year.of.Discharge, Week.of.Discharge, sep="-"), y=count, 
                                           group=discharge_group)) + geom_line(aes(color=discharge_group)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Weekly Normal Discharge group
weekly_normal <- filter(patients_weekly_exit, discharge_group == "Normal Discharge")
# acf
acf(weekly_normal$count)
plot.ts(weekly_normal$count)
acf(diff(weekly_normal$count))
plot.ts(diff(weekly_normal$count))
weekly_normal <- weekly_normal[,c("Year.of.Discharge", "Week.of.Discharge", "weekly_count")]
# arima
weekly.normal.model <- arima(weekly_normal$weekly_count, order = c(1,0,0))
weekly.normal.prediction <-predict(weekly.normal.model,n.ahead=prediction.length)
weekly.normal.admissions.pred <- weekly.normal.prediction$pred[1:prediction.length]
plot.ts(c(weekly_normal$weekly_count, weekly.normal.admissions.pred))
# create df
disch_normal_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                               Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                               weekly_count = weekly.normal.admissions.pred),
                    weekly_count = as.integer(weekly_count),
                    Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                    Year.of.Discharge = as.character(Year.of.Discharge))
# using proportions to calculate counts for days of week
normal_discharges_days <- calculate.weekly.disch.from.proportions(disch_normal_pred, 
                                                                  weekday_discharge_proportions)
normal_disch_per_day <- normal_discharges_days[,c("Year.of.Discharge", "Week.of.Discharge", 
                                        "date", "daily_count")]
# join historical and predictions df
daily_hist_normal_disch <- patients_exit_hospital %>%
                    filter(discharge_group == "Normal Discharge") %>%
                    group_by(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                    arrange(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                    summarise(daily_count = n()) %>%
                    rename(date = Date.of.Discharge)
last_normal_discharge_pred <- bind_rows(daily_hist_normal_disch,
                                        normal_disch_per_day)
ggplot(data=last_normal_discharge_pred, aes(x=date, y=daily_count, group=0)) + geom_line() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Weekly External Transfer Discharge group
weekly_transfer <- filter(patients_weekly_exit, discharge_group == "External Transfer")
# acf
acf(weekly_transfer$count)
plot.ts(weekly_transfer$count)
acf(diff(weekly_transfer$count))
plot.ts(diff(weekly_transfer$count))
weekly_transfer <- weekly_transfer[,c("Year.of.Discharge", "Week.of.Discharge", "weekly_count")]
# arima
weekly.transfer.model <- arima(weekly_transfer$weekly_count, order = c(1,0,0))
weekly.transfer.prediction <-predict(weekly.transfer.model,n.ahead=prediction.length)
weekly.transfer.admissions.pred <- weekly.transfer.prediction$pred[1:prediction.length]
plot.ts(c(weekly_transfer$weekly_count, weekly.transfer.admissions.pred))
# create df
disch_transfer_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                      Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                      weekly_count = weekly.transfer.admissions.pred),
                             weekly_count = as.integer(weekly_count),
                             Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                             Year.of.Discharge = as.character(Year.of.Discharge))
# using proportions to calculate counts for days of week
transfer_discharges_days <- calculate.weekly.disch.from.proportions(disch_transfer_pred, 
                                                                    weekday_discharge_proportions)
transfer_disch_per_day <- transfer_discharges_days[,c("Year.of.Discharge", "Week.of.Discharge", 
                                                   "date", "daily_count")]
# join historical and predictions df
daily_hist_transfer_disch <- patients_exit_hospital %>%
                             filter(discharge_group == "External Transfer") %>%
                             group_by(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                             arrange(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                             summarise(daily_count = n()) %>%
                             rename(date = Date.of.Discharge)
last_transfer_discharge_pred <- bind_rows(daily_hist_transfer_disch,transfer_disch_per_day)
ggplot(data=last_transfer_discharge_pred, aes(x=date, y=daily_count, group=0)) + geom_line() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Weekly Deaths or Palliative Care Discharge group
weekly_palliative <- filter(patients_weekly_exit, discharge_group == "Palliative/Deceased")
# acf
acf(weekly_palliative$count)
plot.ts(weekly_palliative$count)
acf(diff(weekly_palliative$count))
plot.ts(diff(weekly_palliative$count))
weekly_palliative <- weekly_palliative[,c("Year.of.Discharge", "Week.of.Discharge", "weekly_count")]
# arima
weekly.palliative.model <- arima(weekly_palliative$weekly_count, order = c(1,0,0))
weekly.palliative.prediction <-predict(weekly.palliative.model,n.ahead=prediction.length)
weekly.palliative.admissions.pred <- weekly.palliative.prediction$pred[1:prediction.length]
plot.ts(c(weekly_palliative$weekly_count, weekly.palliative.admissions.pred))
# create df
disch_palliative_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                      Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                      weekly_count = weekly.palliative.admissions.pred),
                                weekly_count = as.integer(weekly_count),
                                Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                                Year.of.Discharge = as.character(Year.of.Discharge))
# using proportions to calculate counts for days of week
palliative_discharges_days <- calculate.weekly.disch.from.proportions(disch_palliative_pred, 
                                                                      weekday_discharge_proportions)
palliative_disch_per_day <- palliative_discharges_days[,c("Year.of.Discharge", "Week.of.Discharge", 
                                                       "date", "daily_count")]
# join historical and predictions df
daily_hist_palliative_disch <- patients_exit_hospital %>%
                               filter(discharge_group == "Palliative/Deceased") %>%
                               group_by(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                               arrange(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                               summarise(daily_count = n()) %>%
                               rename(date = Date.of.Discharge)
last_palliative_discharge_pred <- bind_rows(daily_hist_palliative_disch,
                                            palliative_disch_per_day)
ggplot(data=last_palliative_discharge_pred, aes(x=date, y=daily_count, group=0)) + geom_line() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

### Data reconciliation to match totals
## Join all groups of discharge and total discharges together in one df
all_discharges_pred <- rename(last_discharge_pred, all_discharges = daily_count) %>%
                       full_join(rename(last_normal_discharge_pred, normal_discharge = daily_count), 
                                 by=c("Year.of.Discharge", "Week.of.Discharge", "date")) %>%
                       full_join(rename(last_transfer_discharge_pred, external_transfer = daily_count), 
                                 by=c("Year.of.Discharge", "Week.of.Discharge", "date")) %>%
                       full_join(rename(last_palliative_discharge_pred, palliative_deceased = daily_count), 
                                 by=c("Year.of.Discharge", "Week.of.Discharge", "date"))

all_discharges_pred[is.na(all_discharges_pred)] <- 0 # Replace NAs by "0"

all_discharges_pred$adjustement_factor <- all_discharges_pred$all_discharges / (all_discharges_pred$normal_discharge + all_discharges_pred$external_transfer + all_discharges_pred$palliative_deceased)

