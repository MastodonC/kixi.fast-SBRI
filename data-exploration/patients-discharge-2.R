require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
require(forecast)
source("paths.R")

## This script will look at patients exiting the hospital at the end of a stay.
## Will create three groups: 1) normal discharge (going home), 2) external transfer
## (to another hospital), 3) palliative care or deceased patient.

### Functions
remove_first_and_last <- function(dataframe) {
  head(dataframe[-1,], -1) # remove first and last row
}

add_column_with_value_to <- function(dataframe, colname, value) {
  column <- rep(value, nrow(dataframe))
  dataframe[ , colname] <- column
  dataframe
}

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


calculate.resource_pool.count.from.proportions <- function(daily.data, proportions.data) {
  bind_rows(mutate(daily.data, Resource.Pool.name = "Elderly Care"),
            mutate(daily.data, Resource.Pool.name = "Medical"),
            mutate(daily.data, Resource.Pool.name = "Palliative Care"),
            mutate(daily.data, Resource.Pool.name = "Surgical"),
            mutate(daily.data, Resource.Pool.name = "Unscheduled Care"),
            mutate(daily.data, Resource.Pool.name = "Women and Child")) %>%
    inner_join(proportions.data) %>%
    mutate(count = daily_count * proportions)
}

### FUNCTION TO BREAK DOWN RESOURCE POOLS INTO WARDS
# To do: Update the 'mutate' with Ward names and test
calculate.ward.count.from.proportions <- function(data_per_resource, proportions_data) {
  bind_rows(mutate(data_per_resource, Ward.Name = "Observation Unit - Antrim"),
            mutate(data_per_resource, Ward.Name = "Antrim A&E Dept."),
            mutate(data_per_resource, Ward.Name = "Discharge Lounge Antrim"),
            mutate(data_per_resource, Ward.Name = "C4 Elective Unit"),
            mutate(data_per_resource, Ward.Name = "Assessment Medical Unit 1"),
            mutate(data_per_resource, Ward.Name = "C6 Gen Surgery"),
            mutate(data_per_resource, Ward.Name = "Antrim (C)Intensive Care"),
            mutate(data_per_resource, Ward.Name = "A2 Paediatric Ward"),
            mutate(data_per_resource, Ward.Name = "A1 Stroke/Medical Ward"),
            mutate(data_per_resource, Ward.Name = "C3 Gastro & General Medicine"),
            mutate(data_per_resource, Ward.Name = "B4 - General Medicine"),
            mutate(data_per_resource, Ward.Name = "Genm/Endo/Diab - B2"),
            mutate(data_per_resource, Ward.Name = "B5b -Use Ward Code Eau For B5b"),
            mutate(data_per_resource, Ward.Name = "Antrim A4 Medical"),
            mutate(data_per_resource, Ward.Name = "C7"),
            mutate(data_per_resource, Ward.Name = "Antrim C1 Gynae"),
            mutate(data_per_resource, Ward.Name = "C5 Gen Surgery"),
            mutate(data_per_resource, Ward.Name = "Antrim B3 Ward"),
            mutate(data_per_resource, Ward.Name = "A3 Medical Ward" ),
            mutate(data_per_resource, Ward.Name = "Antrim Level B Cardiac Unit"),
            mutate(data_per_resource, Ward.Name = "Eldery Acute Unit"),
            mutate(data_per_resource, Ward.Name = "Antrim C2 Maternity Unit"),
            mutate(data_per_resource, Ward.Name = "Macmillan Unit At Antrim"),
            mutate(data_per_resource, Ward.Name = "C2 Cotted Ward")
  ) %>%
    inner_join(proportions_data) %>%
    mutate(ward_count = count * ward_proportion)
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
# just 1 record I can't classify -> let's drop it:
patients_exit_hospital <- filter(patients_exit_hospital, !is.na(discharge_group)) 

# => ALL DISCHARGES: *patients_exit_hospital*
# => DISCHARGES TYPES: *norm_disch*, *ext_disch*, *pall_disch*
norm_disch <- filter(patients_exit_hospital, discharge_group == "Normal Discharge")
ext_disch <- filter(patients_exit_hospital, discharge_group == "External Transfer")
pall_disch <- filter(patients_exit_hospital, discharge_group == "Palliative/Deceased")

### Discharges per weekday
## Weekdays - Total discharges
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

# All discharges per ward per weekday
discharge_ward_wkday <- patients_exit_hospital %>%
                        group_by(Discharge.weekdays, Ward.Name) %>%
                        summarize(weekday_count = n()) %>%
                        mutate(weekday.ordered = factor(Discharge.weekdays, levels = weekday.list)) %>%
                        arrange(weekday.ordered)

ggplot(discharge_ward_wkday, aes(x = weekday.ordered, y = weekday_count, fill = Ward.Name, label = weekday_count)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

# => ALL DISCHARGES WEEKDAY PROPORTIONS: *weekday_discharge_proportions*

## Weekdays - Discharge groups
disch_grps_per_weekday <- patients_exit_hospital %>%
                          group_by(Discharge.weekdays, discharge_group) %>%
                          summarize(weekday_count = n()) %>%
                          mutate(weekday.ordered = factor(Discharge.weekdays, levels = weekday.list)) %>%
                          arrange(weekday.ordered)
ggplot(data=disch_grps_per_weekday, aes(x=weekday.ordered, y=weekday_count)) + geom_col(aes(fill=discharge_group))

# Weekdays - Normal Discharges
total_norm_disch <- nrow(norm_disch)
norm_disch_per_weekday <- disch_grps_per_weekday %>%
                          filter(discharge_group == "Normal Discharge")
ggplot(data=norm_disch_per_weekday, aes(x=weekday.ordered, y=weekday_count)) + geom_col()
weekday_norm_disch_proportions <- norm_disch_per_weekday %>%
                                  mutate(proportions = weekday_count / total_norm_disch)
# Weekdays - External Transfers
total_ext_disch <- nrow(ext_disch)
ext_disch_per_weekday <- disch_grps_per_weekday %>%
                          filter(discharge_group == "External Transfer")
ggplot(data=ext_disch_per_weekday, aes(x=weekday.ordered, y=weekday_count)) + geom_col()
weekday_ext_disch_proportions <- ext_disch_per_weekday %>%
                                 mutate(proportions = weekday_count / total_ext_disch)
# Weekdays - Palliative+Deaths
total_pall_disch <- nrow(pall_disch)
pall_disch_per_weekday <- disch_grps_per_weekday %>%
                          filter(discharge_group == "Palliative/Deceased")
ggplot(data=pall_disch_per_weekday, aes(x=weekday.ordered, y=weekday_count)) + geom_col()
weekday_pall_disch_proportions <-pall_disch_per_weekday %>%
                                 mutate(proportions = weekday_count / total_pall_disch)

## Resource pools
# Specialities data
specialities_match <- read.csv(resource_pool.specialties.2.path, stringsAsFactors = F, 
                               na.strings=c("","NA"))

exits_per_speciality <- merge(patients_exit_hospital, specialities_match, 
                              by.x="Specialty.on.Exit.of.Ward", by.y="PAS.name")

# => DISCHARGE DATA WITH SPECIALITIES: *exits_per_speciality*

exit_spe_per_weekday <- exits_per_speciality %>%
                        group_by(Discharge.weekdays, Resource.Pool.name) %>%
                        summarize(weekday_count = n()) %>%
                        mutate(weekday.ordered = factor(Discharge.weekdays, levels = weekday.list)) %>%
                        arrange(weekday.ordered)

ggplot(exit_spe_per_weekday, aes(x = weekday.ordered, y = weekday_count, fill = Resource.Pool.name, label = weekday_count)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

# Discharges per resource/ward
exits_per_spe_for_ward <- filter(exits_per_speciality, !Ward.Name %in% ward.ignore)
# Medical
dish_resource_medical <- filter(exits_per_spe_for_ward, Resource.Pool.name == "Medical")
disch_medical_proportions <- dish_resource_medical %>%
                             group_by(Ward.Name) %>%
                             summarize(ward_count = n()) %>%
                             mutate(ward_proportion = ward_count / nrow(dish_resource_medical)) %>%
                             select(Ward.Name, ward_proportion)
# Surgical
dish_resource_surgical <- filter(exits_per_spe_for_ward, Resource.Pool.name == "Surgical")
disch_surgical_proportions <- dish_resource_surgical %>%
                              group_by(Ward.Name) %>%
                              summarize(ward_count = n()) %>%
                              mutate(ward_proportion = ward_count / nrow(dish_resource_surgical)) %>%
  select(Ward.Name, ward_proportion)
# Women and child
dish_resource_wac <- filter(exits_per_spe_for_ward, Resource.Pool.name == "Women and Child")
disch_wac_proportions <- dish_resource_wac %>%
                         group_by(Ward.Name) %>%
                         summarize(ward_count = n()) %>%
                         mutate(ward_proportion = ward_count / nrow(dish_resource_wac)) %>%
                         select(Ward.Name, ward_proportion)
# Elderly
dish_resource_elderly <- filter(exits_per_spe_for_ward, Resource.Pool.name == "Elderly Care")
disch_elderly_proportions <- dish_resource_elderly %>%
                             group_by(Ward.Name) %>%
                             summarize(ward_count = n()) %>%
                             mutate(ward_proportion = ward_count / nrow(dish_resource_elderly)) %>%
                             select(Ward.Name, ward_proportion)
# Unscheduled care
dish_resource_unsched <- filter(exits_per_spe_for_ward, Resource.Pool.name == "Unscheduled Care")
disch_unsched_proportions <- dish_resource_unsched %>%
                             group_by(Ward.Name) %>%
                             summarize(ward_count = n()) %>%
                             mutate(ward_proportion = ward_count / nrow(dish_resource_unsched)) %>%
                             select(Ward.Name, ward_proportion)
# Palliative care
dish_resource_palliative <- filter(exits_per_spe_for_ward, Resource.Pool.name == "Palliative Care")
disch_palliative_proportions <- dish_resource_palliative %>%
                                group_by(Ward.Name) %>%
                                summarize(ward_count = n()) %>%
                                mutate(ward_proportion = ward_count / nrow(dish_resource_palliative)) %>%
                                select(Ward.Name, ward_proportion)
# Proportions of resource pools:
# => ALL DISCHARGES RESOURCE POOLS PROPORTIONS: *resource_discharge_proportions*
resource_discharge_proportions <- exits_per_speciality %>%
                                  group_by(Resource.Pool.name) %>%
                                  summarize(resource_count = n()) %>%
                                  mutate(proportions = resource_count / nrow(exits_per_speciality))

ggplot(data=resource_discharge_proportions, aes(x=Resource.Pool.name, y=resource_count)) + geom_col()

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
weekly_exits <- remove_first_and_last(weekly_exits)
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
# Add confidence interval (95%)
weekly_pred_with_ci <- forecast.Arima(weekly_exits_model, 20) %>%
                       as.data.frame()
colnames(weekly_pred_with_ci) <- c("weekly_count", "low_80", "high_80", "lower (95%)", "upper (95%)")
weekly_pred_with_ci <- weekly_pred_with_ci[,c("weekly_count", "lower (95%)", "upper (95%)")]

# create predictions df
last.year <- max(weekly_exits$Year.of.Discharge)
last.week <- as.integer(last(weekly_exits$Week.of.Discharge))
disch_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                               Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                               weekly_count = weekly_exits_pred),
                    weekly_count = weekly_count,
                    Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                    Year.of.Discharge = as.character(Year.of.Discharge))
disch_pred_ci <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                               Week.of.Discharge = (last.week+1):(last.week+prediction.length)),
                       Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                       Year.of.Discharge = as.character(Year.of.Discharge)) %>%
                bind_cols(weekly_pred_with_ci)
# Looking at discharge groups
patients_weekly_exit <- patients_exit_hospital %>%
                        group_by(Year.of.Discharge, Week.of.Discharge, discharge_group) %>%
                        arrange(Year.of.Discharge, Week.of.Discharge) %>%
                        summarise(weekly_count = n())

ggplot(data=patients_weekly_exit, aes(x=paste(Year.of.Discharge, Week.of.Discharge, sep="-"), y=count, 
                                           group=discharge_group)) + geom_line(aes(color=discharge_group)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Weekly Normal Discharge group
weekly_normal <- filter(patients_weekly_exit, discharge_group == "Normal Discharge")
weekly_normal <- remove_first_and_last(weekly_normal)
# acf
acf(weekly_normal$count)
plot.ts(weekly_normal$count)
acf(diff(weekly_normal$count))
plot.ts(diff(weekly_normal$count))
weekly_normal <- weekly_normal[,c("Year.of.Discharge", "Week.of.Discharge", "weekly_count")]
# arima
weekly.normal.model <- arima(weekly_normal$weekly_count, order = c(1,0,0))
weekly.normal.prediction <-predict(weekly.normal.model,n.ahead=prediction.length)
weekly.normal.discharges.pred <- weekly.normal.prediction$pred[1:prediction.length]
plot.ts(c(weekly_normal$weekly_count, weekly.normal.discharges.pred))
# Add confidence interval (95%)
weekly_normal_pred_with_ci <- forecast.Arima(weekly.normal.model, 20) %>%
                              as.data.frame()
colnames(weekly_normal_pred_with_ci) <- c("weekly_count", "low_80", "high_80", "lower (95%)", "upper (95%)")
weekly_normal_pred_with_ci <- weekly_normal_pred_with_ci[,c("weekly_count", "lower (95%)", "upper (95%)")]
# create df
disch_normal_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                               Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                               weekly_count = weekly.normal.discharges.pred),
                    weekly_count = weekly_count,
                    Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                    Year.of.Discharge = as.character(Year.of.Discharge))
disch_normal_pred_ci <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                              Week.of.Discharge = (last.week+1):(last.week+prediction.length)),
                              Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                              Year.of.Discharge = as.character(Year.of.Discharge)) %>%
                       bind_cols(weekly_normal_pred_with_ci)
# Weekly External Transfer Discharge group
weekly_transfer <- filter(patients_weekly_exit, discharge_group == "External Transfer")
weekly_transfer <- remove_first_and_last(weekly_transfer)
# acf
acf(weekly_transfer$count)
plot.ts(weekly_transfer$count)
acf(diff(weekly_transfer$count))
plot.ts(diff(weekly_transfer$count))
weekly_transfer <- weekly_transfer[,c("Year.of.Discharge", "Week.of.Discharge", "weekly_count")]
# arima
weekly.transfer.model <- arima(weekly_transfer$weekly_count, order = c(1,0,0))
weekly.transfer.prediction <-predict(weekly.transfer.model,n.ahead=prediction.length)
weekly.transfer.discharges.pred <- weekly.transfer.prediction$pred[1:prediction.length]
plot.ts(c(weekly_transfer$weekly_count, weekly.transfer.discharges.pred))
# Add confidence interval (95%)
weekly_transfer_pred_with_ci <- forecast.Arima(weekly.transfer.model, 20) %>%
                                as.data.frame()
colnames(weekly_transfer_pred_with_ci) <- c("weekly_count", "low_80", "high_80", "lower (95%)", "upper (95%)")
weekly_transfer_pred_with_ci <- weekly_transfer_pred_with_ci[,c("weekly_count", "lower (95%)", "upper (95%)")]
# create df
disch_transfer_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                      Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                      weekly_count = weekly.transfer.discharges.pred),
                             weekly_count = weekly_count,
                             Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                             Year.of.Discharge = as.character(Year.of.Discharge))
disch_transfer_pred_ci <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                  Week.of.Discharge = (last.week+1):(last.week+prediction.length)),
                       Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                       Year.of.Discharge = as.character(Year.of.Discharge)) %>%
                       bind_cols(weekly_transfer_pred_with_ci)
# Weekly Deaths or Palliative Care Discharge group
weekly_palliative <- filter(patients_weekly_exit, discharge_group == "Palliative/Deceased")
weekly_palliative <- remove_first_and_last(weekly_palliative)
# acf
acf(weekly_palliative$count)
plot.ts(weekly_palliative$count)
acf(diff(weekly_palliative$count))
plot.ts(diff(weekly_palliative$count))
weekly_palliative <- weekly_palliative[,c("Year.of.Discharge", "Week.of.Discharge", "weekly_count")]
# arima
weekly.palliative.model <- arima(weekly_palliative$weekly_count, order = c(1,0,0))
weekly.palliative.prediction <-predict(weekly.palliative.model,n.ahead=prediction.length)
weekly.palliative.discharges.pred <- weekly.palliative.prediction$pred[1:prediction.length]
plot.ts(c(weekly_palliative$weekly_count, weekly.palliative.discharges.pred))
# Add confidence interval (95%)
weekly_palliative_pred_with_ci <- forecast.Arima(weekly.palliative.model, 20) %>%
                                  as.data.frame()
colnames(weekly_palliative_pred_with_ci) <- c("weekly_count", "low_80", "high_80", "lower (95%)", "upper (95%)")
weekly_palliative_pred_with_ci <- weekly_palliative_pred_with_ci[,c("weekly_count", "lower (95%)", "upper (95%)")]
# create df
disch_palliative_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                      Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                      weekly_count = weekly.palliative.discharges.pred),
                                weekly_count = weekly_count,
                                Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                                Year.of.Discharge = as.character(Year.of.Discharge))
disch_palliative_pred_ci <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                             Week.of.Discharge = (last.week+1):(last.week+prediction.length)),
                                  Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                                  Year.of.Discharge = as.character(Year.of.Discharge)) %>%
                           bind_cols(weekly_palliative_pred_with_ci)

## Weekly predictions per resource pool
patients_weekly_resources <- exits_per_speciality %>%
                             group_by(Year.of.Discharge, Week.of.Discharge, Resource.Pool.name) %>%
                             arrange(Year.of.Discharge, Week.of.Discharge) %>%
                             summarise(count = n())
# Medical
weekly_medic_pool <- filter(patients_weekly_resources, Resource.Pool.name == "Medical")
weekly_medic_pool <- remove_first_and_last(weekly_medic_pool)
# acf
acf(weekly_medic_pool$count)
plot.ts(weekly_medic_pool$count)
acf(diff(weekly_medic_pool$count))
plot.ts(diff(weekly_medic_pool$count))
weekly_medic_pool <- weekly_medic_pool[,c("Year.of.Discharge", "Week.of.Discharge", "count")]
# arima
weekly_medic_pool_model <- arima(weekly_medic_pool$count, order = c(1,1,0))
weekly_medic_pool_prediction <-predict(weekly_medic_pool_model,n.ahead=prediction.length)
weekly_medic_pool_discharges_pred <- weekly_medic_pool_prediction$pred[1:prediction.length]
plot.ts(c(weekly_medic_pool$count, weekly_medic_pool_discharges_pred))
# create df
disch_medic_pool_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                          Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                          weekly_count = weekly_medic_pool_discharges_pred),
                               weekly_count = weekly_count,
                               Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                               Year.of.Discharge = as.character(Year.of.Discharge))
# Surgical
weekly_surgic_pool <- filter(patients_weekly_resources, Resource.Pool.name == "Surgical")
weekly_surgic_pool <- remove_first_and_last(weekly_surgic_pool)
# acf
acf(weekly_surgic_pool$count)
plot.ts(weekly_surgic_pool$count)
acf(diff(weekly_surgic_pool$count))
plot.ts(diff(weekly_surgic_pool$count))
weekly_surgic_pool <- weekly_surgic_pool[,c("Year.of.Discharge", "Week.of.Discharge", "count")]
# arima
weekly_surgic_pool_model <- arima(weekly_surgic_pool$count, order = c(1,0,0))
weekly_surgic_pool_prediction <-predict(weekly_surgic_pool_model,n.ahead=prediction.length)
weekly_surgic_pool_discharges_pred <- weekly_surgic_pool_prediction$pred[1:prediction.length]
plot.ts(c(weekly_surgic_pool$count, weekly_surgic_pool_discharges_pred))
# create df
disch_surgic_pool_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                          Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                          weekly_count = weekly_surgic_pool_discharges_pred),
                               weekly_count = weekly_count,
                               Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                               Year.of.Discharge = as.character(Year.of.Discharge))
# Women and Child
weekly_wac_pool <- filter(patients_weekly_resources, Resource.Pool.name == "Women and Child")
weekly_wac_pool <- remove_first_and_last(weekly_wac_pool)
# acf
acf(weekly_wac_pool$count)
plot.ts(weekly_wac_pool$count)
acf(diff(weekly_wac_pool$count))
plot.ts(diff(weekly_wac_pool$count))
weekly_wac_pool <- weekly_wac_pool[,c("Year.of.Discharge", "Week.of.Discharge", "count")]
# arima
weekly_wac_pool_model <- arima(weekly_wac_pool$count, order = c(1,0,0))
weekly_wac_pool_prediction <-predict(weekly_wac_pool_model,n.ahead=prediction.length)
weekly_wac_pool_discharges_pred <- weekly_wac_pool_prediction$pred[1:prediction.length]
plot.ts(c(weekly_wac_pool$count, weekly_wac_pool_discharges_pred))
# create df
disch_wac_pool_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                           Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                           weekly_count = weekly_wac_pool_discharges_pred),
                                weekly_count = weekly_count,
                                Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                                Year.of.Discharge = as.character(Year.of.Discharge))
# Elderly Care
weekly_elder_pool <- filter(patients_weekly_resources, Resource.Pool.name == "Elderly Care")
weekly_elder_pool <- remove_first_and_last(weekly_elder_pool)
# acf
acf(weekly_elder_pool$count)
plot.ts(weekly_elder_pool$count)
acf(diff(weekly_elder_pool$count))
plot.ts(diff(weekly_elder_pool$count))
weekly_elder_pool <- weekly_elder_pool[,c("Year.of.Discharge", "Week.of.Discharge", "count")]
# arima
weekly_elder_pool_model <- arima(weekly_elder_pool$count, order = c(1,0,0))
weekly_elder_pool_prediction <-predict(weekly_elder_pool_model,n.ahead=prediction.length)
weekly_elder_pool_discharges_pred <- weekly_elder_pool_prediction$pred[1:prediction.length]
plot.ts(c(weekly_elder_pool$count, weekly_elder_pool_discharges_pred))
# create df
disch_elder_pool_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                        Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                        weekly_count = weekly_elder_pool_discharges_pred),
                             weekly_count = weekly_count,
                             Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                             Year.of.Discharge = as.character(Year.of.Discharge))
# Unscheduled Care
weekly_unsched_pool <- filter(patients_weekly_resources, Resource.Pool.name == "Unscheduled Care")
weekly_unsched_pool <- remove_first_and_last(weekly_unsched_pool)
# acf
acf(weekly_unsched_pool$count)
plot.ts(weekly_unsched_pool$count)
acf(diff(weekly_unsched_pool$count))
plot.ts(diff(weekly_unsched_pool$count))
weekly_unsched_pool <- weekly_unsched_pool[,c("Year.of.Discharge", "Week.of.Discharge", "count")]
# arima
weekly_unsched_pool_model <- arima(weekly_unsched_pool$count, order = c(1,1,0))
weekly_unsched_pool_prediction <-predict(weekly_unsched_pool_model,n.ahead=prediction.length)
weekly_unsched_pool_discharges_pred <- weekly_unsched_pool_prediction$pred[1:prediction.length]
plot.ts(c(weekly_unsched_pool$count, weekly_unsched_pool_discharges_pred))
# create df
disch_unsched_pool_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                          Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                          weekly_count = weekly_unsched_pool_discharges_pred),
                               weekly_count = weekly_count,
                               Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                               Year.of.Discharge = as.character(Year.of.Discharge))
# Palliative Care
weekly_palliat_pool <- filter(patients_weekly_resources, Resource.Pool.name == "Palliative Care")
weekly_palliat_pool <- remove_first_and_last(weekly_palliat_pool)
# acf
acf(weekly_palliat_pool$count)
plot.ts(weekly_palliat_pool$count)
acf(diff(weekly_palliat_pool$count))
plot.ts(diff(weekly_palliat_pool$count))
weekly_palliat_pool <- weekly_palliat_pool[,c("Year.of.Discharge", "Week.of.Discharge", "count")]
# arima
weekly_palliat_pool_model <- arima(weekly_palliat_pool$count, order = c(1,0,0))
weekly_palliat_pool_prediction <-predict(weekly_palliat_pool_model,n.ahead=prediction.length)
weekly_palliat_pool_discharges_pred <- weekly_palliat_pool_prediction$pred[1:prediction.length]
plot.ts(c(weekly_palliat_pool$count, weekly_palliat_pool_discharges_pred))
# create df
disch_palliat_pool_pred <-mutate(data.frame(Year.of.Discharge = rep(last.year,prediction.length),
                                            Week.of.Discharge = (last.week+1):(last.week+prediction.length),
                                            weekly_count = weekly_palliat_pool_discharges_pred),
                                 weekly_count = weekly_count,
                                 Week.of.Discharge = sprintf("%02d", Week.of.Discharge),
                                 Year.of.Discharge = as.character(Year.of.Discharge))

### Data reconciliation to match weekly totals
## Join all groups of discharge and total discharges together in one df
weekly_discharges_pred <- rename(disch_pred, all_discharges = weekly_count) %>%
                          full_join(rename(disch_normal_pred, normal_discharge = weekly_count), 
                                    by=c("Year.of.Discharge", "Week.of.Discharge")) %>%
                          full_join(rename(disch_transfer_pred, external_transfer = weekly_count), 
                                    by=c("Year.of.Discharge", "Week.of.Discharge")) %>%
                          full_join(rename(disch_palliative_pred, palliative_deceased = weekly_count), 
                                    by=c("Year.of.Discharge", "Week.of.Discharge"))

weekly_discharges_pred[is.na(weekly_discharges_pred)] <- 0 # Replace NAs by "0"

weekly_discharges_pred$sum_disch_types <- weekly_discharges_pred$normal_discharge + weekly_discharges_pred$external_transfer + 
                                          weekly_discharges_pred$palliative_deceased

weekly_discharges_pred$adjustement_factor <- weekly_discharges_pred$all_discharges / weekly_discharges_pred$sum_disch_types

weekly_discharges_pred$normal_discharge_adjusted <- weekly_discharges_pred$normal_discharge * weekly_discharges_pred$adjustement_factor
weekly_discharges_pred$external_transfer_adjusted <- weekly_discharges_pred$external_transfer * weekly_discharges_pred$adjustement_factor
weekly_discharges_pred$palliative_deceased_adjusted <- weekly_discharges_pred$palliative_deceased * weekly_discharges_pred$adjustement_factor

# Checking that the sum of all adjusted columns equals to all_discharged column
weekly_discharges_pred$check_adjustments <- weekly_discharges_pred$normal_discharge_adjusted + weekly_discharges_pred$external_transfer_adjusted + 
                                            weekly_discharges_pred$palliative_deceased_adjusted
test_adjustements <- filter(weekly_discharges_pred, all_discharges != check_adjustments)
test_adjustements <- test_adjustements[,c("Year.of.Discharge", "Week.of.Discharge", 
                                          "all_discharges", "check_adjustments")]
# 9 out of 20 records are returned -> visually "all_discharges" are equal to "check_adjustments" to the 4th decimal place

weekly_discharges_pred <- weekly_discharges_pred[,c("Year.of.Discharge", "Week.of.Discharge", "all_discharges", 
                                                    "normal_discharge_adjusted", "external_transfer_adjusted",
                                                    "palliative_deceased_adjusted")]
## Reconcile weekly predictions per resource pool
weekly_disch_resource_pred <- rename(disch_pred, all_discharges = weekly_count) %>%
                              full_join(rename(disch_medic_pool_pred, medical = weekly_count), 
                                        by=c("Year.of.Discharge", "Week.of.Discharge")) %>%
                              full_join(rename(disch_surgic_pool_pred, surgical = weekly_count), 
                                        by=c("Year.of.Discharge", "Week.of.Discharge")) %>%
                              full_join(rename(disch_wac_pool_pred, women_and_child = weekly_count), 
                                        by=c("Year.of.Discharge", "Week.of.Discharge")) %>%
                              full_join(rename(disch_elder_pool_pred, elderly_care = weekly_count), 
                                        by=c("Year.of.Discharge", "Week.of.Discharge")) %>%
                              full_join(rename(disch_unsched_pool_pred, unscheduled_care = weekly_count), 
                                        by=c("Year.of.Discharge", "Week.of.Discharge")) %>%
                              full_join(rename(disch_palliat_pool_pred, palliative_care = weekly_count), 
                                        by=c("Year.of.Discharge", "Week.of.Discharge"))

weekly_disch_resource_pred$sum_disch_types <- weekly_disch_resource_pred$medical + 
                                              weekly_disch_resource_pred$surgical + 
                                              weekly_disch_resource_pred$women_and_child +
                                              weekly_disch_resource_pred$elderly_care + 
                                              weekly_disch_resource_pred$unscheduled_care + 
                                              weekly_disch_resource_pred$palliative_care

weekly_disch_resource_pred$adjustement_factor <- weekly_disch_resource_pred$all_discharges / weekly_disch_resource_pred$sum_disch_types

weekly_disch_resource_pred$medical_adjusted <- weekly_disch_resource_pred$medical * weekly_disch_resource_pred$adjustement_factor
weekly_disch_resource_pred$surgical_adjusted <- weekly_disch_resource_pred$surgical * weekly_disch_resource_pred$adjustement_factor
weekly_disch_resource_pred$women_and_child_adjusted <- weekly_disch_resource_pred$women_and_child * weekly_disch_resource_pred$adjustement_factor
weekly_disch_resource_pred$elderly_care_adjusted <- weekly_disch_resource_pred$elderly_care * weekly_disch_resource_pred$adjustement_factor
weekly_disch_resource_pred$unscheduled_care_adjusted <- weekly_disch_resource_pred$unscheduled_care * weekly_disch_resource_pred$adjustement_factor
weekly_disch_resource_pred$palliative_care_adjusted <- weekly_disch_resource_pred$palliative_care * weekly_disch_resource_pred$adjustement_factor

weekly_disch_resource_pred <- weekly_disch_resource_pred[,c("Year.of.Discharge", "Week.of.Discharge", "medical_adjusted",
                                                            "surgical_adjusted", "women_and_child_adjusted", 
                                                            "elderly_care_adjusted", "unscheduled_care_adjusted", 
                                                            "palliative_care_adjusted")]

## Join predictions with confidence intervals
hist_weekly_discharges <- patients_exit_hospital %>%
                          group_by(Year.of.Discharge, Week.of.Discharge, discharge_group) %>%
                          arrange(Year.of.Discharge, Week.of.Discharge) %>%
                          summarise(weekly_count = n())

weekly_discharges_pred_with_ci <- hist_weekly_discharges %>%
                                  bind_rows(mutate(disch_normal_pred_ci, discharge_group = "Normal Discharge")) %>%
                                  bind_rows(mutate(disch_transfer_pred_ci, discharge_group = "External Transfer")) %>%
                                  bind_rows(mutate(disch_palliative_pred_ci, discharge_group = "Palliative/Deceased"))

write.csv(weekly_discharges_pred_with_ci, file = "weekly_discharges_historic_and_predictions.csv", row.names = F)

## Break down weekly predictions into daily predictions
normal_dish_daily <- weekly_discharges_pred %>%
                      select(Year.of.Discharge, Week.of.Discharge, normal_discharge_adjusted) %>%
                      rename(weekly_count = normal_discharge_adjusted) %>%
                      calculate.weekly.disch.from.proportions(weekday_discharge_proportions) %>%
                      select(Year.of.Discharge, Week.of.Discharge, date, daily_count)

external_dish_daily <- weekly_discharges_pred %>%
                       select(Year.of.Discharge, Week.of.Discharge, external_transfer_adjusted) %>%
                       rename(weekly_count = external_transfer_adjusted) %>%
                       calculate.weekly.disch.from.proportions(weekday_discharge_proportions) %>%
                       select(Year.of.Discharge, Week.of.Discharge, date, daily_count)

palliative_dish_daily <- weekly_discharges_pred %>%
                         select(Year.of.Discharge, Week.of.Discharge, palliative_deceased_adjusted) %>%
                         rename(weekly_count = palliative_deceased_adjusted) %>%
                         calculate.weekly.disch.from.proportions(weekday_discharge_proportions) %>%
                         select(Year.of.Discharge, Week.of.Discharge, date, daily_count)

## Break down daily predictions into resource predictions
daily_normal_disch_per_resource <- calculate.resource_pool.count.from.proportions(normal_dish_daily, 
                                                                                  resource_discharge_proportions) %>%
                                   select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, count) %>%
                                   add_column_with_value_to("discharge_group", "Normal Discharge")

daily_transfer_disch_per_resource <- calculate.resource_pool.count.from.proportions(external_dish_daily, 
                                                                                    resource_discharge_proportions) %>%
                                     select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, count) %>%
                                     add_column_with_value_to("discharge_group", "External Transfer")

daily_palliative_disch_per_resource <- calculate.resource_pool.count.from.proportions(palliative_dish_daily, 
                                                                                      resource_discharge_proportions) %>%
                                       select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, count) %>%
                                       add_column_with_value_to("discharge_group", "Palliative/Deceased")

## Break down resource predictions into ward
pred_by_disch_by_resource <- daily_normal_disch_per_resource %>% 
                             bind_rows(daily_transfer_disch_per_resource) %>%
                             bind_rows(daily_palliative_disch_per_resource)
# -> then break by resource pool
# Date and Ward name df to merge with the df of discharge predictions per discharge group and resource pool
ward_data <- exits_per_spe_for_ward %>%
             group_by(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge, Resource.Pool.name, 
                      discharge_group, Ward.Name) %>%
             select(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge, Resource.Pool.name, 
                    discharge_group, Ward.Name)
# Medical
medical_per_ward <- pred_by_disch_by_resource %>%
                    filter(Resource.Pool.name == "Medical") %>%
                    calculate.ward.count.from.proportions(disch_medical_proportions) %>%
                    select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, 
                           discharge_group, Ward.Name, ward_count)
# Surgical
surgical_per_ward <- pred_by_disch_by_resource %>%
                     filter(Resource.Pool.name == "Surgical") %>%
                     calculate.ward.count.from.proportions(disch_surgical_proportions) %>%
                     select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, 
                            discharge_group, Ward.Name, ward_count)
# Women and Child
wac_per_ward <- pred_by_disch_by_resource %>%
                filter(Resource.Pool.name == "Women and Child") %>%
                calculate.ward.count.from.proportions(disch_wac_proportions) %>%
                select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, 
                       discharge_group, Ward.Name, ward_count)
# Elderly Care
elderly_per_ward <- pred_by_disch_by_resource %>%
                    filter(Resource.Pool.name == "Elderly Care") %>%
                    calculate.ward.count.from.proportions(disch_elderly_proportions) %>%
                    select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, 
                           discharge_group, Ward.Name, ward_count)
# Unscheduled Care
unsched_per_ward <- pred_by_disch_by_resource %>%
                    filter(Resource.Pool.name == "Unscheduled Care") %>%
                    calculate.ward.count.from.proportions(disch_unsched_proportions) %>%
                    select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, 
                           discharge_group, Ward.Name, ward_count)
# Palliative Care
palliative_per_ward <- pred_by_disch_by_resource %>%
                       filter(Resource.Pool.name == "Palliative Care") %>%
                       calculate.ward.count.from.proportions(disch_palliative_proportions) %>%
                       select(Year.of.Discharge, Week.of.Discharge, date, Resource.Pool.name, 
                              discharge_group, Ward.Name, ward_count)

disch_per_resource_per_ward <- bind_rows(medical_per_ward, surgical_per_ward) %>%
                               bind_rows(wac_per_ward) %>%
                               bind_rows(elderly_per_ward) %>%
                               bind_rows(unsched_per_ward) %>%
                               bind_rows(palliative_per_ward)
write.csv(disch_per_resource_per_ward, file = "discharge-predictions-ward-level.csv", row.names = F)

# Check the results add up at resource pools and discharge type levels
check_resource_counts <- disch_per_resource_per_ward %>%
                         group_by(Year.of.Discharge, Week.of.Discharge, date, 
                                  discharge_group, Resource.Pool.name) %>%
                         summarise(resource_count = sum(ward_count))

resources_pred <- bind_rows(daily_normal_disch_per_resource, daily_transfer_disch_per_resource) %>%
                  bind_rows(daily_palliative_disch_per_resource)

compare_resource_counts <- merge(check_resource_counts, resources_pred)

errors <- filter(compare_resource_counts, abs(compare_resource_counts$resource_count - compare_resource_counts$count) > 0.01)
# => errors has 0 records so the results add up :)

## Merge all predictions together and with historical data
historical_discharges <- exits_per_speciality %>%
                         group_by(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge, Resource.Pool.name, discharge_group) %>%
                         arrange(Year.of.Discharge, Week.of.Discharge, Date.of.Discharge) %>%
                         summarise(count = n()) %>%
                         rename(date = Date.of.Discharge)

discharge_predictions <- bind_rows(daily_normal_disch_per_resource, daily_transfer_disch_per_resource) %>%
                         bind_rows(daily_palliative_disch_per_resource) %>%
                         bind_rows(historical_discharges) %>%
                         mutate(count = round(count, digits = 2)) %>%
                         arrange(Year.of.Discharge, Week.of.Discharge, date)

write.csv(discharge_predictions, file = "patients_discharge_historic_and_predictions.csv", row.names = F)


## Looking at Discharges and wards
ward.ignore <- c("Antrim Dpu/Endoscopy Unit", "Antrim Induction Unit", "Antrim (C) Neonatal Unit", "Fetal Maternal Assessment Unit", "A4h Haemodialysis Unit", "Antrim Childrens Ambulatory", "Antrim Special Care Baby Int.", "Chemotherapy Unit Laurel House",
                 "Antrim Outpatients Department", "A3h Medical", "Trolley Waits In Day Procedure", "Recovery Area Antrim", "Renal Unit Antrim Hospital", "Operating Theatres", "Closed Do Not Use", "Cardiac Procedure Room Level B", "Day Surgery Unit",
                 "Ant Short Stay Ward Ambulatory", "Acute Assessment Unit", "Short Stay Wrd Closed05/07/13","A1a Ward Rheumatology", "A2 Assessment Unit", "A3tr Trolley Wait Holding Area", "Accident And Emergency Obs", "A&E Trolley Waits", "C3 Trolley Waits Holding Area",
                 "B5 Closed From 03/10/16")

discharges_per_ward <- patients_exit_hospital %>%
                       filter(!Ward.Name %in% ward.ignore) %>%
                       group_by(Ward.Name) %>%
                       summarise(count = n())

ggplot(data=discharges_per_ward, aes(x=reorder(Ward.Name,-count), y=count)) +
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
