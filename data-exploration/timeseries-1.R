require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)

# for our own data source paths
source("paths.R")

remove_first_and_last <- function(dataframe) {
  head(dataframe[-1,], -1) # remove first and last row
}

calculate.weekly.counts.from.proportions <- function(weekly.data, proportions.data) {
  bind_rows(mutate(weekly.data, day = 1),
            mutate(weekly.data, day = 2),
            mutate(weekly.data, day = 3),
            mutate(weekly.data, day = 4),
            mutate(weekly.data, day = 5),
            mutate(weekly.data, day = 6),
            mutate(weekly.data, day = 7)) %>%
    arrange(Year.of.Admission,Week.of.Admission, day) %>%
    mutate(date = as.Date(paste(Year.of.Admission, Week.of.Admission, (day-1), sep="-"), format="%Y-%U-%w"))  %>%
    mutate(date = if_else(is.na(date),
                          as.Date(paste((as.integer(Year.of.Admission)+1), 0, (day-1), sep="-"), format="%Y-%U-%w"),
                          date))  %>% # remove the last days of the last week
    mutate(Admission.weekdays = weekdays(date, abbreviate = FALSE)) %>%
    inner_join(proportions.data) %>%
    mutate(daily.count = weekly.count * proportions)
}

patient.data <- read.csv(patient.data.path, stringsAsFactors = F, na.strings=c("","NA")) %>%
  mutate(
    Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    Week.of.Admission = format(Date.of.Admission, "%U"),
    Year.of.Admission = format(Date.of.Admission, "%Y"),
    Year.Week = paste(Year.of.Admission, Week.of.Admission, sep="-"),
    Day.Admission = format(Date.of.Admission, "%d"),
    Month.Admission = format(Date.of.Admission, "%m"),
    Date.of.Discharge = as.Date(Date.of.Discharge.with.Time,format="%d-%b-%Y %H:%M"),
    DateTime.of.Admission = as.POSIXct(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    Time.of.Admission = strftime(as.POSIXct(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"), format="%H:%M"),
    age.num = as.integer(Age),
    Sex = as.factor(Sex),
    Method.of.Admission.Category = as.factor(Method.of.Admission.Category),
    age.group = as.character(cut(Age, breaks=c(-1,10,20,30,40,50,60,70,80,90,Inf),
                                 labels=c('0-10', '10-20', '20-30', '30-40', '40-50', '50-60', '60-70', '70-80', '80-90', 'Over 90')))
  )

# group by hospital stay (patient, same day admitted, same day discharged)
by_stay <- group_by(patient.data, H.C.Encrypted,age.num, age.group,Sex,Date.of.Admission,Time.of.Admission,DateTime.of.Admission, Date.of.Discharge,Method.of.Admission.Category,Method.of.Discharge, Year.Week, Day.Admission, Week.of.Admission, Month.Admission, Year.of.Admission)
patient.hospital.stays <- summarize(by_stay,
                                    count = n(),
                                    distinct.wards = n_distinct(Ward.Name)) %>%
  mutate(length.of.stay = as.integer(Date.of.Discharge - Date.of.Admission))


### Emergency admissions prediction
## Weeks
emergency.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n())

emergency.admissions.wk <- remove_first_and_last(emergency.admissions.wk)

last.year <- max(emergency.admissions.wk$Year.of.Admission)
last.week <- as.integer(last(emergency.admissions.wk$Week.of.Admission))
prediction.length <- 20

em.model <- arima(emergency.admissions.wk$weekly.count, order = c(1,1,1))
em.prediction <-predict(em.model,n.ahead=prediction.length)
em.admissions.pred <- em.prediction$pred[1:prediction.length]
# we just use the same year for this situation
# together in data set
em.pred.data <-mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                 Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                 weekly.count = em.admissions.pred),
                         weekly.count = as.integer(weekly.count),
                         Week.of.Admission = sprintf("%02d", Week.of.Admission),
                         Year.of.Admission = as.character(Year.of.Admission))

emergency.admissions.final <- bind_rows(emergency.admissions.wk,em.pred.data)

## Weekdays
weekday.list <- c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
patient.hospital.stays$Admission.weekdays <- weekdays(patient.hospital.stays$Date.of.Admission, abbreviate = FALSE)

admissions.per.weekday <- patient.hospital.stays %>%
                          group_by(Admission.weekdays) %>%
                          summarize(Count = n()) %>%
                          mutate(weekday.ordered = factor(Admission.weekdays, levels = weekday.list)) %>%
                          arrange(weekday.ordered)

total_admissions <- nrow(patient.hospital.stays)

weekday.admissions.proportions <- admissions.per.weekday %>%
                                  mutate(proportions = Count / total_admissions)
ggplot(data=admissions.per.weekday, aes(x=weekday.ordered, y=Count)) + geom_bar(stat="identity")


emergency.admissions.per.weekday <- filter(patient.hospital.stays, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Admission.weekdays) %>%
  summarize(Count = n()) %>%
  mutate(weekday.ordered = factor(Admission.weekdays, levels = weekday.list)) %>%
  arrange(weekday.ordered)
emergency.total_admissions <- nrow(filter(patient.hospital.stays, Method.of.Admission.Category == "Emergency Admission"))
emergency.weekday.admissions.proportions <- emergency.admissions.per.weekday %>%
  mutate(proportions = Count / emergency.total_admissions) %>%
  select(Admission.weekdays, proportions)
ggplot(data=emergency.admissions.per.weekday, aes(x=weekday.ordered, y=Count)) + geom_bar(stat="identity")
#weeks_dates <- patient.hospital.stays %>%
               #group_by(Week.of.Admission, Date.of.Admission)

# using proportions to calculate counts for days of week
emergency.admissions.days <- calculate.weekly.counts.from.proportions(emergency.admissions.final, emergency.weekday.admissions.proportions)

# breakdown that matters: method of admission , gender, age


### Maternity admissions

maternity.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Maternity Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n())
maternity.admissions.wk <- remove_first_and_last(maternity.admissions.wk)

acf(diff(maternity.admissions.wk$weekly.count)) # not very conclusive, no clear pattern
plot.ts(maternity.admissions.wk$weekly.count)
plot.ts(diff(maternity.admissions.wk$weekly.count))
mat.model <- arima(maternity.admissions.wk$weekly.count, order = c(1,0,0)) #simplest has best fit ...
mat.prediction <-predict(mat.model,n.ahead=prediction.length)
mat.admissions.pred <- mat.prediction$pred[1:prediction.length]
plot.ts(c(maternity.admissions.wk$weekly.count,mat.admissions.pred))

mat.pred.data <-mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                 Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                 weekly.count = mat.admissions.pred),
                      weekly.count = as.integer(weekly.count),
                      Week.of.Admission = sprintf("%02d", Week.of.Admission),
                      Year.of.Admission = as.character(Year.of.Admission))
maternity.admissions.final <- bind_rows(maternity.admissions.wk,mat.pred.data)

# calculate proportions
maternity.admissions.per.weekday <- filter(patient.hospital.stays, Method.of.Admission.Category == "Maternity Admission") %>%
  group_by(Admission.weekdays) %>%
  summarize(Count = n()) %>%
  mutate(weekday.ordered = factor(Admission.weekdays, levels = weekday.list)) %>%
  arrange(weekday.ordered)
maternity.total_admissions <- nrow(filter(patient.hospital.stays, Method.of.Admission.Category == "Maternity Admission"))
maternity.weekday.admissions.proportions <- maternity.admissions.per.weekday %>%
  mutate(proportions = Count / maternity.total_admissions) %>%
  select(Admission.weekdays, proportions)
ggplot(data=maternity.admissions.per.weekday, aes(x=weekday.ordered, y=Count)) + geom_bar(stat="identity")

maternity.admissions.days <- calculate.weekly.counts.from.proportions(maternity.admissions.final, maternity.weekday.admissions.proportions)


### Other admissions

other.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Other Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n())

### Elective admissions

elective.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Elective Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n())

### TODO:
# predictions at weekly level
# probabilities per day: split over day of week (getting daily counts)
# probabilities per hour: split over hour per day


# which things are we going to predict