require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
require(forecast)

# for our own data source paths
source("paths.R")

all_na_to_0 <- function(d) {
  d[is.na(d)] <- 0
  d
}

remove_first_and_last_n <- function(dataframe, n) {
  head(dataframe[-n,], -n) # remove first and last row
}

add_column_with_value_to <- function(dataframe, colname, value) {
  column <- rep(value, nrow(dataframe))
  dataframe[ , colname] <- column
  dataframe
}

prediction.length <- 20

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

patient.admissions <- filter(patient.data, Mode.of.Entry.to.Ward == "ADM")

# reconciliation to find resource pool (DUMMY FOR NOW)
resource_pool.specialties <- read.csv(resource_pool.specialties.path, stringsAsFactors = F,na.strings=c("","NA"))
patient.admissions <- left_join(patient.admissions, resource_pool.specialties, by=c("Specialty.Description.on.Ward.Entry" = "PAS.name")) 

# group by day, resource pool and admission type, and plot over time
# first per resource pool and day
patients.per.day <- tally(group_by(patient.admissions, Date.of.Admission, Resource.Pool.name)) %>%
                    dcast(Date.of.Admission ~ Resource.Pool.name) %>%
                    setNames(c("Date.of.Admission","elderly","medical","palliative","surgical","unscheduled","women.child"))
patients.per.day <- all_na_to_0(patients.per.day)

ggplot(patients.per.day, aes(Date.of.Admission)) + 
  geom_line(aes(y = patients.per.day$elderly, colour = "Elderly Care")) +
  geom_line(aes(y = patients.per.day$medical, colour = "Medical")) +
  geom_line(aes(y = patients.per.day$palliative, colour = "Palliative Care")) +
  geom_line(aes(y = patients.per.day$surgical, colour = "Surgical")) +
  geom_line(aes(y = patients.per.day$unscheduled, colour = "Unscheduled Care")) +
  geom_line(aes(y = patients.per.day$women.child, colour = "Women and Child"))

acf(patients.per.day$elderly)
acf(diff(patients.per.day$elderly)) # no cycles
acf(diff(patients.per.day$unscheduled)) # nope

acf(patients.per.day$medical) # 7 day cycle
acf(patients.per.day$palliative) # 7 day cycle
acf(patients.per.day$surgical)
pacf(patients.per.day$surgical) # possible 7 day cycle, however memory effect
acf(patients.per.day$women.child) # might be 7 day cycle

patients.per.week <- tally(group_by(patient.admissions, Year.Week, Resource.Pool.name)) %>%
  dcast(Year.Week ~ Resource.Pool.name) %>%
  setNames(c("Year.Week","elderly","medical","palliative","surgical","unscheduled","women.child"))
patients.per.week <- all_na_to_0(patients.per.week)

ggplot(patients.per.week, aes(Year.Week)) + 
  geom_line(aes(y = patients.per.week$elderly, colour = "Elderly Care", group=1)) +
  geom_line(aes(y = patients.per.week$medical, colour = "Medical", group=1)) +
  geom_line(aes(y = patients.per.week$palliative, colour = "Palliative Care", group=1)) +
  geom_line(aes(y = patients.per.week$surgical, colour = "Surgical", group=1)) +
  geom_line(aes(y = patients.per.week$unscheduled, colour = "Unscheduled Care", group=1)) +
  geom_line(aes(y = patients.per.week$women.child, colour = "Women and Child", group=1)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(patients.per.week, aes(Year.Week)) + 
  geom_line(aes(y = patients.per.week$elderly, colour = "Elderly Care", group=1))
acf(patients.per.week$elderly) #nope
ggplot(patients.per.week, aes(Year.Week)) + 
  geom_line(aes(y = patients.per.week$medical, colour = "Medical", group=1))
acf(patients.per.week$medical)

# predict totals
total.patients.per.week <- group_by(patient.admissions, Year.of.Admission, Week.of.Admission) %>%
                           summarize(weekly.count = n()) %>%
                           remove_first_and_last_n(1)

last.year <- max(total.patients.per.week$Year.of.Admission)
last.week <- as.integer(last(total.patients.per.week$Week.of.Admission))

total.patients.model <- arima(total.patients.per.week$weekly.count, order = c(1,0,1))
plot.ts(total.patients.per.week$weekly.count)
total.prediction <- predict(total.patients.model, n.ahead = prediction.length)
total.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                           Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                           weekly.count = total.prediction$pred[1:prediction.length]),
                         weekly.count = as.integer(weekly.count),
                         Week.of.Admission = sprintf("%02d", Week.of.Admission),
                         Year.of.Admission = as.character(Year.of.Admission))
total.admissions <- bind_rows(total.patients.per.week, total.prediction.data)

# emergency admissions
emergency.admissions.wk <- filter(patient.admissions, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n()) %>%
  remove_first_and_last_n(1)

emergency.admissions.model <- arima(emergency.admissions.wk$weekly.count, order = c(1,1,1))
#emergency.admissions.forecast <- forecast(emergency.admissions.model, level = c(95), h = prediction.length)
#autoplot(emergency.admissions.forecast) # to see confidence interval on prediction
emergency.admissions.prediction <- predict(emergency.admissions.model, n.ahead = prediction.length)
emergency.admissions.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                           Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                           weekly.count = emergency.admissions.prediction$pred[1:prediction.length]),
                                weekly.count = as.integer(weekly.count),
                                Week.of.Admission = sprintf("%02d", Week.of.Admission),
                                Year.of.Admission = as.character(Year.of.Admission))
emergency.admissions <- bind_rows(emergency.admissions.wk, emergency.admissions.prediction.data)

# maternity
maternity.admissions.wk <- filter(patient.admissions, Method.of.Admission.Category == "Maternity Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n()) %>%
  remove_first_and_last_n(1)
maternity.admissions.model <- arima(maternity.admissions.wk$weekly.count, order = c(1,1,1))
#maternity.admissions.forecast <- forecast(maternity.admissions.model, level = c(95), h = prediction.length)
#autoplot(maternity.admissions.forecast) # to see confidence interval on prediction
maternity.admissions.prediction <- predict(maternity.admissions.model, n.ahead = prediction.length)
maternity.admissions.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                                          Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                                          weekly.count = maternity.admissions.prediction$pred[1:prediction.length]),
                                               weekly.count = as.integer(weekly.count),
                                               Week.of.Admission = sprintf("%02d", Week.of.Admission),
                                               Year.of.Admission = as.character(Year.of.Admission))
maternity.admissions <- bind_rows(maternity.admissions.wk, maternity.admissions.prediction.data)

# Other admissions
other.admissions.wk <- filter(patient.admissions, Method.of.Admission.Category == "Other Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n()) %>%
  remove_first_and_last_n(1)
other.admissions.model <- arima(other.admissions.wk$weekly.count, order = c(1,1,1))
#other.admissions.forecast <- forecast(other.admissions.model, level = c(95), h = prediction.length)
#autoplot(other.admissions.forecast) # to see confidence interval on prediction
other.admissions.prediction <- predict(other.admissions.model, n.ahead = prediction.length)
other.admissions.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                                          Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                                          weekly.count = other.admissions.prediction$pred[1:prediction.length]),
                                               weekly.count = as.integer(weekly.count),
                                               Week.of.Admission = sprintf("%02d", Week.of.Admission),
                                               Year.of.Admission = as.character(Year.of.Admission))
other.admissions <- bind_rows(other.admissions.wk, other.admissions.prediction.data)

# Elective admissions
elective.admissions.wk <- filter(patient.admissions, Method.of.Admission.Category == "Elective Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n()) %>%
  remove_first_and_last_n(1)
elective.admissions.model <- arima(elective.admissions.wk$weekly.count, order = c(1,1,1))
#elective.admissions.forecast <- forecast(elective.admissions.model, level = c(95), h = prediction.length)
#autoplot(elective.admissions.forecast) # to see confidence interval on prediction
elective.admissions.prediction <- predict(elective.admissions.model, n.ahead = prediction.length)
elective.admissions.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                                      Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                                      weekly.count = elective.admissions.prediction$pred[1:prediction.length]),
                                           weekly.count = as.integer(weekly.count),
                                           Week.of.Admission = sprintf("%02d", Week.of.Admission),
                                           Year.of.Admission = as.character(Year.of.Admission))
elective.admissions <- bind_rows(elective.admissions.wk, elective.admissions.prediction.data)

# sum all the predictions and find factor to make them equal to total
all.admissions <- bind_rows(add_column_with_value_to(emergency.admissions, "Method.of.Admission", "emergency"),
                            add_column_with_value_to(maternity.admissions, "Method.of.Admission", "maternity")) %>%
  bind_rows(add_column_with_value_to(other.admissions, "Method.of.Admission", "other")) %>%
  bind_rows(add_column_with_value_to(elective.admissions, "Method.of.Admission", "elective")) %>%
  bind_rows(add_column_with_value_to(total.admissions, "Method.of.Admission", "total"))  %>%
  dcast(Year.of.Admission + Week.of.Admission ~ Method.of.Admission, value.var = c("weekly.count")) %>%
  mutate(total.of.parts = emergency + maternity + other + elective,
         ratio.to.total = total.of.parts/total)



# check resource pool per type of admission
# emergency admissions
# emergency.patients.per.week <- group_by(patient.admissions, Year.Week, Resource.Pool.name) %>%
#                      filter(Method.of.Admission.Category == "Emergency Admission") %>%
#                      tally() %>%
#                      dcast(Year.Week ~ Resource.Pool.name)  %>%
#                      setNames(c("Year.Week","elderly","medical","palliative","surgical","unscheduled","women.child")) %>%
#                      all_na_to_0()
# # pretty much no palliative care
# # the rest varies
# ggplot(emergency.patients.per.week, aes(Year.Week)) +
#   geom_line(aes(y = emergency.patients.per.week$elderly, colour = "Elderly Care", group=1)) +
#   geom_line(aes(y = emergency.patients.per.week$medical, colour = "Medical", group=1)) +
#   geom_line(aes(y = emergency.patients.per.week$surgical, colour = "Surgical", group=1)) +
#   geom_line(aes(y = emergency.patients.per.week$unscheduled, colour = "Unscheduled Care", group=1)) +
#   geom_line(aes(y = emergency.patients.per.week$women.child, colour = "Women and Child", group=1)) +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# acf(emergency.patients.per.week$elderly) #still nope
# ggplot(emergency.patients.per.week, aes(Year.Week)) + 
#   geom_line(aes(y = emergency.patients.per.week$elderly, colour = "Elderly Care", group=1)) + 
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# emergency.elderly.model <- arima(emergency.patients.per.week$elderly, order=c(1,0,1))
# emergency.elderly.prediction <- predict(emergency.elderly.model, n.ahead=prediction.length, interval = 'confidence')
# emergency.elderly.forecast <- forecast(emergency.elderly.model, level = c(95), h = prediction.length)
# autoplot(emergency.elderly.forecast)
