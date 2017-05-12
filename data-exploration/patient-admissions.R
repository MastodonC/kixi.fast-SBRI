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

calculate.daily.counts.from.proportions <- function(weekly.data, proportions.data) {
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
    mutate(Admission.Weekday = weekdays(date, abbreviate = FALSE))  %>%
    inner_join(proportions.data) %>%
    mutate(daily.count = weekly.count * proportion)
}

calculate.resource_pool.count.from.proportions <- function(daily.data, proportions.data) {
  bind_rows(mutate(daily.data, Resource.Pool.name = "Elderly Care"),
            mutate(daily.data, Resource.Pool.name = "Medical"),
            mutate(daily.data, Resource.Pool.name = "Palliative Care"),
            mutate(daily.data, Resource.Pool.name = "Surgical"),
            mutate(daily.data, Resource.Pool.name = "Unscheduled Care"),
            mutate(daily.data, Resource.Pool.name = "Women and Child")) %>%
    inner_join(proportions.data) %>%
    mutate(count = daily.count * proportion)
}


prediction.length <- 20
weekday.list <- c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

patient.data <- read.csv(patient.data.path, stringsAsFactors = F, na.strings=c("","NA")) %>%
  mutate(
    Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    Week.of.Admission = format(Date.of.Admission, "%U"),
    Year.of.Admission = format(Date.of.Admission, "%Y"),
    Year.Week = paste(Year.of.Admission, Week.of.Admission, sep="-"),
    Day.Admission = format(Date.of.Admission, "%d"),
    Month.Admission = format(Date.of.Admission, "%m"),
    Admission.Weekday = weekdays(Date.of.Admission, abbreviate = F),
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
#patients.per.day <- tally(group_by(patient.admissions, Date.of.Admission, Resource.Pool.name)) %>%
#                     dcast(Date.of.Admission ~ Resource.Pool.name) %>%
#                     setNames(c("Date.of.Admission","elderly","medical","palliative","surgical","unscheduled","women.child"))
# patients.per.day <- all_na_to_0(patients.per.day)
# 
# ggplot(patients.per.day, aes(Date.of.Admission)) + 
#   geom_line(aes(y = patients.per.day$elderly, colour = "Elderly Care")) +
#   geom_line(aes(y = patients.per.day$medical, colour = "Medical")) +
#   geom_line(aes(y = patients.per.day$palliative, colour = "Palliative Care")) +
#   geom_line(aes(y = patients.per.day$surgical, colour = "Surgical")) +
#   geom_line(aes(y = patients.per.day$unscheduled, colour = "Unscheduled Care")) +
#   geom_line(aes(y = patients.per.day$women.child, colour = "Women and Child"))
# 
# acf(patients.per.day$elderly)
# acf(diff(patients.per.day$elderly)) # no cycles
# acf(diff(patients.per.day$unscheduled)) # nope
# 
# acf(patients.per.day$medical) # 7 day cycle
# acf(patients.per.day$palliative) # 7 day cycle
# acf(patients.per.day$surgical)
# pacf(patients.per.day$surgical) # possible 7 day cycle, however memory effect
# acf(patients.per.day$women.child) # might be 7 day cycle
# 
# patients.per.week <- tally(group_by(patient.admissions, Year.Week, Resource.Pool.name)) %>%
#   dcast(Year.Week ~ Resource.Pool.name) %>%
#   setNames(c("Year.Week","elderly","medical","palliative","surgical","unscheduled","women.child"))
# patients.per.week <- all_na_to_0(patients.per.week)
# 
# ggplot(patients.per.week, aes(Year.Week)) + 
#   geom_line(aes(y = patients.per.week$elderly, colour = "Elderly Care", group=1)) +
#   geom_line(aes(y = patients.per.week$medical, colour = "Medical", group=1)) +
#   geom_line(aes(y = patients.per.week$palliative, colour = "Palliative Care", group=1)) +
#   geom_line(aes(y = patients.per.week$surgical, colour = "Surgical", group=1)) +
#   geom_line(aes(y = patients.per.week$unscheduled, colour = "Unscheduled Care", group=1)) +
#   geom_line(aes(y = patients.per.week$women.child, colour = "Women and Child", group=1)) + 
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# ggplot(patients.per.week, aes(Year.Week)) + 
#   geom_line(aes(y = patients.per.week$elderly, colour = "Elderly Care", group=1))
# acf(patients.per.week$elderly) #nope
# ggplot(patients.per.week, aes(Year.Week)) + 
#   geom_line(aes(y = patients.per.week$medical, colour = "Medical", group=1))
# acf(patients.per.week$medical)

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
total.admissions <- total.prediction.data

# emergency admissions
emergency.admissions.wk <- filter(patient.admissions, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n()) %>%
  remove_first_and_last_n(1)

emergency.admissions.model <- arima(emergency.admissions.wk$weekly.count, order = c(1,1,1))
emergency.admissions.forecast <- forecast(emergency.admissions.model, level = c(95), h = prediction.length)
#autoplot(emergency.admissions.forecast) # to see confidence interval on prediction
emergency.admissions.prediction <- predict(emergency.admissions.model, n.ahead = prediction.length)
emergency.admissions.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year, prediction.length),
                                                          Week.of.Admission = ((last.week+1):(last.week+prediction.length)),
                                                          weekly.count = emergency.admissions.prediction$pred[1:prediction.length],
                                                          lower = emergency.admissions.forecast$lower[1:prediction.length],
                                                          upper = emergency.admissions.forecast$upper[1:prediction.length]),
                                           Week.of.Admission = sprintf("%02d", Week.of.Admission),
                                           Year.of.Admission = as.character(Year.of.Admission))
#emergency.admissions <- bind_rows(emergency.admissions.wk, emergency.admissions.prediction.data)
emergency.admissions <- emergency.admissions.prediction.data

# maternity
maternity.admissions.wk <- filter(patient.admissions, Method.of.Admission.Category == "Maternity Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n()) %>%
  remove_first_and_last_n(1)
maternity.admissions.model <- arima(maternity.admissions.wk$weekly.count, order = c(1,1,1))
maternity.admissions.forecast <- forecast(maternity.admissions.model, level = c(95), h = prediction.length)
#autoplot(maternity.admissions.forecast) # to see confidence interval on prediction
maternity.admissions.prediction <- predict(maternity.admissions.model, n.ahead = prediction.length)
maternity.admissions.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                                          Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                                          weekly.count = maternity.admissions.prediction$pred[1:prediction.length],
                                                          lower = maternity.admissions.forecast$lower[1:prediction.length],
                                                          upper = maternity.admissions.forecast$upper[1:prediction.length]),
                                               weekly.count = as.integer(weekly.count),
                                               Week.of.Admission = sprintf("%02d", Week.of.Admission),
                                               Year.of.Admission = as.character(Year.of.Admission))
#maternity.admissions <- bind_rows(maternity.admissions.wk, maternity.admissions.prediction.data)
maternity.admissions <- maternity.admissions.prediction.data

# Other admissions
other.admissions.wk <- filter(patient.admissions, Method.of.Admission.Category == "Other Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n()) %>%
  remove_first_and_last_n(1)
other.admissions.model <- arima(other.admissions.wk$weekly.count, order = c(1,1,1))
other.admissions.forecast <- forecast(other.admissions.model, level = c(95), h = prediction.length)
#autoplot(other.admissions.forecast) # to see confidence interval on prediction
other.admissions.prediction <- predict(other.admissions.model, n.ahead = prediction.length)
other.admissions.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                                          Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                                          weekly.count = other.admissions.prediction$pred[1:prediction.length],
                                                          lower = other.admissions.forecast$lower[1:prediction.length],
                                                          upper = other.admissions.forecast$upper[1:prediction.length]),
                                               weekly.count = as.integer(weekly.count),
                                               Week.of.Admission = sprintf("%02d", Week.of.Admission),
                                               Year.of.Admission = as.character(Year.of.Admission))
#other.admissions <- bind_rows(other.admissions.wk, other.admissions.prediction.data)
other.admissions <- other.admissions.prediction.data

# Elective admissions
elective.admissions.wk <- filter(patient.admissions, Method.of.Admission.Category == "Elective Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(weekly.count=n()) %>%
  remove_first_and_last_n(1)
elective.admissions.model <- arima(elective.admissions.wk$weekly.count, order = c(1,1,1))
elective.admissions.forecast <- forecast(elective.admissions.model, level = c(95), h = prediction.length)
#autoplot(elective.admissions.forecast) # to see confidence interval on prediction
elective.admissions.prediction <- predict(elective.admissions.model, n.ahead = prediction.length)
elective.admissions.prediction.data <- mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                                      Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                                      weekly.count = elective.admissions.prediction$pred[1:prediction.length],
                                                      lower = elective.admissions.forecast$lower[1:prediction.length],
                                                      upper = elective.admissions.forecast$upper[1:prediction.length]),
                                           weekly.count = as.integer(weekly.count),
                                           Week.of.Admission = sprintf("%02d", Week.of.Admission),
                                           Year.of.Admission = as.character(Year.of.Admission))
#elective.admissions <- bind_rows(elective.admissions.wk, elective.admissions.prediction.data)
elective.admissions <- elective.admissions.prediction.data

# sum all the predictions and find factor to make them equal to total
all.admissions.scaling <- bind_rows(add_column_with_value_to(emergency.admissions, "Method.of.Admission", "emergency"),
                            add_column_with_value_to(maternity.admissions, "Method.of.Admission", "maternity")) %>%
  bind_rows(add_column_with_value_to(other.admissions, "Method.of.Admission", "other")) %>%
  bind_rows(add_column_with_value_to(elective.admissions, "Method.of.Admission", "elective")) %>%
  bind_rows(add_column_with_value_to(total.admissions, "Method.of.Admission", "total"))  %>%
  dcast(Year.of.Admission + Week.of.Admission ~ Method.of.Admission, value.var = c("weekly.count")) %>%
  mutate(total.of.parts = emergency + maternity + other + elective,
         ratio.to.total = total.of.parts/total,
         "Emergency Admission" = emergency/ratio.to.total,
         "Maternity Admission" = maternity/ratio.to.total,
         "Elective Admission" = elective/ratio.to.total,
         "Other Admission" = other/ratio.to.total) 
all.admissions.ratios <- select(all.admissions.scaling, Year.of.Admission, Week.of.Admission, ratio.to.total)
all.admissions <- all.admissions.scaling %>%
  select(one_of("Year.of.Admission", "Week.of.Admission", "Emergency Admission", "Maternity Admission", "Elective Admission", "Other Admission")) %>%
  melt(id = c("Year.of.Admission", "Week.of.Admission"), variable.name = "Method.of.Admission.Category", value.name = c("weekly.count"))
all.admissions.lower <- bind_rows(add_column_with_value_to(emergency.admissions, "Method.of.Admission", "emergency"),
                                    add_column_with_value_to(maternity.admissions, "Method.of.Admission", "maternity")) %>%
  bind_rows(add_column_with_value_to(other.admissions, "Method.of.Admission", "other")) %>%
  bind_rows(add_column_with_value_to(elective.admissions, "Method.of.Admission", "elective")) %>%
  bind_rows(add_column_with_value_to(total.admissions, "Method.of.Admission", "total"))  %>%
  dcast(Year.of.Admission + Week.of.Admission ~ Method.of.Admission, value.var = c("lower")) %>%
  left_join(all.admissions.ratios) %>%
  mutate(
         "Emergency Admission" = emergency/ratio.to.total,
         "Maternity Admission" = maternity/ratio.to.total,
         "Elective Admission" = elective/ratio.to.total,
         "Other Admission" = other/ratio.to.total) %>%
  select(one_of("Year.of.Admission", "Week.of.Admission", "Emergency Admission", "Maternity Admission", "Elective Admission", "Other Admission")) %>%
  melt(id = c("Year.of.Admission", "Week.of.Admission"), variable.name = "Method.of.Admission.Category", value.name = c("lower"))
all.admissions.upper <- bind_rows(add_column_with_value_to(emergency.admissions, "Method.of.Admission", "emergency"),
                                  add_column_with_value_to(maternity.admissions, "Method.of.Admission", "maternity")) %>%
  bind_rows(add_column_with_value_to(other.admissions, "Method.of.Admission", "other")) %>%
  bind_rows(add_column_with_value_to(elective.admissions, "Method.of.Admission", "elective")) %>%
  bind_rows(add_column_with_value_to(total.admissions, "Method.of.Admission", "total"))  %>%
  dcast(Year.of.Admission + Week.of.Admission ~ Method.of.Admission, value.var = c("upper")) %>%
  left_join(all.admissions.ratios) %>%
  mutate(
    "Emergency Admission" = emergency/ratio.to.total,
    "Maternity Admission" = maternity/ratio.to.total,
    "Elective Admission" = elective/ratio.to.total,
    "Other Admission" = other/ratio.to.total) %>%
  select(one_of("Year.of.Admission", "Week.of.Admission", "Emergency Admission", "Maternity Admission", "Elective Admission", "Other Admission")) %>%
  melt(id = c("Year.of.Admission", "Week.of.Admission"), variable.name = "Method.of.Admission.Category", value.name = c("upper"))

all.admissions.confidence.interval <- full_join(all.admissions, all.admissions.lower) %>%
                                      full_join(all.admissions.upper) %>%
                                      mutate(weekly.count = round(weekly.count, digits=2),
                                             "lower (95%)" = round(lower, digits=2),
                                             "upper (95%)" = round(upper, digits=2)) %>%
                                      select(one_of("Year.of.Admission", "Week.of.Admission", "Method.of.Admission.Category", "weekly.count", "lower (95%)", "upper (95%)"))
all.admissions.full <- bind_rows(mutate(emergency.admissions.wk, Method.of.Admission.Category = "Emergency Admission"), 
                                 mutate(maternity.admissions.wk, Method.of.Admission.Category = "Maternity Admission")) %>%
                             bind_rows(mutate(elective.admissions.wk, Method.of.Admission.Category = "Elective Admission")) %>%
                             bind_rows(mutate(other.admissions.wk, Method.of.Admission.Category = "Other Admission")) %>%
  bind_rows(all.admissions.confidence.interval) %>%
  arrange(Year.of.Admission, Week.of.Admission, Method.of.Admission.Category)
all.admissions.full <- all.admissions.full[,c(1,2,4,3,5,6)]

write.csv(all.admissions.full,file=admissions.confidence.interval.path, row.names=F)

# now we need to re-separate, split up by weekday and resource pool.
# either per admission type.
# emergency admissions
## Resource pool proportions
emergency.admissions.prediction <- filter(all.admissions, Method.of.Admission.Category == "Emergency Admission")
emergency.admissions <- filter(patient.admissions, Method.of.Admission.Category == "Emergency Admission")
emergency.resource_pool.proportions <- group_by(emergency.admissions, Resource.Pool.name) %>%
  tally() %>%
  mutate(total = nrow(emergency.admissions)) %>%
  mutate(proportion = n/total) %>%
  select(Resource.Pool.name, proportion)
## weekdays
emergency.admissions.per.weekday <- group_by(emergency.admissions, Admission.Weekday) %>%
  tally() %>%
  mutate(total = nrow(emergency.admissions)) %>%
  mutate(proportion = n/total) %>%
  select(Admission.Weekday, proportion)
emergency.admissions.prediction.daily <- calculate.daily.counts.from.proportions(emergency.admissions.prediction, emergency.admissions.per.weekday) %>%
  select(date, Method.of.Admission.Category,daily.count)
## resource pools
emergency.admissions.prediction.per.day.per.resource.pool <- calculate.resource_pool.count.from.proportions(emergency.admissions.prediction.daily,emergency.resource_pool.proportions) %>%
  select(date, Method.of.Admission.Category,Resource.Pool.name, count)
## double check: sum everything and see if it adds up to week totals
emergency.weekly.totals.check <- mutate(emergency.admissions.prediction.per.day.per.resource.pool,
                                        Week.of.Admission = format(date, "%U"),
                                        Year.of.Admission = format(date, "%Y")) %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  summarize(total = sum(count)) %>%
  full_join(emergency.admissions.prediction) %>%
  select(Year.of.Admission, Week.of.Admission, total, weekly.count)
# matches.

# maternity admissions
maternity.admissions.prediction <- filter(all.admissions, Method.of.Admission.Category == "Maternity Admission")
maternity.admissions <- filter(patient.admissions, Method.of.Admission.Category == "Maternity Admission")
maternity.resource_pool.proportions <- group_by(maternity.admissions, Resource.Pool.name) %>%
  tally() %>%
  mutate(total = nrow(maternity.admissions)) %>%
  mutate(proportion = n/total) %>%
  select(Resource.Pool.name, proportion)
## weekdays
maternity.admissions.per.weekday <- group_by(maternity.admissions, Admission.Weekday) %>%
  tally() %>%
  mutate(total = nrow(maternity.admissions)) %>%
  mutate(proportion = n/total) %>%
  select(Admission.Weekday, proportion)
maternity.admissions.prediction.daily <- calculate.daily.counts.from.proportions(maternity.admissions.prediction, maternity.admissions.per.weekday) %>%
  select(date, Method.of.Admission.Category,daily.count)
## resource pools
maternity.admissions.prediction.per.day.per.resource.pool <- calculate.resource_pool.count.from.proportions(maternity.admissions.prediction.daily,maternity.resource_pool.proportions) %>%
  select(date, Method.of.Admission.Category,Resource.Pool.name, count)

# elective admissions
elective.admissions.prediction <- filter(all.admissions, Method.of.Admission.Category == "Elective Admission")
elective.admissions <- filter(patient.admissions, Method.of.Admission.Category == "Elective Admission")
elective.resource_pool.proportions <- group_by(elective.admissions, Resource.Pool.name) %>%
  tally() %>%
  mutate(total = nrow(elective.admissions)) %>%
  mutate(proportion = n/total) %>%
  select(Resource.Pool.name, proportion)
## weekdays
elective.admissions.per.weekday <- group_by(elective.admissions, Admission.Weekday) %>%
  tally() %>%
  mutate(total = nrow(elective.admissions)) %>%
  mutate(proportion = n/total) %>%
  select(Admission.Weekday, proportion)
elective.admissions.prediction.daily <- calculate.daily.counts.from.proportions(elective.admissions.prediction, elective.admissions.per.weekday) %>%
  select(date, Method.of.Admission.Category,daily.count)
## resource pools
elective.admissions.prediction.per.day.per.resource.pool <- calculate.resource_pool.count.from.proportions(elective.admissions.prediction.daily,elective.resource_pool.proportions) %>%
  select(date, Method.of.Admission.Category,Resource.Pool.name, count)

# other admissions
other.admissions.prediction <- filter(all.admissions, Method.of.Admission.Category == "Other Admission")
other.admissions <- filter(patient.admissions, Method.of.Admission.Category == "Other Admission")
other.resource_pool.proportions <- group_by(other.admissions, Resource.Pool.name) %>%
  tally() %>%
  mutate(total = nrow(other.admissions)) %>%
  mutate(proportion = n/total) %>%
  select(Resource.Pool.name, proportion)
## weekdays
other.admissions.per.weekday <- group_by(other.admissions, Admission.Weekday) %>%
  tally() %>%
  mutate(total = nrow(other.admissions)) %>%
  mutate(proportion = n/total) %>%
  select(Admission.Weekday, proportion)
other.admissions.prediction.daily <- calculate.daily.counts.from.proportions(other.admissions.prediction, other.admissions.per.weekday) %>%
  select(date, Method.of.Admission.Category,daily.count)
## resource pools
other.admissions.prediction.per.day.per.resource.pool <- calculate.resource_pool.count.from.proportions(other.admissions.prediction.daily,other.resource_pool.proportions) %>%
  select(date, Method.of.Admission.Category,Resource.Pool.name, count)

# add it all together
all.admissions.predictions <- bind_rows(emergency.admissions.prediction.per.day.per.resource.pool , maternity.admissions.prediction.per.day.per.resource.pool) %>%
  bind_rows(elective.admissions.prediction.per.day.per.resource.pool) %>%
  bind_rows(other.admissions.prediction.per.day.per.resource.pool) %>%
  rename(Date.of.Admission = date)
all.admissions.data <- group_by(patient.admissions, Date.of.Admission, Method.of.Admission.Category, Resource.Pool.name) %>%
                       summarize(count = n())
all.admissions.result <- bind_rows(all.admissions.predictions, all.admissions.data) %>%
                         arrange(Date.of.Admission)
write.csv(all.admissions.result, file=admissions.result.path, row.names=F)
