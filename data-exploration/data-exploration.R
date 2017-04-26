require(dplyr)
require(reshape2)
require(ggplot2)
require(cowplot)
require(tidyr)
require(lubridate)
require(sqldf)

# for our own data source paths
source("paths.R")

## READING DATA
# Emergency data
emergency.data <- read.csv(emergency.data.path, stringsAsFactors = F,na.strings=c("","NA")) %>%
                  mutate(
                    Date.of.Arrival = as.Date(Arrival.Date,format="%d/%m/%Y"),
                    Left.DateTime = as.Date(Left.Dept.Datetime,format="%d/%m/%Y %H:%M:%S"),
                    Week.of.Arrival = as.integer(format(Date.of.Arrival,format="%W")) + 1,
                    Month.of.Arrival = format(Date.of.Arrival, "%m"),
                    Year.of.Arrival = format(Date.of.Arrival, "%Y"),
                    Year.Week = paste(Year.of.Arrival, sprintf("%02d", Week.of.Arrival), sep="-"),
                    Hour.of.Arrival = hour(hm(Arrival.Time)),
                    Date.Decision.of.Admission = as.Date(DADT.DateTime, format="%d/%m/%Y %H:%M:%S"),
                    Time.Decision.of.Admission = strftime(as.POSIXct(DADT.DateTime,format="%d/%m/%Y %H:%M:%S"), format="%H:%M"),
                    DateTime.Decision.of.Admission = as.POSIXct(DADT.DateTime,format="%d/%m/%Y %H:%M:%S")
                  ) %>%
                  rename(Time.of.Arrival = Arrival.Time)

## PAS data
patient.data <- read.csv(patient.data.path, stringsAsFactors = F,na.strings=c("","NA")) %>%
  mutate(
    Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    Week.of.Admission = as.integer(format(Date.of.Admission,format="%W")) + 1,
    Year.of.Admission = format(Date.of.Admission, "%Y"),
    Year.Week = paste(Year.of.Admission, sprintf("%02d", Week.of.Admission), sep="-"),
    Day.Admission = format(Date.of.Admission, "%d"),
    Month.Admission = format(Date.of.Admission, "%m"),
    Date.of.Discharge = as.Date(Date.of.Discharge.with.Time,format="%d-%b-%Y %H:%M"),
    DateTime.of.Admission = as.POSIXct(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    Time.of.Admission = strftime(as.POSIXct(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"), format="%H:%M"),
    age.num = as.integer(Age),
    Sex = as.factor(Sex),
    Method.of.Admission.Category = as.factor(Method.of.Admission.Category)
  )

# what happened in january?
first.week.2017 <- filter(patient.data, Year.Week == "2017-01")
first.week.2016 <- filter(patient.data, Year.Week == "2016-01")
normal.week <- filter(patient.data, Year.Week == "2016-13")
first.week.2017 <- filter(patient.data, Year.Week == "2017-01")
em.first.week.2016 <- filter(emergency.data, Year.Week == "2016-01")
em.first.week.2017 <- filter(emergency.data, Year.Week == "2017-01")

## Data Exploration

#plotting arrivals per day
arrivals.per.day <- emergency.data %>%
                    group_by(Date.of.Arrival) %>%
                    arrange(Date.of.Arrival) %>%
                    summarize(Count = n())
ggplot(data = arrivals.per.day, aes(x = Date.of.Arrival, y = Count)) + geom_point() + geom_smooth() + ggtitle("Arrivals per day")
plot.ts(arrivals.per.day$Count)
plot.ts(diff(arrivals.per.day$Count))
acf(diff(arrivals.per.day$Count))

arrivals.per.week <- group_by(emergency.data, Year.of.Arrival,Week.of.Arrival) %>%
                     arrange(Year.of.Arrival, Week.of.Arrival) %>%
                     summarize(count = n())
plot.ts(arrivals.per.week$count)

# Arrivals per weekday
emergency.data$Arrival.weekdays <- weekdays(emergency.data$Date.of.Arrival, abbreviate = FALSE)
weekday.list <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
arrivals.per.weekday <- emergency.data %>%
                        group_by(Arrival.weekdays) %>%
                        summarize(Count = n()) %>%
                        mutate(weekday.ordered = factor(Arrival.weekdays, levels = weekday.list)) %>%
                        arrange(weekday.ordered)

arrivals.per.weekdays.and.triage <- emergency.data %>%
                                    group_by(Arrival.weekdays, Triage.Category.Description...Last) %>%
                                    summarize(Count = n()) %>%
                                    mutate(weekday.ordered = factor(Arrival.weekdays, levels = weekday.list)) %>%
                                    arrange(weekday.ordered)

ggplot(data=arrivals.per.weekday, aes(x=weekday.ordered, y=Count)) + geom_bar(stat="identity")

ggplot(arrivals.per.weekdays.and.triage, aes(x=weekday.ordered, y=Count)) + geom_col(aes(fill=Triage.Category.Description...Last))

# Arrivals per month
emergency.data$Arrival.months <- months(emergency.data$Date.of.Arrival, abbreviate = FALSE)

arrivals.per.month <- emergency.data %>%
                      group_by(Arrival.months) %>%
                      summarize(Count = n())

arrivals.per.month.per.triage <- emergency.data %>%
                                 group_by(Arrival.months, Triage.Category.Description...Last) %>%
                                 summarize(Count = n())

ggplot(data=arrivals.per.month, aes(x=Arrival.months, y=Count)) + geom_bar(stat="identity")

ggplot(arrivals.per.month.per.triage, aes(x=Arrival.months, y=Count)) + geom_col(aes(fill=Triage.Category.Description...Last))

# try to find timeseries arrival per month
arrivals.per.month <- mutate(emergency.data, month.year = paste(Year.of.Arrival, Month.of.Arrival, sep="-")) %>%
  group_by(Year.of.Arrival,Month.of.Arrival, month.year) %>%
  arrange(Year.of.Arrival, Month.of.Arrival) %>%
  summarize(count = n())
ggplot(data = arrivals.per.month, aes(x=month.year, y=count, group = 1)) + geom_line()
plot.ts(diff(arrivals.per.month$count))

acf(arrivals.per.month$count)
# looks like diffing is required
acf(diff(arrivals.per.month$count))

# Arrivals per quarter
emergency.data$Arrival.quarters <- quarters(emergency.data$Date.of.Arrival, abbreviate = FALSE)

arrivals.per.quarter <- emergency.data %>%
                        group_by(Arrival.quarters) %>%
                        summarize(Count = n())

arrivals.per.quarter.per.triage <- emergency.data %>%
                                   group_by(Arrival.quarters, Triage.Category.Description...Last) %>%
                                   summarize(Count = n())

ggplot(data=arrivals.per.quarter, aes(x=Arrival.quarters, y=Count)) + geom_bar(stat="identity")

ggplot(arrivals.per.quarter.per.triage, aes(x=Arrival.quarters, y=Count)) + geom_col(aes(fill=Triage.Category.Description...Last))

# Departures pr weekday
#emergency.data$Departure.weekdays <- if(typeof(emergency.data$Left.DateTime) == "Date"){
  #weekdays(emergency.data$Left.DateTime, abbreviate = FALSE)} else {
  #"NA"}
emergency.data$Departure.weekdays <- weekdays(emergency.data$Left.DateTime, abbreviate = FALSE)

departures.per.weekday <- emergency.data %>%
                          group_by(Departure.weekdays) %>%
                          summarize(Count = n()) %>%
                          filter(Departure.weekdays != "NA")

ggplot(data=departures.per.weekday, aes(x=Departure.weekdays, y=Count)) + geom_bar(stat="identity")

# Departures per month
emergency.data$Departure.months <- months(emergency.data$Left.DateTime, abbreviate = FALSE)

departures.per.month <- emergency.data %>%
                        group_by(Departure.months) %>%
                        summarize(Count = n()) %>%
                        filter(Departure.months != "NA")

ggplot(data=departures.per.month, aes(x=Departure.months, y=Count)) + geom_bar(stat="identity")

# Departures per quarter
emergency.data$Departure.quarters <- quarters(emergency.data$Left.DateTime, abbreviate = FALSE)

departures.per.quarter <- emergency.data %>%
                          group_by(Departure.quarters) %>%
                          summarize(Count = n()) %>%
                          filter(Departure.quarters != "QNA")

ggplot(data=departures.per.quarter, aes(x=Departure.quarters, y=Count)) + geom_bar(stat="identity")

# they all have coherent dates

# group by hospital stay (patient, same day admitted, same day discharged)
by_stay <- group_by(patient.data, H.C.Encrypted,age.num,Sex,Date.of.Admission,Time.of.Admission,DateTime.of.Admission, Date.of.Discharge,Method.of.Admission.Category,Method.of.Discharge, Year.Week, Day.Admission, Month.Admission)
patient.hospital.stays <- summarize(by_stay,
                              count = n(),
                              distinct.wards = n_distinct(Ward.Name)
                            ) %>%
                            mutate(length.of.stay = as.integer(Date.of.Discharge - Date.of.Admission))

# summary stats
summary(patient.hospital.stays)


# readmissions
patients.total <- group_by(patient.data,H.C.Encrypted) %>%
  summarize(records=n())
patient.recurrent <- group_by(patient.hospital.stays, H.C.Encrypted) %>%
                     summarize(stays.count = n()) %>%
                     filter(stays.count > 1)
patient.not.recurrent <- group_by(patient.hospital.stays, H.C.Encrypted) %>%
  summarize(stays.count = n()) %>%
  filter(stays.count <= 1)
# 28% of patients are readmitted
# TODO: check how many times

# genders check
gender.counts <- group_by(patient.hospital.stays, Sex) %>% summarize(count = n())
# check numbers per age group
ggplot(data=patient.hospital.stays, aes(patient.hospital.stays$age.num)) + geom_histogram(binwidth=5)
ggplot(data=patient.hospital.stays, aes(patient.hospital.stays$age.num, fill=Sex)) + geom_histogram(binwidth=5, alpha=.5)

patient.arrivals.per.day <- patient.hospital.stays %>%
                    group_by(Date.of.Admission) %>%
                    arrange(Date.of.Admission) %>%
                    summarize(arrival.count = n()) %>%
                    mutate(Date = Date.of.Admission)
ggplot(data = patient.arrivals.per.day, aes(x = Date.of.Admission, y = Count)) + geom_point() + geom_smooth() + ggtitle("Arrivals per day")
# let's see if a histogram with varying bin size will give us more information (week? month?)
ggplot(data=patient.hospital.stays, aes(patient.hospital.stays$Date.of.Admission)) + geom_histogram(binwidth=30)
# not conclusive

# how many patients in hospital at any one time, using patient.data
# rolling count, using arrivals and departures - arrivals on day: patient.arrivals.per.day
# departures per day
patient.departures.per.day <- group_by(patient.hospital.stays, Date.of.Discharge) %>%
                              arrange(Date.of.Discharge) %>%
                              summarize(departure.count = n()) %>%
                              mutate(Date = Date.of.Discharge)
# merge arrivals and departures
patient.arrivals.and.departures <- full_join(patient.arrivals.per.day, patient.departures.per.day)
# rolling count?


patient.arrivals.per.month <- patient.hospital.stays %>%
  mutate(
    month = months(Date.of.Admission),
    year = format(Date.of.Admission,"%Y")
  ) %>%
  group_by(month,year) %>%
  summarize(count = n())
ggplot(data=patient.arrivals.per.month, aes(x = month, y = count)) + geom_point()

# length of stay
# histogram
ggplot(data=patient.hospital.stays, aes(patient.hospital.stays$length.of.stay)) + geom_histogram()
# scatterplot age - sex - not conclusive
pairs(~length.of.stay+age.num+Sex+Method.of.Admission.Category,data=patient.hospital.stays,
      main="Simple Scatterplot Matrix")

# method of admission
patient.stays.per.method.of.admission <- group_by(patient.hospital.stays, Method.of.Admission.Category) %>%
                                          summarize(count = n(),
                                                    mean.length.of.stay = mean(length.of.stay,na.rm = TRUE),
                                                    sd.length.of.stay = sd(length.of.stay, na.rm = TRUE)
                                                    )

patient.stays.per.method.of.discharge <- group_by(patient.hospital.stays, Method.of.Discharge) %>%
                                          summarize(count=n())

# merging patient data with emergency data

# we can have more than one Arrival per day!
emergency.data.duplicate.admissions <- group_by(emergency.data, H.C.Encrypted, Date.of.Arrival) %>%
  summarize(count = n()) %>%
  filter(count > 1)
duplicate.id.1 <- emergency.data.duplicate.admissions[1,]$H.C.Encrypted
duplicate1 <- filter(emergency.data, H.C.Encrypted == duplicate.id.1)
# do corresponding stays have same time stamp?  should we use hour?
duplicate.patient <- filter(patient.data, H.C.Encrypted == duplicate.id.1)

patient.hospital.without.ids <- filter(patient.hospital.stays, is.na(H.C.Encrypted))
patient.hospital.with.ids <- filter(patient.hospital.stays, is.na(H.C.Encrypted) == F)
# these are the patients that are admitted
# !! Arrival is not admission - arrival could be a little before admission (or not followed by admission)
#patient.merged <- left_join(patient.hospital.with.ids, emergency.data)
# making data in table and then using sqldf to merge
#patient.merged <- sqldf('select patient.hospital.with.ids.*, emergency.data.* from patient.hospital left outer join emergency.data on patient.hospital.with.ids.H.C.Encrypted = emergency.data.H.C.Encrypted and patient.hospital.with.ids.DateTime.of.Admission <= emergency.data.DateTime.Decision.of.Admission + 14400')

# patterns with method of admission counted in?
# pretty sure this could be done better
emergency.admissions <- filter(patient.hospital.stays, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Date.of.Admission) %>%
  summarise(em.count=n())
maternity.admissions <- filter(patient.hospital.stays, Method.of.Admission.Category == "Maternity Admission") %>%
  group_by(Date.of.Admission) %>%
  summarise(mat.count=n())
other.admissions <- filter(patient.hospital.stays, Method.of.Admission.Category == "Other Admission") %>%
  group_by(Date.of.Admission) %>%
  summarise(other.count=n())
elective.admissions <- filter(patient.hospital.stays, Method.of.Admission.Category == "Elective Admission") %>%
  group_by(Date.of.Admission) %>%
  summarise(elective.count=n())
all.admissions <- full_join(emergency.admissions,maternity.admissions) %>%
  full_join(other.admissions) %>%
  full_join(elective.admissions)

ggplot(data=all.admissions, aes(Date.of.Admission)) +
  geom_line(aes(y=em.count, colour="emergency")) +
  geom_line(aes(y=mat.count, colour="maternity")) +
  geom_line(aes(y=other.count, colour="other")) +
  geom_line(aes(y=elective.count, colour="elective"))
plot.ts(all.admissions$em.count)
plot.ts(diff(all.admissions$em.count))

elective.admissions.oddity <- filter(all.admissions, elective.count > 30)

# check patterns per week instead, daily is a little too noisy
emergency.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Year.Week) %>%
  summarise(em.count=n())
maternity.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Maternity Admission") %>%
  group_by(Year.Week) %>%
  summarise(mat.count=n())
other.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Other Admission") %>%
  group_by(Year.Week) %>%
  summarise(other.count=n())
elective.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Elective Admission") %>%
  group_by(Year.Week) %>%
  summarise(elective.count=n())
all.admissions.wk <- full_join(emergency.admissions.wk,maternity.admissions.wk) %>%
  full_join(other.admissions.wk) %>%
  full_join(elective.admissions.wk)
# check if this gets all the data
other.types <- filter(patient.hospital.stays, Method.of.Admission.Category != "Emergency Admission" &&
                                              Method.of.Admission.Category != "Maternity Admission" &&
                                              Method.of.Admission.Category != "Other Admission" &&
                                              Method.of.Admission.Category != "Elective Admission")
# all good
ggplot(data=all.admissions.wk, aes(Year.Week, group=1)) +
  geom_line(aes(y=em.count, colour="emergency")) +
  geom_line(aes(y=mat.count, colour="maternity")) +
  geom_line(aes(y=other.count, colour="other")) +
  geom_line(aes(y=elective.count, colour="elective"))
# emergency and others have different levels, so plot separately
ggplot(data=all.admissions.wk, aes(Year.Week, group=1)) +
  geom_line(aes(y=em.count, colour="emergency")) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

plot.ts(all.admissions.wk$em.count)
plot.ts(diff(all.admissions.wk$em.count))
acf(all.admissions.wk$em.count)
acf(diff(all.admissions.wk$em.count))

# peak around 5 for emergency, see if there's a day-of-month pattern
emergency.grouped <- filter(patient.hospital.stays, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Day.Admission, Month.Admission)
emergency.result <- summarise(emergency.grouped, total.count = n())
for(month in distinct(patient.hospital.stays$Month.Admission)) {
  m <- filter(emergency.grouped, Month.Admission == month) %>% 
       summarise(month = n())
  emergency.result <- left_join(emergency.result, m)
}


ggplot(data=emergency.admissions, aes(Day.Admission))


ggplot(data=all.admissions.wk, aes(Year.Week, group=1)) +
  geom_line(aes(y=mat.count, colour="maternity")) +
  geom_line(aes(y=other.count, colour="other")) +
  geom_line(aes(y=elective.count, colour="elective")) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


# plot per hour
ed.per.hour <- group_by(emergency.data, Hour.of.Admission) %>% summarize(count=n())
ggplot(data=emergency.data, aes(emergency.data$Hour.of.Admission)) + geom_histogram(binwidth=1)
ggplot(data=ed.per.hour, aes(x=Hour.of.Admission, y=count)) + geom_bar(stat="identity")

# how many wards does a person go through in one visit, and which?
wards <- left_join(patient.data,patient.hospital.stays) %>% select(H.C.Encrypted, Ward.Name,count, distinct.wards, length.of.stay)

plot1 <- ggplot(data=wards, aes(wards$count)) + geom_histogram(binwidth=1)
plot2 <- ggplot(data=wards, aes(wards$distinct.wards)) + geom_histogram()
plot_grid(plot1, plot2, align='h', labels=c('count', 'distinct count'))
# patient data ordered by chronology of visit (Ward.Episode.Number)
patient.data.ordered <- arrange(patient.data,H.C.Encrypted,Date.of.Admission,Time.of.Admission,Ward.Episode.Number) %>%
  select(H.C.Encrypted,Ward.Name,Specialty.Description.on.Ward.Entry, Ward.Episode.Number,Mode.of.Exit.from.Ward)

# TODO:
# possibly output per hour to have profiles, also weekdays and weekends



# anomalous data
# 130 rows with NA lenght of stay
no.length.of.stay <- filter(patient.hospital.stays, is.na(length.of.stay))
# = patients have not left
# no patient id! 369 in original data, 229 in hospital stays
no.id.patient.data <- filter(patient.data, is.na(H.C.Encrypted))
no.id.stays <- patient.hospital.stays[is.na(patient.data$H.C.Encrypted),]

# 1 patient, 93 PAS records for 1 stay
patient1 <- patient.hospital.stays[patient.hospital.stays$count == 93.0,]
patient1.stay <- patient.data[patient.data$H.C.Encrypted == patient1$H.C.Encrypted,]
# TODO get better stats on numbers of transitions

# patient stay 244 days
patient2 <- filter(patient.hospital.stays, length.of.stay == 244.0)
patient2.stay <- filter(patient.data, H.C.Encrypted == patient2$H.C.Encrypted)
