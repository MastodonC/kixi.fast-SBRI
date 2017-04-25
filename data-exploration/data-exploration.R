require(dplyr)
require(reshape2)
require(ggplot2)
require(cowplot)
require(tidyr)
require(lubridate)

# for our own data source paths
source("paths.R")

## READING DATA
# Emergency data
emergency.data <- read.csv(emergency.data.path, stringsAsFactors = F,na.strings=c("","NA")) %>%
                  mutate(
                    Date.of.Admission = as.Date(Arrival.Date,format="%d/%m/%Y"),
                    Left.DateTime = as.Date(Left.Dept.Datetime,format="%d/%m/%Y %H:%M:%S"),
                    Week.of.Admission = as.integer(format(Date.of.Admission,format="%W")) + 1,
                    Month.of.Admission = format(Date.of.Admission, "%m"),
                    Year.of.Admission = format(Date.of.Admission, "%Y"),
                    Hour.of.Admission = hour(hm(Arrival.Time))
                  ) %>%
                  rename(Time.of.Admission = Arrival.Time)

## PAS data
patient.data <- read.csv(patient.data.path, stringsAsFactors = F,na.strings=c("","NA")) %>%
  mutate(
    Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    Date.of.Discharge = as.Date(Date.of.Discharge.with.Time,format="%d-%b-%Y %H:%M"),
    Time.of.Admission = strftime(as.POSIXct(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"), format="%H:%M"),
    age.num = as.integer(Age),
    Sex = as.factor(Sex),
    Method.of.Admission.Category = as.factor(Method.of.Admission.Category)
  )

## Data Exploration

#plotting arrivals per day
arrivals.per.day <- emergency.data %>%
                    group_by(Date.of.Admission) %>%
                    arrange(Date.of.Admission) %>%
                    summarize(Count = n())
ggplot(data = arrivals.per.day, aes(x = Date.of.Admission, y = Count)) + geom_point() + geom_smooth() + ggtitle("Arrivals per day")
plot.ts(arrivals.per.day$Count)
plot.ts(diff(arrivals.per.day$Count))
acf(diff(arrivals.per.day$Count))

arrivals.per.week <- group_by(emergency.data, Year.of.Admission,Week.of.Admission) %>%
                     arrange(Year.of.Admission, Week.of.Admission) %>%
                     summarize(count = n())
plot.ts(arrivals.per.week$count)

# Arrivals per weekday
emergency.data$Arrival.weekdays <- weekdays(emergency.data$Date.of.Admission, abbreviate = FALSE)

arrivals.per.weekday <- emergency.data %>%
                        group_by(Arrival.weekdays) %>%
                        summarize(Count = n())

arrivals.per.weekdays.and.triage <- emergency.data %>%
                                    group_by(Arrival.weekdays, Triage.Category.Description...Last) %>%
                                    summarize(Count = n())

ggplot(data=arrivals.per.weekday, aes(x=Arrival.weekdays, y=Count)) + geom_bar(stat="identity")

ggplot(data=arrivals.per.weekdays.and.triage, aes(x=Arrival.weekdays, y=Count))
+ geom_bar(aes(fill=Triage.Category.Description...Last), stat="identity")


# Arrivals per month
emergency.data$Arrival.months <- months(emergency.data$Date.of.Admission, abbreviate = FALSE)

arrivals.per.month <- emergency.data %>%
                      group_by(Arrival.months) %>%
                      summarize(Count = n())

ggplot(data=arrivals.per.month, aes(x=Arrival.months, y=Count)) + geom_bar(stat="identity")

# try to find timeseries arrival per month
arrivals.per.month <- mutate(emergency.data, month.year = paste(Year.of.Admission, Month.of.Admission, sep="-")) %>%
  group_by(Year.of.Admission,Month.of.Admission, month.year) %>%
  arrange(Year.of.Admission, Month.of.Admission) %>%
  summarize(count = n())
ggplot(data = arrivals.per.month, aes(x=month.year, y=count, group = 1)) + geom_line()
plot.ts(diff(arrivals.per.month$count))

acf(arrivals.per.month$count)
# looks like diffing is required
acf(diff(arrivals.per.month$count))

# Arrivals per quarter
emergency.data$Arrival.quarters <- quarters(emergency.data$Date.of.Admission, abbreviate = FALSE)

arrivals.per.quarter <- emergency.data %>%
                        group_by(Arrival.quarters) %>%
                        summarize(Count = n())

ggplot(data=arrivals.per.quarter, aes(x=Arrival.quarters, y=Count)) + geom_bar(stat="identity")


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
by_stay <- group_by(patient.data, H.C.Encrypted,age.num,Sex,Date.of.Admission,Time.of.Admission,Date.of.Discharge,Method.of.Admission.Category,Method.of.Discharge)
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
                    summarize(Count = n())
ggplot(data = patient.arrivals.per.day, aes(x = Date.of.Admission, y = Count)) + geom_point() + geom_smooth() + ggtitle("Arrivals per day")
# let's see if a histogram with varying bin size will give us more information (week? month?)
ggplot(data=patient.hospital.stays, aes(patient.hospital.stays$Date.of.Admission)) + geom_histogram(binwidth=30)
# not conclusive
# ask Sunny about detecting seasonality (Fourrier transform, smoothing ...)

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

# we can have more than one admission per day!
emergency.data.duplicate.admissions <- group_by(emergency.data, H.C.Encrypted, Date.of.Admission) %>%
  summarize(count = n()) %>%
  filter(count > 1)
duplicate.id.1 <- emergency.data.duplicate.admissions[1,]$H.C.Encrypted
duplicate1 <- filter(emergency.data, H.C.Encrypted == duplicate.id.1)
# do corresponding stays have same time stamp?  should we use hour?
duplicate.patient <- filter(patient.data, H.C.Encrypted == duplicate.id.1)

patient.hospital.without.ids <- filter(patient.hospital.stays, is.na(H.C.Encrypted))
patient.hospital.with.ids <- filter(patient.hospital.stays, is.na(H.C.Encrypted) == F)
patient.merged <- left_join(patient.hospital.with.ids, emergency.data)

daily.admissions <- select(patient.merged, Date.of.Admission,Method.of.Admission.Category) %>%
  gather(Date.of.Admission,Method.of.Admission.Category)
# patterns with method of admission counted in?
# pretty sure this could be done better
emergency.admissions <- filter(patient.merged, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Date.of.Admission) %>%
  summarise(em.count=n())
maternity.admissions <- filter(patient.merged, Method.of.Admission.Category == "Maternity Admission") %>%
  group_by(Date.of.Admission) %>%
  summarise(mat.count=n())
other.admissions <- filter(patient.merged, Method.of.Admission.Category == "Other Admission") %>%
  group_by(Date.of.Admission) %>%
  summarise(other.count=n())
elective.admissions <- filter(patient.merged, Method.of.Admission.Category == "Elective Admission") %>%
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

# plot per hour
ed.per.hour <- group_by(emergency.data, Hour.of.Admission) %>% summarize(count=n())
ggplot(data=emergency.data, aes(emergency.data$Hour.of.Admission)) + geom_histogram(binwidth=1)

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
