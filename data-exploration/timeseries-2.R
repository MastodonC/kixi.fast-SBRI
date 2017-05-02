require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)

# for our own data source paths
source("paths.R")

emergency.data <- read.csv(emergency.data.path, stringsAsFactors = F,na.strings=c("","NA")) %>%
  mutate(
    Date.of.Arrival = as.Date(Arrival.Date,format="%d/%m/%Y"),
    Left.DateTime = as.Date(Left.Dept.Datetime,format="%d/%m/%Y %H:%M:%S"),
    Week.of.Arrival = as.integer(format(Date.of.Arrival,format="%W")) + 1,
    Month.of.Arrival = format(Date.of.Arrival, "%m"),
    Year.of.Arrival = format(Date.of.Arrival, "%Y"),
    Year.Week = paste(Year.of.Arrival, sprintf("%02d", Week.of.Arrival), sep="-"),
    Hour.of.Arrival = hour(hm(Arrival.Time)),
    Triage.Date = as.Date(Triage.Pre.Stream.Datetime,format="%d/%m/%Y"),
    Triage.Time = strftime(as.POSIXct(Triage.Pre.Stream.Datetime,format="%d/%m/%Y %H:%M:%S"), format="%H:%M"),
    Triage.Hour = hour(hm(Triage.Time)),
    ED.Clinician.Date = as.Date(ED.Clinician.Date.and.Time...First,format="%d/%m/%Y"),
    ED.Clinician.Time = strftime(as.POSIXct(ED.Clinician.Date.and.Time...First,format="%d/%m/%Y %H:%M:%S"), format="%H:%M"),
    ED.Clinician.Hour = hour(hm(ED.Clinician.Time)),
    Date.Decision.of.Admission = as.Date(DADT.DateTime, format="%d/%m/%Y %H:%M:%S"),
    Time.Decision.of.Admission = strftime(as.POSIXct(DADT.DateTime,format="%d/%m/%Y %H:%M:%S"), format="%H:%M"),
    DateTime.Decision.of.Admission = as.POSIXct(DADT.DateTime,format="%d/%m/%Y %H:%M:%S"),
    Leave.ED.Date = as.Date(Left.Dept.Datetime,format="%d/%m/%Y"),
    Leave.ED.Time = strftime(as.POSIXct(Left.Dept.Datetime,format="%d/%m/%Y %H:%M:%S"), format="%H:%M"),
    Leave.ED.Hour = hour(hm(Leave.ED.Time))
  ) %>%
  rename(Time.of.Arrival = Arrival.Time)

# group by patient arrival
by_arrival <- group_by(emergency.data, H.C.Encrypted, Year.of.Arrival, Month.of.Arrival, 
                       Week.of.Arrival, Date.of.Arrival, Time.of.Arrival, Hour.of.Arrival, 
                       Date.Decision.of.Admission, Time.Decision.of.Admission, DateTime.Decision.of.Admission)

## Patients arrivals
# Arrivals per week
arrivals.per.week <- by_arrival %>%
                     group_by(Year.of.Arrival, Month.of.Arrival, Week.of.Arrival) %>%
                     arrange(Year.of.Arrival, Month.of.Arrival, Week.of.Arrival) %>%
                     summarise(weekly.arrivals=n())

# Arrivals per day
arrivals.per.day <- by_arrival %>%
                     group_by(Year.of.Arrival, Month.of.Arrival, Week.of.Arrival, Date.of.Arrival) %>%
                     arrange(Year.of.Arrival, Month.of.Arrival, Week.of.Arrival, Date.of.Arrival) %>%
                     summarise(daily.arrivals=n())

# Arrivals per hour
arrivals.per.hour <- by_arrival %>%
                     group_by(Year.of.Arrival, Month.of.Arrival, 
                              Week.of.Arrival, Date.of.Arrival, Hour.of.Arrival) %>%
                     arrange(Year.of.Arrival, Month.of.Arrival, 
                             Week.of.Arrival, Date.of.Arrival, Hour.of.Arrival) %>%
                     summarise(hourlly.arrivals=n())

## Patients at triage (per category)

## Patients first seeing a clinician

## Patients leaving ED 