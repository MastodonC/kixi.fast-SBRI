require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)

# for our own data source paths
source("paths.R")

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

ggplot(patients.per.day, aes(Date.of.Admission)) + 
  geom_line(aes(y = patients.per.day$elderly, colour = "Elderly Care")) +
  geom_line(aes(y = patients.per.day$medical, colour = "Medical")) +
  geom_line(aes(y = patients.per.day$palliative, colour = "Palliative Care")) +
  geom_line(aes(y = patients.per.day$surgical, colour = "Surgical")) +
  geom_line(aes(y = patients.per.day$unscheduled, colour = "Unscheduled Care")) +
  geom_line(aes(y = patients.per.day$women.child, colour = "Women and Child"))

acf(patients.per.day$elderly)

patients.per.week <- tally(group_by(patient.admissions, Year.Week, Resource.Pool.name)) %>%
  dcast(Year.Week ~ Resource.Pool.name) %>%
  setNames(c("Year.Week","elderly","medical","palliative","surgical","unscheduled","women.child"))

ggplot(patients.per.week, aes(Year.Week)) + 
  geom_line(aes(y = patients.per.week$elderly, colour = "Elderly Care", group=1)) +
  geom_line(aes(y = patients.per.week$medical, colour = "Medical", group=1)) +
  geom_line(aes(y = patients.per.week$palliative, colour = "Palliative Care", group=1)) +
  geom_line(aes(y = patients.per.week$surgical, colour = "Surgical", group=1)) +
  geom_line(aes(y = patients.per.week$unscheduled, colour = "Unscheduled Care", group=1)) +
  geom_line(aes(y = patients.per.week$women.child, colour = "Women and Child", group=1)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
