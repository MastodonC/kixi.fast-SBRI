# Counts per Resource pool at any time
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


weekday.list <- c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

patient.data <- read.csv(patient.data.path, stringsAsFactors = F, na.strings=c("","NA")) %>%
  mutate(
    Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    Date.of.Discharge = as.Date(Date.of.Discharge.with.Time,format="%d-%b-%Y %H:%M"),
    Date.of.Ward.Entry = as.Date(Date.of.Ward.Entry.with.Time,format="%d-%b-%Y %H:%M"),
    Date.of.Ward.Exit = as.Date(Date.of.Ward.Exit.with.Time,format="%d-%b-%Y %H:%M"),
    Week.of.Ward.Entry = format(Date.of.Ward.Entry, "%U"),
    Year.of.Ward.Entry = format(Date.of.Ward.Entry, "%Y"),
    Week.of.Ward.Exit = format(Date.of.Ward.Exit, "%U"),
    Year.of.Ward.Exit = format(Date.of.Ward.Exit, "%Y"),
    length.of.stay = as.integer(Date.of.Ward.Exit - Date.of.Ward.Entry),
    age.group = as.character(cut(Age, breaks=c(-1,10,20,30,40,50,60,70,80,90,Inf),
                                 labels=c('0-10', '10-20', '20-30', '30-40', '40-50', '50-60', '60-70', '70-80', '80-90', 'Over 90')))
    )

# reconciliation to find resource pool (DUMMY FOR NOW)
##! Assumption: that we can use specialty on entry to determine resource pool (and not specialty on exit)
resource_pool.specialties <- read.csv(resource_pool.specialties.path, stringsAsFactors = F,na.strings=c("","NA"))
patient.data.with.resource_pool <- left_join(patient.data, resource_pool.specialties, by=c("Specialty.Description.on.Ward.Entry" = "PAS.name"))

# only count for date of entry, and exclude exit date to avoid counting twice. that works too.
# group by resource pool and date of entry:
patient.entry.per.resource_pool <- group_by(patient.data.with.resource_pool, Date.of.Ward.Entry, Resource.Pool.name) %>%
                                   summarize(entry.count = n()) %>%
                                   rename(Date = Date.of.Ward.Entry)
patient.exit.per.resource_pool <- filter(patient.data.with.resource_pool, is.na(Date.of.Ward.Exit) == 0) %>% # remove all records with no date of exit for this, assume they haven't left
                                  group_by(Date.of.Ward.Exit, Resource.Pool.name) %>%
                                  summarize(exit.count = n()) %>%
                                  rename(Date = Date.of.Ward.Exit)
patients.entry.exit.per.resource_pool <- left_join(patient.entry.per.resource_pool, patient.exit.per.resource_pool) %>%
                                         all_na_to_0()

# elderly care
elderly.care.counts <- filter(patients.entry.exit.per.resource_pool, Resource.Pool.name == "Elderly Care") %>%
                       ungroup() %>%
                       arrange(Date) %>%
                       mutate(cumulative.entries = cumsum(entry.count),
                              cumulative.exits = cumsum(exit.count),
                              count = cumulative.entries - cumulative.exits) %>%
                       select(Date, count)
# heuristic to choose the number of records to drop: first row where it goes down again, last row where it goes up.
# or check length of stay for this resource pool
elderly.care.lengths <- quantile(filter(patient.data.with.resource_pool, Resource.Pool.name == "Elderly Care" & is.na(length.of.stay) == 0)$length.of.stay)[4] %>%
                        as.integer()
# assumption: using the 75% x 1.5 gives us most people (as in most people that were in the ward at the time have left)
elderly.care.counts <- elderly.care.counts[-1:as.integer(-1.5*elderly.care.lengths),]

ggplot(data = elderly.care.counts, aes(x=Date, y=count, group = 1)) +
  geom_line() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) # upward trend? is that possible?
plot.ts(diff(elderly.care.counts$count))
acf(elderly.care.counts$count)
acf(diff(elderly.care.counts$count)) # not really
elderly.care.counts.prediction.model <- arima(elderly.care.counts$count, order = c(1,1,1))

# ward counts
patient.entry.per.ward <- group_by(patient.data, Date.of.Ward.Entry, Ward.Name) %>%
  summarize(entry.count = n()) %>%
  rename(Date = Date.of.Ward.Entry)
patient.exit.per.ward <- filter(patient.data, is.na(Date.of.Ward.Exit) == 0) %>% # remove all records with no date of exit for this, assume they haven't left
  group_by(Date.of.Ward.Exit, Ward.Name) %>%
  summarize(exit.count = n()) %>%
  rename(Date = Date.of.Ward.Exit)
patients.entry.exit.per.ward <- left_join(patient.entry.per.ward, patient.exit.per.ward) %>%
  all_na_to_0

ward.counts <-  ungroup(patients.entry.exit.per.ward) %>%
  group_by(Ward.Name) %>%
  arrange(Ward.Name,Date) %>%
  mutate(cumulative.entries = cumsum(entry.count),
         cumulative.exits = cumsum(exit.count),
         ward.count = cumulative.entries - cumulative.exits) # %>%
  select(Ward.Name, Date, count)

ward.counts.horizontal <- dcast(ward.counts, Date ~ Ward.Name) %>%
                          all_na_to_0()
ward.counts.horizontal <- cbind(ward.counts.horizontal, rowSums(ward.counts.horizontal[,2:ncol(ward.counts.horizontal)])) %>%
  rename("rowSums(ward.counts.horizontal[, 2:ncol(ward.counts.horizontal)])" = total.patients)
acf(ward.counts.horizontal[,3])
# check totals per day in hospital: one record per patient per visit contains all info we need
patient.visits <- filter(patient.data, Mode.of.Entry.to.Ward == "ADM")
patient.admissions.per.day <- patient.visits %>%
                             group_by(Date.of.Admission) %>%
                             arrange(Date.of.Admission) %>%
                             summarize(arrival.count = n()) %>%
                             mutate(Date = Date.of.Admission)
patient.discharges.per.day <- group_by(patient.visits, Date.of.Discharge) %>%
                              arrange(Date.of.Discharge) %>%
                              summarize(departure.count = n()) %>%
                              mutate(Date = Date.of.Discharge)
patient.admissions.and.discharges <- left_join(patient.admissions.per.day, patient.discharges.per.day)
patient.counts <- mutate(patient.admissions.and.discharges,                        
                         cumulative.arrivals = cumsum(arrival.count),
                         cumulative.departures = cumsum(departure.count),
                         count = cumulative.arrivals - cumulative.departures) %>%
                  select(Date, count)
plot.ts(patient.counts$count)
ward.patient.count.check <- full_join(patient.counts, ward.counts.horizontal)


# check daily proportions on wards
# "ignore" list:
# 
ward.ignore <- c("Antrim Dpu/Endoscopy Unit", "Antrim Induction Unit", "Antrim (C) Neonatal Unit", "Fetal Maternal Assessment Unit", "A4h Haemodialysis Unit", "Antrim Childrens Ambulatory", "Antrim Special Care Baby Int.", "Chemotherapy Unit Laurel House",
                 "Antrim Outpatients Department", "A3h Medical", "Trolley Waits In Day Procedure", "Recovery Area Antrim", "Renal Unit Antrim Hospital", "Operating Theatres", "Closed Do Not Use", "Cardiac Procedure Room Level B", "Day Surgery Unit",
                 "Ant Short Stay Ward Ambulatory", "Acute Assessment Unit", "Short Stay Wrd Closed05/07/13","A1a Ward Rheumatology", "A2 Assessment Unit", "A3tr Trolley Wait Holding Area", "Accident And Emergency Obs", "A&E Trolley Waits", "C3 Trolley Waits Holding Area",
                 "B5 Closed From 03/10/16")
patient.ward.proportions <- left_join(ward.counts, patient.counts, by=c("Date")) %>%
                            filter(!(Ward.Name %in% ward.ignore)) %>%
                            mutate(proportion = ward.count/count)
# df1 = 25 - 1
# df2 = 740 - (25 - 1)
ggplot(patient.ward.proportions, aes(Ward.Name, proportion)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# not 100% conclusive: some have pretty wide margins
# check per resource pool
# one would expect a quasi-match per resource pool
elderly.care.data <- filter(patient.data.with.resource_pool, Resource.Pool.name == "Elderly Care")
elderly.care.entry.ward <- group_by(elderly.care.data, Date.of.Ward.Entry, Ward.Name) %>%
                           summarize(entry.count = n()) %>%
                           rename(Date = Date.of.Ward.Entry)

elderly.care.exit.ward <- filter(elderly.care.data, is.na(Date.of.Ward.Exit) == 0) %>% # remove all records with no date of exit for this, assume they haven't left
  group_by(Date.of.Ward.Exit, Ward.Name) %>%
  summarize(exit.count = n()) %>%
  rename(Date = Date.of.Ward.Exit)
elderly.care.per.ward <- left_join(elderly.care.entry.ward, elderly.care.exit.ward) %>%
  all_na_to_0()
elderly.care.per.ward.counts <- mutate(elderly.care.per.ward,                        
                                cumulative.entries = cumsum(entry.count),
                                cumulative.exits = cumsum(exit.count),
                                count = cumulative.entries - cumulative.exits) %>%
  select(Date, count)
# NOTE: if we decide to go with proportions, normalizing things so we have proportions that add up to one

# probably no predictions for this one, so recallibration
closed.or.not <- filter(patient.data, Ward.Name == "B5 Closed From 03/10/16") %>%
  arrange(Date.of.Ward.Entry) %>%
  select(Date.of.Ward.Entry) # couple in december?



