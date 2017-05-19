# Counts per Resource pool at any time
require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
require(forecast)

# for our own data source paths
source("paths.R")

prediction.length <- 20 # in days

all_na_to_0 <- function(d) {
  d[is.na(d)] <- 0
  d
}

calculate.resource_pool.count.from.proportions <- function(daily.data, proportions.data) {
  bind_rows(mutate(daily.data, Resource.Pool.name = "Elderly Care"),
            mutate(daily.data, Resource.Pool.name = "Medical"),
            mutate(daily.data, Resource.Pool.name = "Palliative Care"),
            mutate(daily.data, Resource.Pool.name = "Surgical"),
            mutate(daily.data, Resource.Pool.name = "Unscheduled Care"),
            mutate(daily.data, Resource.Pool.name = "Women and Child")) %>%
    inner_join(proportions.data) %>%
    mutate(count = daily.count * proportion) %>%
    select(date,count,Resource.Pool.name)
}

calculate.daily.counts.from.proportions <- function(weekly.data, proportions.data) {
  bind_rows(mutate(weekly.data, day = 1),
            mutate(weekly.data, day = 2),
            mutate(weekly.data, day = 3),
            mutate(weekly.data, day = 4),
            mutate(weekly.data, day = 5),
            mutate(weekly.data, day = 6),
            mutate(weekly.data, day = 7)) %>%
    arrange(Year,Week, day) %>%
    mutate(date = as.Date(paste(Year, Week, (day-1), sep="-"), format="%Y-%U-%w"))  %>%
    mutate(date = if_else(is.na(date),
                          as.Date(paste((as.integer(Year)+1), 0, (day-1), sep="-"), format="%Y-%U-%w"),
                          date))  %>% # remove the last days of the last week
    mutate(Admission.Weekday = weekdays(date, abbreviate = FALSE))  %>%
    inner_join(proportions.data) %>%
    mutate(daily.count = weekly.count * proportion)
}

remove_first_and_last_n <- function(dataframe, n) {
  dataframe[-c(1:n,(nrow(dataframe) - n):nrow(dataframe)),]
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
patient.data.with.exit.resource_pool <- left_join(patient.data, resource_pool.specialties, by=c("Specialty.on.Exit.of.Ward" = "PAS.name"))
check.change.of.resource_pool <- full_join(patient.data.with.exit.resource_pool, patient.data.with.resource_pool, by=c("H.C.Encrypted", "Age", "Sex", "Date.of.Admission.With.Time", "Source.of.Admission", "Method.of.Admission", "Transferred.From", "Method.of.Admission.Category", "Date.Time.Medically.Assessed.Ready", "Reason.for.Discharge.Delay", "Date.of.Discharge.with.Time", "Method.of.Discharge", "Destination.Discharge.Description", "Transferred.To", "Ward.Name", "Date.of.Ward.Entry.with.Time", "Mode.of.Entry.to.Ward", "Specialty.Description.on.Ward.Entry", "Date.of.Ward.Exit.with.Time", "Mode.of.Exit.from.Ward", "Specialty.on.Exit.of.Ward", "Ward.Episode.Number", "Date.of.Admission", "Date.of.Discharge", "Date.of.Ward.Entry", "Date.of.Ward.Exit","Week.of.Ward.Entry", "Year.of.Ward.Entry", "Week.of.Ward.Exit", "Year.of.Ward.Exit", "length.of.stay")) %>%
  filter(Resource.Pool.name.x != Resource.Pool.name.y) %>%
  select(Resource.Pool.name.x, Resource.Pool.name.y)


## 1. Research to figure out methodology
#########################################

# patient.with.one.resource_pool <- full_join(patient.data.with.exit.resource_pool, patient.data.with.resource_pool, by=c("H.C.Encrypted", "Age", "Sex", "Date.of.Admission.With.Time", "Source.of.Admission", "Method.of.Admission", "Transferred.From", "Method.of.Admission.Category", "Date.Time.Medically.Assessed.Ready", "Reason.for.Discharge.Delay", "Date.of.Discharge.with.Time", "Method.of.Discharge", "Destination.Discharge.Description", "Transferred.To", "Ward.Name", "Date.of.Ward.Entry.with.Time", "Mode.of.Entry.to.Ward", "Specialty.Description.on.Ward.Entry", "Date.of.Ward.Exit.with.Time", "Mode.of.Exit.from.Ward", "Specialty.on.Exit.of.Ward", "Ward.Episode.Number", "Date.of.Admission", "Date.of.Discharge", "Date.of.Ward.Entry", "Date.of.Ward.Exit","Week.of.Ward.Entry", "Year.of.Ward.Entry", "Week.of.Ward.Exit", "Year.of.Ward.Exit", "length.of.stay")) %>%
#   filter(Resource.Pool.name.x == Resource.Pool.name.y) %>%
#   select(one_of("H.C.Encrypted", "Age", "Sex", "Date.of.Admission.With.Time", "Source.of.Admission", "Method.of.Admission", "Transferred.From", "Method.of.Admission.Category", "Date.Time.Medically.Assessed.Ready", "Reason.for.Discharge.Delay", "Date.of.Discharge.with.Time", "Method.of.Discharge", "Destination.Discharge.Description", "Transferred.To", "Ward.Name", "Date.of.Ward.Entry.with.Time", "Mode.of.Entry.to.Ward", "Specialty.Description.on.Ward.Entry", "Date.of.Ward.Exit.with.Time", "Mode.of.Exit.from.Ward", "Specialty.on.Exit.of.Ward", "Ward.Episode.Number", "Date.of.Admission", "Date.of.Discharge", "Date.of.Ward.Entry", "Date.of.Ward.Exit", "Week.of.Ward.Entry", "Year.of.Ward.Entry", "Week.of.Ward.Exit", "Year.of.Ward.Exit", "length.of.stay", "Resource.Pool.name.x")) %>%
#   rename(Resource.Pool.name = Resource.Pool.name.x)
# # only count for date of entry, and exclude exit date to avoid counting twice. that works too.
# # group by resource pool and date of entry:
# patient.entry.per.resource_pool <- group_by(patient.data.with.resource_pool, Date.of.Ward.Entry, Resource.Pool.name) %>%
#                                    summarize(entry.count = n()) %>%
#                                    rename(Date = Date.of.Ward.Entry)
# patient.exit.per.resource_pool <- filter(patient.data.with.resource_pool, is.na(Date.of.Ward.Exit) == 0) %>% # remove all records with no date of exit for this, assume they haven't left
#                                   group_by(Date.of.Ward.Exit, Resource.Pool.name) %>%
#                                   summarize(exit.count = n()) %>%
#                                   rename(Date = Date.of.Ward.Exit)
# patients.entry.exit.per.resource_pool <- left_join(patient.entry.per.resource_pool, patient.exit.per.resource_pool) %>%
#                                          all_na_to_0()

# elderly care
# elderly.care.counts <- filter(patients.entry.exit.per.resource_pool, Resource.Pool.name == "Elderly Care") %>%
#                        ungroup() %>%
#                        arrange(Date) %>%
#                        mutate(cumulative.entries = cumsum(entry.count),
#                               cumulative.exits = cumsum(exit.count),
#                               count = cumulative.entries - cumulative.exits) #%>%
                       # select(Date, count)
# heuristic to choose the number of records to drop: first row where it goes down again, last row where it goes up.
# or check length of stay for this resource pool
# elderly.care.lengths <- quantile(filter(patient.data.with.resource_pool, Resource.Pool.name == "Elderly Care" & is.na(length.of.stay) == 0)$length.of.stay)[4] %>%
#                         as.integer()
# assumption: using the 75% x 1.5 gives us most people (as in most people that were in the ward at the time have left)
# elderly.care.counts <- elderly.care.counts[-1:as.integer(-1.5*elderly.care.lengths),]
# 
# ggplot(data = elderly.care.counts, aes(x=Date, y=count, group = 1)) +
#   geom_line() +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) # upward trend? is that possible?
# plot.ts(elderly.care.counts$count)
# plot.ts(diff(elderly.care.counts$count))
# acf(elderly.care.counts$count)
# acf(diff(elderly.care.counts$count)) # not really
# elderly.care.counts.prediction.model <- arima(elderly.care.counts$count, order = c(1,1,1))

# ward counts
# patient.entry.per.ward <- group_by(patient.data, Date.of.Ward.Entry, Ward.Name) %>%
#   summarize(entry.count = n()) %>%
#   rename(Date = Date.of.Ward.Entry)
# patient.exit.per.ward <- filter(patient.data, is.na(Date.of.Ward.Exit) == 0) %>% # remove all records with no date of exit for this, assume they haven't left
#   group_by(Date.of.Ward.Exit, Ward.Name) %>%
#   summarize(exit.count = n()) %>%
#   rename(Date = Date.of.Ward.Exit)
# patients.entry.exit.per.ward <- left_join(patient.entry.per.ward, patient.exit.per.ward) %>%
#   all_na_to_0
# 
# ward.counts <-  ungroup(patients.entry.exit.per.ward) %>%
#   group_by(Ward.Name) %>%
#   arrange(Ward.Name,Date) %>%
#   mutate(cumulative.entries = cumsum(entry.count),
#          cumulative.exits = cumsum(exit.count),
#          ward.count = cumulative.entries - cumulative.exits) # %>%
#   #select(Ward.Name, Date, count)
# 
# ward.counts.horizontal <- dcast(ward.counts, Date ~ Ward.Name) %>%
#                           all_na_to_0()
# ward.counts.horizontal <- cbind(ward.counts.horizontal, rowSums(ward.counts.horizontal[,2:ncol(ward.counts.horizontal)])) 
# acf(ward.counts.horizontal[,3])
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
# patient.ward.proportions <- left_join(ward.counts, patient.counts, by=c("Date")) %>%
#                             filter(!(Ward.Name %in% ward.ignore)) %>%
#                             mutate(proportion = ward.count/count)
# df1 = 25 - 1
# df2 = 740 - (25 - 1)
# ggplot(patient.ward.proportions, aes(Ward.Name, proportion)) +
#   geom_boxplot() +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# not 100% conclusive: some have pretty wide margins
# check per resource pool
# one would expect a quasi-match per resource pool
# elderly.care.data <- filter(patient.data.with.resource_pool, Resource.Pool.name == "Elderly Care") %>%
#   filter(!(Ward.Name %in% ward.ignore))
# elderly.care.entry.ward <- filter(elderly.care.data, is.na(Date.of.Ward.Exit) == 0) %>%
#                            group_by(Ward.Name, Date.of.Ward.Entry) %>%
#                            summarize(entry.count = n()) %>%
#                            rename(Date = Date.of.Ward.Entry)
# 
# elderly.care.exit.ward <- filter(elderly.care.data, is.na(Date.of.Ward.Exit) == 0) %>% # remove all records with no date of exit for this, assume they haven't left
#   group_by(Ward.Name, Date.of.Ward.Exit) %>%
#   summarize(exit.count = n()) %>%
#   rename(Date = Date.of.Ward.Exit)
# elderly.care.per.ward <- left_join(elderly.care.entry.ward, elderly.care.exit.ward) %>%
#   all_na_to_0()
# 
# elderly.care.per.ward.counts <- ungroup(elderly.care.per.ward) %>%
#                                 arrange(Ward.Name, Date) %>%
#                                 group_by(Ward.Name) %>%
#                                 mutate(cumulative.entries = cumsum(entry.count),
#                                        cumulative.exits = cumsum(exit.count),
#                                        ward.count = cumulative.entries - cumulative.exits) #%>%
#                                 #select(Date, Ward.Name, ward.count)
# 
# stroke.ward.counts <- filter(elderly.care.per.ward, Ward.Name == "A1 Stroke/Medical Ward") 
# ggplot(data=stroke.ward.counts, aes(Date)) +
#   geom_line(aes(y=entry.count, colour="entry")) +
#   geom_line(aes(y=exit.count, colour="exit"))
#                                 
# elderly.care.ward.proportions <- left_join(elderly.care.per.ward.counts, elderly.care.counts, by=c("Date"))  %>%
#   filter(is.na(count) == 0) %>% # gets rid of the records at the start which are probably invalid
#   filter(!(Ward.Name %in% ward.ignore))  %>%
#   mutate(proportion = ward.count/count)
# ggplot(elderly.care.ward.proportions, aes(Ward.Name, proportion)) +
#   geom_boxplot() +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# ggplot(elderly.care.per.ward.counts, aes(Ward.Name, ward.count)) +
#   geom_boxplot() +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# stroke.ward <- filter(elderly.care.per.ward.counts, Ward.Name == "A1 Stroke/Medical Ward") %>% arrange(Date)
# plot.ts(stroke.ward$ward.count) # goes up beyond bed capacity!
# # do people stay longer?
# stroke.mean.length_of_stay <- filter(elderly.care.data, Ward.Name == "A1 Stroke/Medical Ward") %>%
#                               group_by(Date.of.Ward.Exit) %>%
#                               summarize(avg.length.of.stay = mean(length.of.stay))
# 
# # not growing particularly
# 
# # how to check whether patients have overlapping patient records:
# #  same patient has several records at the same time in the same ward? 
# stroke.ward.patients.several <- filter(elderly.care.data, Ward.Name == "A1 Stroke/Medical Ward") %>%
#                         group_by(H.C.Encrypted, Date.of.Admission) %>%
#                         summarize(count = n())
# stroke.ward.patients.overlap <- filter(elderly.care.data, Ward.Name == "A1 Stroke/Medical Ward") %>%
#                         left_join(stroke.ward.patients.several) %>%
#                         filter(count > 1) %>%
#                         arrange(H.C.Encrypted, Date.of.Ward.Entry) %>%
#                         select(H.C.Encrypted, Date.of.Ward.Entry, Date.of.Ward.Exit, count) %>%
#                         group_by(H.C.Encrypted) %>%
#                         mutate(next.ward.entry = lead(Date.of.Ward.Entry,1)) %>%
#                         filter(Date.of.Ward.Exit > next.ward.entry)
# # no overlapping as far as I can see
# # checking patients
# stroke.ward.patients <- filter(elderly.care.data, Ward.Name == "A1 Stroke/Medical Ward")  %>%
#                         arrange(Date.of.Ward.Entry)
# stroke.ward.lengths <- arrange(stroke.ward.patients, Date.of.Ward.Exit) %>%
#                         mutate(length.of.stay = as.integer(Date.of.Ward.Exit - Date.of.Ward.Entry)) %>%
#                         group_by(Date.of.Ward.Exit) %>%
#                         summarize(avg.length.of.stay = mean(length.of.stay))
# plot.ts(stroke.ward.lengths$avg.length.of.stay)
# ggplot(data = stroke.ward.patients, aes(x = Date.of.Ward.Exit, y = avg.length.of.stay)) + geom_point() + geom_smooth()
# stroke.ward.exceeding <- filter(stroke.ward.patients, Date.of.Ward.Exit > Date.of.Discharge) # none

# NOTE: if we decide to go with proportions, normalizing things so we have proportions that add up to one

# probably no predictions for this one, so recallibration
closed.or.not <- filter(patient.data, Ward.Name == "B5 Closed From 03/10/16") %>%
  arrange(Date.of.Ward.Entry) %>%
  select(Date.of.Ward.Entry) # couple in december?

# remove resource pools from the equation is the answer, also full_join instead of left_join

## 2. Actual results
######################
# steps:
# 1. predictions on a weekly level
# 2. scale predictions so they sum to total prediction (TODO)
# 3. split up in weekdays and resource pools (TODO)

#### 2.1 predictions on a weekly level

relevant.patient.data <- filter(patient.data.with.resource_pool, !(Ward.Name %in% ward.ignore))
patient.entry.ward <- group_by(relevant.patient.data, Ward.Name, Year.of.Ward.Entry, Week.of.Ward.Entry) %>%
  summarize(entry.count = n()) %>%
  rename(Year = Year.of.Ward.Entry,
         Week = Week.of.Ward.Entry)
patient.exit.ward <- filter(relevant.patient.data, is.na(Date.of.Ward.Exit) == 0) %>%
  group_by(Ward.Name, Year.of.Ward.Exit, Week.of.Ward.Exit) %>%
  summarize(exit.count = n()) %>%
  rename(Year = Year.of.Ward.Exit,
         Week = Week.of.Ward.Exit)
patient.per.ward <- full_join(patient.entry.ward, patient.exit.ward) %>%
  all_na_to_0()

relevant.patient.per.ward.counts <- ungroup(patient.per.ward) %>%
  arrange(Ward.Name, Year, Week) %>%
  group_by(Ward.Name) %>%
  mutate(cumulative.entries = cumsum(entry.count),
         cumulative.exits = cumsum(exit.count),
         ward.count = cumulative.entries - cumulative.exits)

patient.per.ward.count.horizontal <- dcast(relevant.patient.per.ward.counts,
                                           Year + Week ~ Ward.Name, value.var = c("ward.count"))
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="A1 Stroke/Medical Ward"] <- "a1.medical"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="A2 Paediatric Ward"] <- "a2.paediatric"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="A3 Medical Ward"] <- "a3.medical"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Antrim A4 Medical"] <- "a4.medical"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Antrim A&E Dept."] <- "ae.dept"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Antrim B3 Ward"] <- "b3.ward"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Antrim C1 Gynae"] <- "c1.gynae"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Antrim C2 Maternity Unit"] <- "c2.maternity"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Antrim (C)Intensive Care"] <- "intensive.care"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Antrim Level B Cardiac Unit"] <- "b.cardiac"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Assessment Medical Unit 1"] <- "assessment.1"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="B4 - General Medicine"] <- "b4.general"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="B5b -Use Ward Code Eau For B5b"] <- "b5b.eau"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="C2 Cotted Ward"] <- "c2.cotted"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="C3 Gastro & General Medicine"] <- "c3.gastro"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="C4 Elective Unit"] <- "c4.elective"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="C5 Gen Surgery"] <- "c5.surgery"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="C6 Gen Surgery"] <- "c6.gen"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="C7"] <- "c7"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Discharge Lounge Antrim"] <- "discharge.lounge"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Eldery Acute Unit"] <- "elderly.acute"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Genm/Endo/Diab - B2"] <- "b2"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Macmillan Unit At Antrim"] <- "macmillan"
names(patient.per.ward.count.horizontal)[names(patient.per.ward.count.horizontal)=="Observation Unit - Antrim"] <- "obs"


lag_na_values <- function(d) {
    mutate(d,
    a1.medical = if_else(is.na(a1.medical) & is.na(lag(a1.medical)) == 0,
                         lag(a1.medical),
                         a1.medical),
    a2.paediatric = if_else(is.na(a2.paediatric) & is.na(lag(a2.paediatric)) == 0,
                            lag(a2.paediatric),
                            a2.paediatric),
    a3.medical = if_else(is.na(a3.medical) & is.na(lag(a3.medical)) == 0,
                         lag(a3.medical),
                         a3.medical),
    a4.medical = if_else(is.na(a4.medical) & is.na(lag(a4.medical)) == 0,
                         lag(a4.medical),
                         a4.medical),
    ae.dept = if_else(is.na(ae.dept) & is.na(lag(ae.dept)) == 0,
                      lag(ae.dept),
                      ae.dept),
    b3.ward = if_else(is.na(b3.ward) & is.na(lag(b3.ward)) == 0,
                      lag(b3.ward),
                      b3.ward),
    c1.gynae = if_else(is.na(c1.gynae) & is.na(lag(c1.gynae)) == 0,
                       lag(c1.gynae),
                       c1.gynae),
    c2.maternity = if_else(is.na(c2.maternity) & is.na(lag(c2.maternity)) == 0,
                           lag(c2.maternity),
                           c2.maternity),
    intensive.care = if_else(is.na(intensive.care) & is.na(lag(intensive.care)) == 0,
                             lag(intensive.care),
                             intensive.care),
    b.cardiac = if_else(is.na(b.cardiac) & is.na(lag(b.cardiac)) == 0,
                        lag(b.cardiac),
                        b.cardiac),
    assessment.1 = if_else(is.na(assessment.1) & is.na(lag(assessment.1)) == 0,
                           lag(assessment.1),
                           assessment.1),
    b4.general = if_else(is.na(b4.general) & is.na(lag(b4.general)) == 0,
                         lag(b4.general),
                         b4.general),
    b5b.eau = if_else(is.na(b5b.eau) & is.na(lag(b5b.eau)) == 0,
                      lag(b5b.eau),
                      b5b.eau),
    c2.cotted = if_else(is.na(c2.cotted) & is.na(lag(c2.cotted)) == 0,
                        lag(c2.cotted),
                        c2.cotted),
    c3.gastro = if_else(is.na(c3.gastro) & is.na(lag(c3.gastro)) == 0,
                        lag(c3.gastro),
                        c3.gastro),
    c4.elective = if_else(is.na(c4.elective) & is.na(lag(c4.elective)) == 0,
                          lag(c4.elective),
                          c4.elective),
    c5.surgery = if_else(is.na(c5.surgery) & is.na(lag(c5.surgery)) == 0,
                         lag(c5.surgery),
                         c5.surgery),
    c6.gen = if_else(is.na(c6.gen) & is.na(lag(c6.gen)) == 0,
                     lag(c6.gen),
                     c6.gen),
    c7 = if_else(is.na(c7) & is.na(lag(c7)) == 0,
                 lag(c7),
                 c7),
    discharge.lounge = if_else(is.na(discharge.lounge) & is.na(lag(discharge.lounge) == 0),
                               lag(discharge.lounge),
                               discharge.lounge),
    elderly.acute = if_else(is.na(elderly.acute) & is.na(lag(elderly.acute) == 0),
                            lag(elderly.acute),
                            elderly.acute),
    b2 = if_else(is.na(b2) & is.na(lag(b2) == 0),
                 lag(b2),
                 b2),
    macmillan = if_else(is.na(macmillan) & is.na(lag(macmillan) == 0),
                        lag(macmillan),
                        macmillan),
    obs = if_else(is.na(obs) & is.na(lag(obs) == 0),
                  lag(obs),
                  obs)) }

# fill NA values: should always be the previous one, because NA means there was no change   
# let's arbitrarily put first values that are NA to 0
for (i in c(1:ncol(patient.per.ward.count.horizontal))) {
  if (is.na(patient.per.ward.count.horizontal[1,i])) {
    patient.per.ward.count.horizontal[1,i] <- 0
  }
}


patient.per.ward.count.horizontal <- remove_first_and_last_n(patient.per.ward.count.horizontal, 2)
last.data.date <- patient.per.ward.count.horizontal[nrow(patient.per.ward.count.horizontal),]$Date
last.year <- max(patient.per.ward.count.horizontal$Year)
last.week <- as.integer(last(patient.per.ward.count.horizontal$Week))


calculate_weekly_ward_prediction <- function(ward.column, ward.name, arima.order, prediction.length) {
  ward.model <- arima(ward.column, order = arima.order)
  print(ward.model)
  ward.forecast <- forecast(ward.model, level = c(95), h = prediction.length)
  ward.prediction <- predict(ward.model, n.ahead = prediction.length)
  ward.pred.data <- data.frame(Year = rep(last.year,prediction.length),
                               Week = (last.week+1):(last.week+prediction.length))
  ward.pred.data[,ward.name] <- ward.prediction$pred[1:prediction.length]
  ward.pred.data <- mutate(ward.pred.data,
                           Week = sprintf("%02d", Week),
                           Year = as.character(Year))  
  # autoplot(ward.forecast)
  ward.pred.data
}

#initialize
pred.data <- mutate(data.frame(Year = rep(last.year,prediction.length),
                               Week = (last.week+1):(last.week+prediction.length)),
                    Week = sprintf("%02d", Week),
                    Year = as.character(Year))  
# could be difficult to b5b.eau, elderly.acute: separate treatment?
for (ward in c("a1.medical","a2.paediatric","a3.medical","a4.medical","ae.dept","b3.ward","c1.gynae","c2.maternity", 
               "intensive.care","b.cardiac","assessment.1","b4.general","c2.cotted","c3.gastro", "b5b.eau",
               "c4.elective","c5.surgery","c6.gen","c7","b2","macmillan","obs", "discharge.lounge")) {
  print(ward)
  pred.data <- full_join(pred.data, calculate_weekly_ward_prediction(patient.per.ward.count.horizontal[,c(ward)], ward, c(1,0,0), prediction.length))
}
# special treatment for elderly acute because freak point at end
pred.data <- full_join(pred.data, calculate_weekly_ward_prediction(patient.per.ward.count.horizontal[-c(nrow(patient.per.ward.count.horizontal)),c("elderly.acute")], "elderly.acute", c(1,0,0), prediction.length + 1))
# a1.medical, a2.paediatric, a3.medical, a4.medical, ae.dept, b3.ward, c1.gynae, c2.maternity, 
# intensive.care, b.cardiac, assessment.1, b4.general, b5b.eau, c2.cotted, c3.gastro, 
# c4.elective, c5.surgery, c6.gen, c7, discharge.lounge, elderly.acute, b2, macmillan, obs

# 2.2. scale predictions so they sum to total prediction

# 2.3. split up in weekdays and resource pools

a1.medical.number <- nrow(filter(patient.data.with.resource_pool, Ward.Name == "A1 Stroke/Medical Ward"))
a1.medical.resource_pool.proportions <- filter(patient.data.with.resource_pool, Ward.Name == "A1 Stroke/Medical Ward") %>%
                                        group_by(Resource.Pool.name) %>%
                                        summarize(proportion = n()/a1.medical.number)
a1.medical.pred.data.with.resource_pools <- calculate.resource_pool.count.from.proportions(a1.medical.pred.data, a1.medical.resource_pool.proportions)


