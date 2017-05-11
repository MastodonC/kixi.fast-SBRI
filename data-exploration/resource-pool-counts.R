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
    Date.of.Ward.Entry = as.Date(Date.of.Ward.Entry.with.Time,format="%d-%b-%Y %H:%M"),
    Date.of.Ward.Exit = as.Date(Date.of.Ward.Exit.with.Time,format="%d-%b-%Y %H:%M"),
    Week.of.Ward.Entry = format(Date.of.Ward.Entry, "%U"),
    Year.of.Ward.Entry = format(Date.of.Ward.Entry, "%Y"),
    Week.of.Ward.Exit = format(Date.of.Ward.Exit, "%U"),
    Year.of.Ward.Exit = format(Date.of.Ward.Exit, "%Y"),
    length.of.stay = as.integer(Date.of.Ward.Exit - Date.of.Ward.Entry)
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
                              count = cumulative.entries - cumulative.exits) #%>%
                       #select(Date, count)
# heuristic to choose the number of records to drop: first row where it goes down again, last row where it goes up.
# or check length of stay for this resource pool
elderly.care.lengths <- quantile(filter(patient.data.with.resource_pool, Resource.Pool.name == "Elderly Care" & is.na(length.of.stay) == 0)$length.of.stay)[4] %>%
                        as.integer()
# assumption: using the 75% x 1.5 gives us most people
#elderly.care.counts <- head(elderly.care.counts, -1.5*elderly.care.lengths)
