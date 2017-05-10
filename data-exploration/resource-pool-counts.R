# Counts per Resource pool at any time
require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
require(forecast)

# for our own data source paths
source("paths.R")

weekday.list <- c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

patient.data <- read.csv(patient.data.path, stringsAsFactors = F, na.strings=c("","NA")) %>%
  mutate(
    Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
    Date.of.Ward.Entry = as.Date(Date.of.Ward.Entry.with.Time,format="%d-%b-%Y %H:%M"),
    Date.of.Ward.Exit = as.Date(Date.of.Ward.Exit.with.Time,format="%d-%b-%Y %H:%M"),
    Week.of.Ward.Entry = format(Date.of.Ward.Entry, "%U"),
    Year.of.Ward.Entry = format(Date.of.Ward.Entry, "%Y"),
    Week.of.Ward.Exit = format(Date.of.Ward.Exit, "%U"),
    Year.of.Ward.Exit = format(Date.of.Ward.Exit, "%Y")
    )

# reconciliation to find resource pool (DUMMY FOR NOW)
##! Assumption: that we can use specialty on entry to determine resource pool (and not specialty on exit)
resource_pool.specialties <- read.csv(resource_pool.specialties.path, stringsAsFactors = F,na.strings=c("","NA"))
patient.data.with.resource_pool <- left_join(patient.data, resource_pool.specialties, by=c("Specialty.Description.on.Ward.Entry" = "PAS.name"))

# consolidate: if a patient has stayed in the same resource pool even while changing wards, merge rows
# so from: entry exit pool
#          1     2    Medical
#          2     3    Medical
# 
#      to: 1     3    Medical
# or: only count for date of entry, and exclude exit date to avoid counting twice. that works too.
# group by resource pool and date of entry:
patient.entry.per.resource_pool <- group_by(patient.data.with.resource_pool, Date.of.Ward.Entry, Resource.Pool.name) %>%
                                   summarize(entry.count = n()) %>%
                                   rename(Date = Date.of.Ward.Entry)
patient.exit.per.resource_pool <- group_by(patient.data.with.resource_pool, Date.of.Ward.Exit, Resource.Pool.name) %>%
  summarize(exit.count = n()) %>%
  rename(Date = Date.of.Ward.Exit)
patients.entry.exit.per.resource_pool <- full_join(patient.entry.per.resource_pool, patient.exit.per.resource_pool)
