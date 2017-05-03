require(dplyr)
require(reshape2)
require(ggplot2)

# for our own data source paths
source("paths.R")

resource_pool.specialties <- read.csv(resource_pool.specialties.path, stringsAsFactors = F,na.strings=c("","NA"))

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

patient.data.specialties <- unique(patient.data$Specialty.Description.on.Ward.Entry)
patient.data.by.specialty <- group_by(patient.data, Specialty.Description.on.Ward.Entry) %>%
                             summarise(count = n()) %>%
                             arrange(count)
one.patient <- filter(patient.data.by.specialty, count == 1)
ten.patient <- filter(patient.data.by.specialty, count < 10)
ggplot(data=patient.data.by.specialty, aes(x= reorder(Specialty.Description.on.Ward.Entry,-count), y=count)) +
  geom_bar(position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

patient.data.same.specialty <- filter(patient.data, Specialty.Description.on.Ward.Entry == Specialty.on.Exit.of.Ward)
patient.data.different.specialty <- filter(patient.data, Specialty.Description.on.Ward.Entry != Specialty.on.Exit.of.Ward)

