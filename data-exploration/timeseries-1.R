require(dplyr)
require(reshape2)
require(ggplot2)

# for our own data source paths
source("paths.R")

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
    Method.of.Admission.Category = as.factor(Method.of.Admission.Category),
    age.group = as.character(cut(Age, breaks=c(-1,10,20,30,40,50,60,70,80,90,Inf),
                                 labels=c('0-10', '10-20', '20-30', '30-40', '40-50', '50-60', '60-70', '70-80', '80-90', 'Over 90')))
  )

# group by hospital stay (patient, same day admitted, same day discharged)
by_stay <- group_by(patient.data, H.C.Encrypted,age.num, age.group,Sex,Date.of.Admission,Time.of.Admission,DateTime.of.Admission, Date.of.Discharge,Method.of.Admission.Category,Method.of.Discharge, Year.Week, Day.Admission, Week.of.Admission, Month.Admission, Year.of.Admission)
patient.hospital.stays <- summarize(by_stay,
                                    count = n(),
                                    distinct.wards = n_distinct(Ward.Name)) %>%
  mutate(length.of.stay = as.integer(Date.of.Discharge - Date.of.Admission))


### Emergency admissions prediction

emergency.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Emergency Admission") %>%
  group_by(Year.of.Admission, Week.of.Admission) %>%
  arrange(Year.of.Admission, Week.of.Admission) %>%
  summarise(em.count=n())

last.year <- max(emergency.admissions.wk$Year.of.Admission)
last.week <- last(emergency.admissions.wk$Week.of.Admission)
prediction.length <- 20

em.model <- arima(emergency.admissions.wk$em.count, order = c(1,1,1))
em.prediction <-predict(em.model,n.ahead=prediction.length)
em.admissions.pred <- em.prediction$pred[1:prediction.length]
# we just use the same year for this situation
# together in data set
em.pred.data <-mutate(data.frame(Year.of.Admission = rep(last.year,prediction.length),
                                 Week.of.Admission = (last.week+1):(last.week+prediction.length),
                                 em.count = em.admissions.pred),
                         em.count = as.integer(em.count),
                         Week.of.Admission = as.integer(Week.of.Admission),
                         Year.of.Admission = as.character(Year.of.Admission))

emergency.admissions.final <- emergency.admissions.wk
emergency.admissions.nrow <- nrow(emergency.admissions.wk)
for (i in 1:prediction.length) {
  emergency.admissions.final[emergency.admissions.nrow+i,] <- em.pred.data[i,]
}

em.admissions.wk.result <- rbind(emergency.admissions.wk, em.pred.data[1,])
tst <- rbind(em.admissions.wk.result, em.pred.data[2,])

### Maternity admissions

maternity.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Maternity Admission") %>%
  group_by(Year.Week) %>%
  summarise(mat.count=n())

### Other admissions

other.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Other Admission") %>%
  group_by(Year.Week) %>%
  summarise(other.count=n())

### Elective admissions

elective.admissions.wk <- filter(patient.hospital.stays, Method.of.Admission.Category == "Elective Admission") %>%
  group_by(Year.Week) %>%
  summarise(elective.count=n())

### TODO:
# predictions at weekly level
# probabilities per day: split over day of week (getting daily counts)
# probabilities per hour: split over hour per day


# which things are we going to predict