require(dplyr)
require(reshape2)
require(ggplot2)

# Emergency data
emergency.data <- read.csv("~/SBRI/20170228_SBRIB_AAH_ED_Dataset_Encry.csv", stringsAsFactors = F,na.strings=c("","NA"))
# remove nils
emergency.data <- emergency.data %>%
                  mutate(
                    Arrival.Date.formatted = as.Date(Arrival.Date,format="%d/%m/%Y"),
                    Left.DateTime = as.Date(Left.Dept.Datetime,format="%d/%m/%Y %H:%M:%S")
                  )


#plotting arrivals per day
arrivals.per.day <- emergency.data %>%
                    group_by(Arrival.Date.formatted) %>%
                    arrange(Arrival.Date.formatted) %>%
                    summarize(Count = n())
ggplot(data = arrivals.per.day, aes(x = Arrival.Date.formatted, y = Count)) + geom_point() + geom_smooth() + ggtitle("Arrivals per day")

## PAS data
# 17-FEB-2015 15:22
pharmacy.data <- read.csv("~/SBRI/20170228_Pharmacy_SBRIB_AntrimWardDataset_Encry.csv", stringsAsFactors = F,na.strings=c("","NA")) %>%
                 mutate(
                   Date.of.Admission = as.Date(Date.of.Admission.With.Time,format="%d-%b-%Y %H:%M"),
                   Date.of.Discharge = as.Date(Date.of.Discharge.with.Time,format="%d-%b-%Y %H:%M"),
                   age.num = as.integer(Age),
                   Sex = as.factor(Sex),
                   Method.of.Admission.Category = as.factor(Method.of.Admission.Category)
                 )



# group by hospital stay (patient, same day admitted, same day discharged)
by_stay <- group_by(pharmacy.data, H.C.Encrypted,age.num,Sex,Date.of.Admission,Date.of.Discharge,Method.of.Admission.Category)
pharmacy.hospital.stays <- summarize(by_stay,
                              count = n()
                            ) %>%
                            mutate(length.of.stay = as.integer(Date.of.Discharge - Date.of.Admission))

# summary stats
summary(pharmacy.hospital.stays)

# genders check
gender.counts <- group_by(pharmacy.hospital.stays, Sex) %>% summarize(count = n())
# check numbers per age group
ggplot(data=pharmacy.hospital.stays, aes(pharmacy.hospital.stays$age.num)) + geom_histogram(binwidth=5)
ggplot(data=pharmacy.hospital.stays, aes(pharmacy.hospital.stays$age.num, fill=Sex)) + geom_histogram(binwidth=5, alpha=.5)

pharmacy.arrivals.per.day <- pharmacy.hospital.stays %>%
                    group_by(Date.of.Admission) %>%
                    arrange(Date.of.Admission) %>%
                    summarize(Count = n())
ggplot(data = pharmacy.arrivals.per.day, aes(x = Date.of.Admission, y = Count)) + geom_point() + geom_smooth() + ggtitle("Arrivals per day")
# let's see if a histogram with varying bin size will give us more information (week? month?)
ggplot(data=pharmacy.hospital.stays, aes(pharmacy.hospital.stays$Date.of.Admission)) + geom_histogram(binwidth=30)

pharmacy.arrivals.per.month <- pharmacy.hospital.stays %>%
  mutate(
    month = months(Date.of.Admission),
    year = format(Date.of.Admission,"%Y")
  ) %>%
  group_by(month,year) %>%
  summarize(count = n())
ggplot(data=pharmacy.arrivals.per.month, aes(x = month, y = count)) + geom_point()

# length of stay
# histogram
ggplot(data=pharmacy.hospital.stays, aes(pharmacy.hospital.stays$length.of.stay)) + geom_histogram()
# scatterplot age - sex - not conclusive
pairs(~length.of.stay+age.num+Sex+Method.of.Admission.Category,data=pharmacy.hospital.stays, 
      main="Simple Scatterplot Matrix")

# method of admission
pharmacy.stays.per.method.of.admission <- group_by(pharmacy.hospital.stays, Method.of.Admission.Category) %>%
                                          summarize(count = n(),
                                                    mean.length.of.stay = mean(length.of.stay,na.rm = TRUE),
                                                    sd.length.of.stay = sd(length.of.stay, na.rm = TRUE)
                                                    )

# anomalous data
# 130 rows with NA lenght of stay
no.length.of.stay <- filter(pharmacy.hospital.stays, is.na(length.of.stay))
# = patients have not left
# no patient id!
no.id <- pharmacy.hospital.stays[is.na(pharmacy.data$H.C.Encrypted),]

# 1 patient, 93 PAS records for 1 stay
patient1 <- pharmacy.hospital.stays[pharmacy.hospital.stays$count == 93.0,]
patient1.stay <- pharmacy.data[pharmacy.data$H.C.Encrypted == patient1$H.C.Encrypted,]

# patient stay 244 days
patient2 <- filter(pharmacy.hospital.stays, length.of.stay == 244.0)
patient2.stay <- filter(pharmacy.data, H.C.Encrypted == patient2$H.C.Encrypted)
