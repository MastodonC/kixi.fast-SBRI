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
                   age.num = as.integer(Age)
                 )



# group by hospital stay (patient, same day admitted, same day discharged)
by_stay <- group_by(pharmacy.data, H.C.Encrypted,age.num,Sex,Date.of.Admission,Date.of.Discharge)
pharmacy.hospital.stays <- summarize(by_stay,
                            count = n()
                            )

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
ggplot(data=pharmacy.hospital.stays, aes(pharmacy.hospital.stays$Date.of.Admission)) + geom_histogram(binwidth=7)

