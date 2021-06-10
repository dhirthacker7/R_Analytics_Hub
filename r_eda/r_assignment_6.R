# Importing data

emergency_calls <- read.csv("911.csv")
View(emergency_calls)

# Loading packages

install.packages("Rshiny")
require(dplyr)
require(plyr)
require(psych)
require(ggplot2)
require(plotly)
require(tidyr)
library(lubridate)
library(viridis)
install.packages(rshi)
library(chron)
# install.packages("ggmap")
library(ggmap)
library(ggmap)
library(date)
# install.packages("date")
require(date)

# Summarizing the data

dim(emergency_calls)
summary(emergency_calls)
describe(emergency_calls)
str(emergency_calls)
 
# Checking the variable class

class(emergency_calls$zip)
class(emergency_calls$timeStamp)
class(emergency_calls$title)
 
# Changing the class of zipcode to factor

emergency_calls$zip <- factor(emergency_calls$zip)

# Cleaning the data

# Removing the dummy variable ('addr' column and 'e' column)

emergency_calls <- emergency_calls[,1:8]
View(emergency_calls)

# Changing the class of timestamp to date

emergency_calls$timeStamp <- as.Date(emergency_calls$timeStamp)
class(emergency_calls$timeStamp)
 
#Top 10 zipcodes for all 911 calls#
Top_10_zipcodes <-count(emergency_calls$zip)
Top_10_zipcodes <-arrange(Top_10_zipcodes,-freq)
Top_10_zipcodes <-Top_10_zipcodes[2:106,]
Top_10_zipcodes <- head(Top_10_zipcodes,10)

Top_10_zipcodes$zipcode <- Top_10_zipcodes[,1]
Top_10_zipcodes<- Top_10_zipcodes[,2:3]
View(Top_10_zipcodes)

write.csv(top_10_zipcodes, file='E:\\Sem_8\\sd_lab\\r\\r_assignment_2\\Top_10_zipcodes.csv')

Top_10_zipcodes$Perc <- Top_10_zipcodes$freq/sum(Top_10_zipcodes$freq) * 100
Top_10_zipcodes$zipcode <- factor(Top_10_zipcodes$zipcode)

# plot

ggplot(Top_10_zipcodes,aes(reorder(zipcode,-freq), freq, fill = zipcode)) + geom_bar(stat = "identity")

ggplot(Top_10_zipcodes,aes(reorder(zipcode,-Perc), Perc, fill = zipcode)) + geom_bar(stat = "identity")

# splitting the title column into type and subtype columns

emergency_calls <- separate(emergency_calls, col = title, into = c("Type", "SubType"), sep = ":")
View(emergency_calls)

# splitting the year, month, day, hour, weekdays into separate columns

emergency_calls$Year <- year(emergency_calls$timeStamp)
emergency_calls$Month <- month(emergency_calls$timeStamp)
emergency_calls$Day <- day(emergency_calls$timeStamp)
emergency_calls$Hour <- hour(emergency_calls$timeStamp)
emergency_calls$Weekday <- weekdays(emergency_calls$timeStamp)

write.csv(emergency_calls, file = 'E:\\Sem_8\\sd_lab\\r\\r_assignment_2\\Total_911_calls_date_split.csv')

emergency_calls$Year <- factor(emergency_calls$Year)
emergency_calls$Month <- factor(emergency_calls$Month)
emergency_calls$Day <- factor(emergency_calls$Day)
emergency_calls$Hour <- factor(emergency_calls$Hour)
emergency_calls$Weekday <- factor(emergency_calls$Weekday)

# list of total calls by Type

Total_calls_by_type <- summarise(group_by(emergency_calls, Type), Total_calls=n())
write.csv(Total_calls_by_type, file='E:\\Sem_8\\sd_lab\\r\\r_assignment_2\\Total_calls_by_type.csv')
Total_calls_by_type$Perc <- Total_calls_by_type$Total_calls/sum(Total_calls_by_type$Total_calls)*100

# list of total calls by zip

Total_calls_by_zip  <- summarise(group_by(emergency_calls, zip), Total_calls=n())
write.csv(Total_calls_by_zip, file='E:\\Sem_8\\sd_lab\\r\\r_assignment_2\\Total_calls_by_zip.csv')

# list of total calls by date

Total_calls_by_date <- summarise(group_by(emergency_calls, Year,Month,Day), Total_calls = n())
write.csv(Total_calls_by_date, file='E:\\Sem_8\\sd_lab\\r\\r_assignment_2\\Total_calls_by_date.csv')

# list of total calls by date(new)

Emergency_Calls <- separate(emergency_calls, col = timeStamp, into = c("Date","Time"), sep = " ")
Total_calls_by_Date <- summarise(group_by(Emergency_Calls,Date), Total_calls = n())
write.csv(Total_calls_by_Date, file = 'E:\\Sem_8\\sd_lab\\r\\r_assignment_2\\Total_calls_by_Date(new).csv')

# graph for total calls date-wise

ggplot(Total_calls_by_Date, aes(Date, Total_calls, group=1)) + geom_line(color = "blue", size = 1)

# total calls by date type

Total_calls_by_date_type <- Emergency_Calls %>% group_by(Date, Type) %>% summarise(Total = n())
write.csv(Total_calls_by_date_type, file = 'E:\\Sem_8\\sd_lab\\r\\r_assignment_2\\Total_calls_by_date_type.csv')

Total_calls_by_date_type$Type <- factor(Total_calls_by_date_type$Type)

#plot

ggplot(Total_calls_by_date_type, aes(Date, Total,group=1)) + geom_line( aes(color = Type), size = 1)

# making tables

table(Emergency_Calls$Year)

table(Emergency_Calls[Emergency_Calls$Year==2016,]$Month)

# plots

ggplot(Emergency_Calls,aes(Year, fill=Year)) +geom_bar() + ggtitle("911 Emergency Calls - Yearly")

ggplot(Emergency_Calls[Emergency_Calls$Year==2016,], aes(Month, fill = Month)) + geom_bar()  + ggtitle("911 Emergency Calls for 2016 - Monthly ")

ggplot(Emergency_Calls, aes(Hour, fill = Hour)) + geom_bar() + ggtitle("911 Emergency Calls - Hourly")

ggplot(Emergency_Calls,aes(Type,fill=Type)) + geom_bar()+ggtitle("911 Emergency Calls by Type")

ggplot(Total_calls_by_type,aes(reorder(Type,-Perc),Perc,fill=Type)) + geom_bar(stat = "identity")+ggtitle("911 Emergency Calls by Type and Perc")

ggplot(Emergency_Calls,aes(Weekday,fill=Weekday))+geom_bar() + ggtitle("911 Emergency calls by weekday")

# # Top 10 township with most no of 911 calls#
#
# Total_calls_by_township <- summarise(group_by(Emergency_Calls,twp), Total_calls=n())
# Total_calls_by_township <- arrange(Total_calls_by_township,-Total_calls)
# Top_10_township <- head(Total_calls_by_township,10)
# View(Top_10_township)
#
#
# Top_10_township$Perc <- Total_calls_by_township$Total_calls/sum(Total_calls_by_township$Total_calls) * 100
# Top_10_township$township <- factor(Total_calls_by_township$twp)
#
# ggplot(Top_10_township,aes(reorder(township,-Total_calls), Total_calls, fill = township)) + geom_bar(stat = "identity")
#
# ggplot(Top_10_township,aes(reorder(township,-Perc), Perc, fill = township)) + geom_bar(stat = "identity")


# SubType Analysis

Total_calls_by_SubType  <- summarise(group_by(emergency_calls,SubType), Total_calls=n())
View(Total_calls_by_SubType)

# Top 10 Subtypes overall

Top_10_subtypes <- arrange(Total_calls_by_SubType,-Total_calls)
Top_10_subtypes <- head(Top_10_subtypes,10)
View(Top_10_subtypes)


Top_10_subtypes$Perc <- Top_10_subtypes$Total_calls/sum(Top_10_subtypes$Total_calls) * 100
View(Top_10_subtypes)

ggplot(Top_10_subtypes, aes(reorder(SubType, Total_calls), Total_calls, fill = SubType)) +
  geom_bar(stat = "identity") + theme(legend.position = "none") +
  ggtitle("911 Emergency Calls by Top 10 SubType") + xlab("Subtype") + ylab("Number of 911 Calls")

# Top subtypes by Traffic#

Total_calls_by_Traffic <- summarise(group_by(Emergency_Calls,Type,SubType),Total = n())
Total_calls_by_Traffic <- subset(Total_calls_by_Traffic,Total_calls_by_Traffic$Type=="Traffic")
View(Total_calls_by_Traffic)

ggplot(Total_calls_by_Traffic, aes(reorder(SubType, Total), Total, fill = SubType)) +
  geom_bar(stat = "identity") + theme(legend.position = "none") +
  ggtitle("911 Emergency Calls by Top SubType within Traffic") + xlab("Subtype") + ylab("Number of 911 Calls")



Subtype_by_weekdays <- summarise(group_by(Emergency_Calls,Weekday, SubType), Total = n())

Subtype_by_weekdays <- subset(Subtype_by_weekdays,Subtype_by_weekdays$Weekday != "Sunday"
                              & Subtype_by_weekdays$Weekday !="Saturday")

# Crimes by weekdays

View(Subtype_by_weekdays)

Subtype_by_weekends <- subset(Subtype_by_weekdays,Subtype_by_weekdays$Weekday == "Sunday"
                              | Subtype_by_weekdays$Weekday =="Saturday")

# Crimes by weekends

View(Subtype_by_weekends)

Hour_wise_spike <- summarise(group_by(Emergency_Calls,Type,SubType,Hour), Total =n())
View(Hour_wise_spike)

Hour_wise_spike_traffic <- subset(Hour_wise_spike,Hour_wise_spike$Type == "Traffic")
View(Hour_wise_spike_traffic)


ggplot(Hour_wise_spike_traffic, aes(Hour, Total,group=1)) + geom_line( aes(color = SubType), size = 1)