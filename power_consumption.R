install.packages("dplyr")
library(dplyr)
library(tidyr)
library(ggplot2)


household_power_consumption <- read.table("C:/Users/User/Desktop/Ubiqum/household_power_consumption/household_power_consumption.txt", sep=";", stringsAsFactors = FALSE, header = TRUE)
View(household_power_consumption)

mean(is.na(household_power_consumption))
sum(is.na(household_power_consumption))
mean(!is.na(household_power_consumption))
sum(!is.na(household_power_consumption))

str(household_power_consumption)
summary(household_power_consumption)



household_power_consumption$Global_active_power<- as.numeric(household_power_consumption$Global_active_power)
household_power_consumption$Global_reactive_power<-as.numeric(household_power_consumption$Global_reactive_power)
household_power_consumption$Voltage<- as.numeric(household_power_consumption$Voltage)
household_power_consumption$Global_intensity<- as.numeric(household_power_consumption$Global_intensity)
household_power_consumption$Sub_metering_1<- as.numeric(household_power_consumption$Sub_metering_1)
household_power_consumption$Sub_metering_2<- as.numeric(household_power_consumption$Sub_metering_2)
household_power_consumption$Sub_metering_3<- as.numeric(household_power_consumption$Sub_metering_3)



str(household_power_consumption)

household_power_consumption2<-household_power_consumption
View(household_power_consumption2)

#Converting Date to as.Date
#household_power_consumption2$Date<- as.Date(household_power_consumption2$Date)
str(household_power_consumption$Date)


#creating Date+Time

household_power_consumption2$DateTime <- paste(household_power_consumption2$Date,household_power_consumption2$Time)
View(household_power_consumption2)

household_power_consumption2$DateTime <- strptime(household_power_consumption2$DateTime, "%d/%m/%Y %H:%M:%S", tz ="") # Converts the string to a Date/Time Object
household_power_consumption2$DateTime =  as.POSIXct(household_power_consumption2$DateTime, tz = "")
View(household_power_consumption2)
#household_power_consumption$DateTime =  as.POSIXct(household_power_consumption$DateTime, "%d/%m/%Y %H:%M:%S", tz ="GMT")
No_NA<- na.omit(household_power_consumption2)
View(No_NA)
str(No_NA)

#________________________Not Working YET___________________
No_NA<- mutate(No_NA, Hours =format(No_NA$DateTime, "%H"))
#View(No_NA)
No_NA<- mutate(No_NA, years =format(No_NA$DateTime,  "%Y"))
#View(No_NA)
No_NA<- mutate(No_NA, months=format(No_NA$DateTime, "%m"))
#View(No_NA)
No_NA<- mutate(No_NA,  days=format(No_NA$DateTime, "%d"))
#View(No_NA)
No_NA<- mutate(No_NA, EnergyPerM=round(Global_active_power*1000/60) - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)
#View(No_NA)
No_NA<- mutate(No_NA, weeks=format(No_NA$DateTime, "%w"))
View(No_NA)

#_______________________________________________________________

ggplot(No_NA, aes(Hours,EnergyPerM)) + geom_line() +
  theme(legend.position = "none")
ggplot(No_NA, aes(EnergyPerM)) + 
geom_line(stat="density", adjust=.25, colour="red") +   
geom_line(stat="density") +    geom_line(stat="density", adjust=2, colour="blue") 


ggplot(No_NA, aes(x=EnergyPerM, y=months)) + geom_jitter()

######################################################
No_NA<- mutate(No_NA,
    TotalSubmet = (Sub_metering_1+Sub_metering_2+Sub_metering_3))

View(No_NA)
ggplot(No_NA, aes(TotalSubmet)) + 
  geom_line(stat="density", adjust=.25, colour="red") +   
  geom_line(stat="density") +    geom_line(stat="density", adjust=2, colour="blue") 


#per day

Per_days<- group_by(No_NA, days)%>%
  summarize(DaySubmet=sum(TotalSubmet), count=n(),
            avg_d=mean(TotalSubmet))
  
ggplot(Per_days, aes(days,DaySubmet)) + geom_point(color="firebrick") + geom_smooth(model=lm)

ggplot(Per_days,aes(days, avg_d)) + geom_jitter( color="firebrick")+ geom_smooth(method="loess", size =1.5)

ggplot(Per_days, aes(days, avg_d)) +  geom_point(stat='identity', position='identity',size=3) + 
  geom_smooth(method="lm") + stat_smooth(method = "loess")

################################################PerHours#####################################
#Per Hours

Per_Hours<-group_by(No_NA, Hours)%>%
  summarise(hourSubmet=mean(TotalSubmet),
            count=n(), avgSub_hour=mean(TotalSubmet))

View(Per_Hours)

ggplot(Per_Hours, aes(Hours,avgSub_hour, fill=hourSubmet)) +geom_bar(stat="identity", position="identity")

#########################################################################################
#Per year

Per_years<- group_by(No_NA, years)%>%
  summarise(yearsSubmet= mean(TotalSubmet),
            count=n(), avgSubmet_years=mean(TotalSubmet))

  ggplot(Per_years, aes(avgSubmet_years, colours =years )) + geom_freqpoly(binwidth = 0.5)
  ggplot(Per_years, aes(years, avgSubmet_years, colour=years)) + geom_point() + geom_smooth(method="lm")
  
ggplot(Per_years, aes(years, avgSubmet_years, fill = avgSubmet_years)) + geom_density()
  
  
ggplot(Per_years, aes(year,avgSubmet_years, color=years, shape=years)) +
  geom_point(size=10) + geom_smooth(method = "lm", aes(fill=years))
#_____________________________________________________________

ggplot(Per_years, aes(years)) + geom_area(stat="bin")



 #+ geom_smooth(method = "lm")


