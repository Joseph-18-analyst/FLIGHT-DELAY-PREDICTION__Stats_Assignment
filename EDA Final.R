library(data.table)
library(Hmisc)
library(tidyverse)
library(readr)
library(dplyr)
library(magrittr)
library(base)
library(ggcorrplot)
flights <- read.csv("C://DCU_Data_Analytics//CA660_Statistical_DA//flights.csv")

# To delete cancelled and diverted flight entries
flights_new <- subset(flights,flights$DIVERTED<=0 & flights$CANCELLED <=0)
flights_eda <- flights_new[-c(1,24:26)]

# To eliminate NA values
flights_eda$AIR_SYSTEM_DELAY[is.na(flights_eda$AIR_SYSTEM_DELAY)] <- 0
flights_eda$SECURITY_DELAY[is.na(flights_eda$SECURITY_DELAY)] <- 0
flights_eda$AIRLINE_DELAY[is.na(flights_eda$AIRLINE_DELAY)] <- 0
flights_eda$LATE_AIRCRAFT_DELAY[is.na(flights_eda$LATE_AIRCRAFT_DELAY)] <- 0
flights_eda$WEATHER_DELAY[is.na(flights_eda$WEATHER_DELAY)] <- 0
summary(flights_eda)

# To analyse the dataset for  "departure delay" in minutes 
delay_analysis <- hist(flights_eda$DEPARTURE_DELAY,breaks = 15)
D <- data.frame(delay_analysis$breaks[2:22],delay_analysis$counts,cumsum(delay_analysis$counts))
D$percentage <- round(D$delay_analysis.counts*100/5714008,digits = 3)
D$cum_percentage <- round(D$cumsum.delay_analysis.counts.*100/5714008,digits = 3)
write.csv(D,file = "delay analysis.csv")

# average delay by airlines
airline_delay_avg <- flights_eda %>%
group_by(AIRLINE) %>%
  summarise(mean_delay = mean(DEPARTURE_DELAY))
ggplot(airline_delay_avg,aes(AIRLINE,mean_delay,fill=AIRLINE), position = "dodge")+geom_col()+
  geom_text(aes(label= round(mean_delay,digits = 2),vjust=-0.5))

# Airlines count in dataset
ggplot(flights_eda,aes(AIRLINE,fill=AIRLINE))+geom_bar()+
  geom_text(stat = 'count',aes(label =..count.., vjust = -1))

# departure delay classification above 15mins for delay
flights_eda$DEP_DELAY_yes_no <- ifelse(flights_eda$DEPARTURE_DELAY<15,0,1)

# monthwise departure delays
flights_eda$MONTH <- as.factor(flights_eda$MONTH)
ggplot(flights_eda,aes(x=MONTH,y=DEP_DELAY_yes_no,fill='red'))+geom_col()+
  labs(title = "Month wise departure delays",x="Month",y="No. of delays" )

#daywise departure delays
flights_eda$DAY <- as.factor(flights_eda$DAY)
ggplot(flights_eda,aes(x=DAY,DEP_DELAY_yes_no,fill='red'))+geom_col()+
  labs(title = "Date wise departure delays",x="Date",y="No. of delays" )

#day of week-wise departure delays
flights_eda$DAY_OF_WEEK <- as.factor(flights_eda$DAY_OF_WEEK)
ggplot(flights_eda,aes(x=DAY_OF_WEEK,y=DAY_OF_WEEK,fill='red'))+geom_col()+
  labs(title = "week wise departure delays",x="Day of Week",y="No. of delays" )

# scheduled departure vs departure delay 
ggplot(flights_eda,aes(x=SCHEDULED_DEPARTURE,y=flights_eda$DEP_DELAY_yes_no))+geom_col()+
  labs(title = "scheduled departure vs delays ",x="Scheduled Departure",y="No. of delays" )

# monthwise proportion of different type of delays
flights_eda$NEW_AIR_SYSTEM_DELAY <- ifelse(flights_eda$AIR_SYSTEM_DELAY>0,1,0)
flights_eda$NEW_SECURITY_DELAY <- ifelse(flights_eda$SECURITY_DELAY >0,1,0)
flights_eda$NEW_AIRLINE_DELAY <- ifelse(flights_eda$AIRLINE_DELAY >0,1,0)
flights_eda$NEW_LATE_AIRCRAFT_DELAY <- ifelse(flights_eda$LATE_AIRCRAFT_DELAY >0,1,0)
flights_eda$NEW_WEATHER_DELAY <- ifelse(flights_eda$WEATHER_DELAY >0,1,0)

delay_type <- flights_eda %>%
  group_by(MONTH) %>%
  summarise(count1 = sum(NEW_AIR_SYSTEM_DELAY),count2=sum(NEW_SECURITY_DELAY),
            count3=sum(NEW_AIRLINE_DELAY),count4=sum(NEW_LATE_AIRCRAFT_DELAY),count5=sum(NEW_WEATHER_DELAY)) 
delay_type$total <- rowSums(delay_type[c(2:6)])
delay_type$AS <- 100*delay_type$count1/delay_type$total
delay_type$SD <- 100*delay_type$count2/delay_type$total
delay_type$AD <- 100*delay_type$count3/delay_type$total
delay_type$LA <- 100*delay_type$count4/delay_type$total
delay_type$WD <- 100*delay_type$count5/delay_type$total
write.csv(delay_type,file = "monthwise delay types.csv")

# datewise proportion of different type of delays
delay_type_day <- flights_eda %>%
  group_by(DAY) %>%
  summarise(count1 = sum(NEW_AIR_SYSTEM_DELAY),count2=sum(NEW_SECURITY_DELAY),
            count3=sum(NEW_AIRLINE_DELAY),count4=sum(NEW_LATE_AIRCRAFT_DELAY),count5=sum(NEW_WEATHER_DELAY)) 
delay_type_day$total <- rowSums(delay_type_day[c(2:6)])
delay_type_day$AS <- 100*delay_type_day$count1/delay_type_day$total
delay_type_day$SD <- 100*delay_type_day$count2/delay_type_day$total
delay_type_day$AD <- 100*delay_type_day$count3/delay_type_day$total
delay_type_day$LA <- 100*delay_type_day$count4/delay_type_day$total
delay_type_day$WD <- 100*delay_type_day$count5/delay_type_day$total
write.csv(delay_type_day,file = "daywise delay types.csv")

# day of week-wise proportion of different type of delays
delay_type_day1 <- flights_eda %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(count1 = sum(NEW_AIR_SYSTEM_DELAY),count2=sum(NEW_SECURITY_DELAY),
            count3=sum(NEW_AIRLINE_DELAY),count4=sum(NEW_LATE_AIRCRAFT_DELAY),count5=sum(NEW_WEATHER_DELAY)) 
delay_type_day1$total <- rowSums(delay_type_day1[c(2:6)])
delay_type_day1$AS <- 100*delay_type_day1$count1/delay_type_day1$total
delay_type_day1$SD <- 100*delay_type_day1$count2/delay_type_day1$total
delay_type_day1$AD <- 100*delay_type_day1$count3/delay_type_day1$total
delay_type_day1$LA <- 100*delay_type_day1$count4/delay_type_day1$total
delay_type_day1$WD <- 100*delay_type_day1$count5/delay_type_day1$total
write.csv(delay_type_day1,file = "week wise delay types.csv")

# airline wise proportion of different type of delays
delay_type_day2 <- flights_eda %>%
  group_by(AIRLINE) %>%
  summarise(count1 = sum(NEW_AIR_SYSTEM_DELAY),count2=sum(NEW_SECURITY_DELAY),
            count3=sum(NEW_AIRLINE_DELAY),count4=sum(NEW_LATE_AIRCRAFT_DELAY),count5=sum(NEW_WEATHER_DELAY)) 
delay_type_day2$total <- rowSums(delay_type_day2[c(2:6)])
delay_type_day2$AS <- 100*delay_type_day2$count1/delay_type_day2$total
delay_type_day2$SD <- 100*delay_type_day2$count2/delay_type_day2$total
delay_type_day2$AD <- 100*delay_type_day2$count3/delay_type_day2$total
delay_type_day2$LA <- 100*delay_type_day2$count4/delay_type_day2$total
delay_type_day2$WD <- 100*delay_type_day2$count5/delay_type_day2$total
write.csv(delay_type_day2,file = "airline wise delay types.csv")

# to analyse taxi out and taxi in w.r.to origin airport and destination airport
OA_taxi_out <- flights_eda %>%
  group_by(ORIGIN_AIRPORT) %>%
  summarise(median_OA=median(TAXI_OUT),mean_OA=mean(TAXI_OUT))
OA_taxi_out <- OA_taxi_out[order(OA_taxi_out$median_OA,decreasing = TRUE),]
ggplot(head(OA_taxi_out,10),aes(ORIGIN_AIRPORT,median_OA,fill=ORIGIN_AIRPORT))+geom_col()
write.csv(OA_taxi_out,file = "OA vs Taxi out")

DA_taxi_in <- flights_eda %>%
  group_by(DESTINATION_AIRPORT) %>%
  summarise(median_DA=median(TAXI_IN),mean_DA=mean(TAXI_IN))
DA_taxi_in <- DA_taxi_in[order(DA_taxi_in$median_DA,decreasing = TRUE),]
ggplot(head(DA_taxi_in,10),aes(DESTINATION_AIRPORT,median_DA,fill=DESTINATION_AIRPORT))+geom_col()
write.csv(DA_taxi_in,file = "DA vs Taxi in")

# number of flights in each month
ggplot(flights_eda,aes(MONTH,fill=AIRLINE))+geom_bar()

# number of flights in each day
ggplot(flights_eda,aes(DAY_OF_WEEK,fill=AIRLINE))+geom_bar()

# number of flights by date
ggplot(flights_eda,aes(DAY,fill=AIRLINE))+geom_bar()

#correlogram
correlogram <- flights_eda[c(9,11,12,14:17,19,20,22,23:27)]
str(correlogram)
corr <- cor(correlogram)
corr <- round(corr,2)
ggcorrplot(corr,hc.order = TRUE,
           method = "square",
           type = "full",
           colors = c("red","white","green"),
           title = "correlogram",
           ggtheme = theme_dark)

