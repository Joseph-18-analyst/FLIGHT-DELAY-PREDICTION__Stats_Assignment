#PREPARE A TRAINING DATASET
train_df<-read.csv(file="C://DCU_Data_Analytics//CA660_Statistical_DA//flights.csv",head=TRUE,sep = ",")
head(train_df)

#lets check data is weekend or not.  If it is weekend, enter 1 or else 0. The weekend starts from Friday and ends on Sunday.
train_df$WEEKEND<-ifelse(train_df$DAY_OF_WEEK==5 | 
                           train_df$DAY_OF_WEEK==6| train_df$DAY_OF_WEEK==7,1,0)
#now create new column dep_hour extracted from dep_time
train_df$DEP_HOUR<-floor(train_df$DEPARTURE_TIME/100)
train_df$DELAY_LABELED<-ifelse(train_df$ARRIVAL_DELAY>15,1,0)
train_df$DELAY_LABELED<-as.integer(train_df$DELAY_LABELED)
#filter those0 flights which cancilation =1
train_df<-data.frame(train_df,train_df$CANCELLED==0)
#filtring NA data
train_df<-data.frame(train_df,train_df$ARRIVAL_DELAY!="NA")
train_df<-na.omit(train_df)
str(train_df)
head(train_df)
library(dplyr)
#count of ontime and delayed flights
delay<-summarise(group_by(train_df, DELAY_LABELED),count=n())
delay$STATUS<-ifelse(delay$DELAY_LABELED==0,"ontime","delayed")
delay<-delay[,-1]
delay_r<-as.data.frame(delay)
delay_r$Percentage<-(delay_r$count/sum(delay_r$count))*100
delay_r$Percentage<-round(delay_r$Percentage,2)

#create a pie chart for delayed and ontime flights
head(delay_r)
install.packages("ggplot2")
library(ggplot2)
blank_theme<-theme_minimal()+theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(size = 14,face = "bold")
)
ggplot(delay_r,aes(x="",y=Percentage,fill=STATUS))+
  geom_bar(stat="identity",width=1,colour="green")+
  coord_polar(theta = "y",start = 0)+
  blank_theme+ggtitle("Pie Chart for Flights")+
  theme(axis.text.x = element_blank())+
  geom_text(aes(y=Percentage/2,label=paste0(Percentage,"%"),hjust=2))


#filghts delayd or ontime in days of weeks
delay_flights<-filter(train_df,train_df$DELAY_LABELED==1)
non_delay_flights<-filter(train_df,train_df$DELAY_LABELED==0)
delay_flights_count<-summarise(group_by(delay_flights,DAY_OF_WEEK),DELAY_COUNT=n())
non_delay_flights_count<-summarise(group_by(non_delay_flights,DAY_OF_WEEK),NON_DELAY_COUNT=n())
dayofweek_count<-merge(delay_flights_count,non_delay_flights_count,by="DAY_OF_WEEK")

dayofweek_count$DAY_OF_WEEK_y<-NULL
#dayofweek_count<-rename(dayofweek_count,replace=c("DAY_OF_WEEK_x"="DAY_OF_WEEK"))


dayofweek_count_r <- as.data.frame(dayofweek_count)
head(dayofweek_count_r)

dayofweek_count_r$Delayed <- (dayofweek_count_r$DELAY_COUNT/(dayofweek_count_r$DELAY_COUNT+dayofweek_count_r$NON_DELAY_COUNT)) * 100
dayofweek_count_r$Ontime <- (dayofweek_count_r$NON_DELAY_COUNT/(dayofweek_count_r$DELAY_COUNT+dayofweek_count_r$NON_DELAY_COUNT)) * 100
dayofweek_count_r <- dayofweek_count_r[,-2:-3]

dayofweek_count_r$Ratio <- dayofweek_count_r$Delayed/dayofweek_count_r$Ontime * 100
dayofweek_count_r$Ratio <- round(dayofweek_count_r$Ratio,2)
install.packages("reshape")
library(reshape)
DF1 <- melt(dayofweek_count_r, id.var="DAY_OF_WEEK")
DF1$Ratio <- DF1[15:21,3]
DF1
DF1 <- DF1[-15:-21,]
DF1[8:14,4] <- NA
DF1


#now create barchart from the above data
install.packages("ggrepel")
library(ggrepel)
ggplot(DF1, aes(x=DAY_OF_WEEK,y=value,fill=variable)) + 
  geom_bar(stat="identity") + 
  geom_path(aes(y=Ratio,color="Ratio of Delayed flights against Non Delayed Flights")) + 
  geom_text_repel(aes(label=Ratio), size = 3) + 
  ggtitle("Percentage of Flights Delayed") + 
  labs(x="Day of Week",y="Percentage")

#linear model is created for analysing the data
train_df_glm<-glm(ARRIVAL_DELAY~MONTH+DEP_HOUR+DEPARTURE_DELAY+WEEKEND,
                  data = train_df,family = "gaussian")
summary(train_df_glm)
#now we are predicting the test dataset
test_df<-read.csv(file="C://DCU_Data_Analytics//CA660_Statistical_DA//test.csv",head=TRUE,sep = ",")
test_df$WEEKEND<-ifelse(test_df$DAY_OF_WEEK==5 | 
                          test_df$DAY_OF_WEEK==6| test_df$DAY_OF_WEEK==7,1,0)
test_df$DEP_HOUR<-floor(test_df$DEPARTURE_TIME/100)
test_df$DELAY_LABELED<-ifelse(test_df$ARRIVAL_DELAY>15,1,0)
test_df$DELAY_LABELED<-as.integer(test_df$DELAY_LABELED)
test_df<-data.frame(test_df,test_df$CANCELLED==0)
test_df<-data.frame(test_df,test_df$ARRIVAL_DELAY!="NA")
test_df <- data.frame(test_df,test_df$ORIGIN_AIRPORT != "SBN")
test_df <- data.frame(test_df,test_df$DESTINATION_AIRPORT != "SBN")
test_df$ARR_DELAY <- as.integer(test_df$ARRIVAL_DELAY)
test_df$DEP_DELAY <- as.integer(test_df$DEPARTURE_DELAY)
head(test_df)
predictions<-predict(train_df_glm,newdata = test_df)
head(predictions,"ARR_DELAY","predictions")
head(predictions)
head(test_df$ARR_DELAY)


#Logistic regression to predict arr_delay
dummy<-subset(train_df,select = c(4,5,7,8,18,19))
dummy$DAY_OF_WEEK<-as.character(dummy$DAY_OF_WEEK)
dummy$DEP_HOUR<-as.character(dummy$DEP_HOUR)
head(dummy$DAY_OF_WEEK)
head(dummy$DEP_HOUR)
head(dummy$DELAY_LABELED)


#split the variables in dummy dataframe
dummy <- dummy[(dummy$ORIGIN_AIRPORT == "SNA" | dummy$ORIGIN_AIRPORT == "ORD" | dummy$ORIGIN_AIRPORT == "HNL") ,]
dummy <- dummy[(dummy$DESTINATION_AIRPORT == "LAX" | dummy$DESTINATION_AIRPORT == "SFO" | dummy$DESTINATION_AIRPORT == "JFK") ,]
dummy_r <- as.data.frame(dummy)
head(dummy_r$DELAY_LABELED)
length(dummy_r$DELAY_LABELED)
dummy_matrix1 <- model.matrix(~DAY_OF_WEEK+DESTINATION_AIRPORT+ORIGIN_AIRPORT,data=dummy_r)
dummy_matrix1 <- dummy_matrix1[,-1]
#dummy_matrix1<-dummy_matrix1[c(1:11016),]
dummy_matrix2 <- model.matrix(~DEP_HOUR,data=dummy_r)
dummy_matrix2 <- dummy_matrix2[,-1]
dummy_matrix<-cbind(dummy_matrix1,dummy_matrix2)
dummy_df<-data.frame(dummy_matrix)
dummy_df<-cbind(dummy_df,dummy_r$DELAY_LABELED[c(1:10990)])

colnames(dummy_df)[ncol(dummy_df)]<-"DELAY_LABELED"
head(dummy_df)

dummy_df_glm<-glm(DELAY_LABELED~ .,
                  data = dummy_df,family = "gaussian")
summary(dummy_df_glm)

dummy_test<-subset(test_df,select = c(4,5,7,8,18,19))
dummy_test$DAY_OF_WEEK<-as.character(dummy_test$DAY_OF_WEEK)
dummy_test$DEP_HOUR<-as.character(dummy_test$DEP_HOUR)
dummy_test <- dummy_test[(dummy_test$ORIGIN_AIRPORT == "SNA" | dummy_test$ORIGIN_AIRPORT == "ORD" | dummy_test$ORIGIN_AIRPORT == "HNL") ,]
dummy_test <- dummy_test[(dummy_test$DESTINATION_AIRPORT == "LAX" | dummy_test$DESTINATION_AIRPORT == "SFO" | dummy_test$DESTINATION_AIRPORT == "JFK") ,]
dummy_test_r <- as.data.frame(dummy_test)
dummy_test_matrix1<-model.matrix(~DAY_OF_WEEK+DESTINATION_AIRPORT+ORIGIN_AIRPORT,data = dummy_test_r)
dummy_test_matrix1 <- dummy_test_matrix1[,-1]
dummy_test_matrix1<-dummy_test_matrix1[c(1:7499),]
dummy_test_matrix2 <- model.matrix(~DEP_HOUR,data=dummy_test_r)
dummy_test_matrix2 <- dummy_test_matrix2[,-1]
dummy_test_matrix <- cbind(dummy_test_matrix1,dummy_test_matrix2)
dummy_test_df <- as.data.frame(dummy_test_matrix)
dummy_test_df <- cbind(dummy_test_df,dummy_test_r$DELAY_LABELED[c(1:7499)])
colnames(dummy_test_df)[ncol(dummy_test_df)] <- "DELAY_LABELED"
head(dummy_test_df)
dummy_test_df<-as.data.frame(dummy_test_df)
predictions1<-predict(dummy_df_glm,newdata1=dummy_test_df)
head(predictions1,"DELAY_LABELED","prediction")
head(predictions1)
class(predictions1)
#predictions<-as.character(predictions)
head(dummy_test_df$DELAY_LABELED)
p<-as.data.frame(predictions1[c(1:10697)])
head(p)
p$binary<-ifelse(p$prediction>0.3,1,0)
q<-as.data.frame(predictions[c(1:10697)])
head(q)
q$binary<-ifelse(q$predictions>0.3,1,0)
actualResults<-summarise(group_by(q,q$binary),Actual=n())
actualResults<-actualResults[-c(3),]
predictedResults <- summarise(group_by(p,p$binary), Predicted =n())

actualResults<-rename(actualResults,replace = c("q$binary"="label"))
predictedResults<-rename(predictedResults,replace = c("p$binary"="label"))

result<-merge(actualResults,predictedResults,by="label")
head(result)




library(caTools)
split= sample.split(train_df,SplitRatio = 0.8)
split
train1= subset(train_df,split=="TRUE")
validation = subset(train_df,split=="FALSE")

dummy<-subset(train1,select = c(4,5,7,8,18,19))
dummy$DAY_OF_WEEK<-as.character(dummy$DAY_OF_WEEK)
dummy$DEP_HOUR<-as.character(dummy$DEP_HOUR)
head(dummy$DAY_OF_WEEK)
head(dummy$DEP_HOUR)
head(dummy$DELAY_LABELED)

dummy <- dummy[(dummy$ORIGIN_AIRPORT == "SNA" | dummy$ORIGIN_AIRPORT == "ORD" | dummy$ORIGIN_AIRPORT == "HNL") ,]
dummy <- dummy[(dummy$DESTINATION_AIRPORT == "LAX" | dummy$DESTINATION_AIRPORT  == "SFO" | dummy$DESTINATION_AIRPORT  == "JFK") ,]
dummy_r <- as.data.frame(dummy)
head(dummy_r$DELAY_LABELED)
length(dummy_r$DELAY_LABELED)
dummy_matrix1 <- model.matrix(~DAY_OF_WEEK+DESTINATION_AIRPORT+ORIGIN_AIRPORT,data=dummy_r)
dummy_matrix1 <- dummy_matrix1[,-1]
#dummy_matrix1<-dummy_matrix1[c(1:8342),]
dummy_matrix2 <- model.matrix(~DEP_HOUR,data=dummy_r)
dummy_matrix2 <- dummy_matrix2[,-1]
dummy_matrix<-cbind(dummy_matrix1,dummy_matrix2)
dummy_df<-data.frame(dummy_matrix)
dummy_df<-cbind(dummy_df,dummy_r$DELAY_LABELED)

colnames(dummy_df)[ncol(dummy_df)]<-"DELAY_LABELED"
head(dummy_df)

dummy_df_glm<-glm(DELAY_LABELED~ .,
                  data = dummy_df,family = "gaussian")
summary(dummy_df_glm)

dummy_valid<-subset(validation,select = c(4,5,7,8,18,19))
dummy_valid$DAY_OF_WEEK<-as.character(dummy_valid$DAY_OF_WEEK)
dummy_valid$DEP_HOUR<-as.character(dummy_valid$DEP_HOUR)
dummy_valid <- dummy_valid[(dummy_valid$ORIGIN_AIRPORT == "SNA" | dummy_valid$ORIGIN_AIRPORT == "ORD" | dummy_valid$ORIGIN_AIRPORT == "HNL") ,]
dummy_valid <- dummy_valid[(dummy_valid$DESTINATION_AIRPORT == "LAX" | dummy_valid$DESTINATION_AIRPORT == "SFO" | dummy_valid$DESTINATION_AIRPORT == "JFK") ,]
dummy_valid_r <- as.data.frame(dummy_valid)
dummy_valid_matrix1<-model.matrix(~DAY_OF_WEEK+DESTINATION_AIRPORT+ORIGIN_AIRPORT,data = dummy_valid_r)
dummy_valid_matrix1 <- dummy_valid_matrix1[,-1]
dummy_valid_matrix2 <- model.matrix(~DEP_HOUR,data=dummy_valid_r)
dummy_valid_matrix2 <- dummy_valid_matrix2[,-1]
#dummy_valid_matrix2<-dummy_valid_matrix2[c(1:2620),]
dummy_valid_matrix <- cbind(dummy_valid_matrix1,dummy_valid_matrix2)
dummy_valid_df <- as.data.frame(dummy_valid_matrix)
dummy_valid_df <- cbind(dummy_valid_df,dummy_valid_r$DELAY_LABELED)
colnames(dummy_valid_df)[ncol(dummy_valid_df)] <- "DELAY_LABELED"
head(dummy_valid_df)
dummy_valid_df<-as.data.frame(dummy_valid_df)



predictions1<-predict(dummy_df_glm,newdata1=dummy_valid_df)
head(predictions1,"DELAY_LABELED","prediction")
head(predictions1)
class(predictions1)
#predictions<-as.character(predictions)
head(dummy_valid_df$DELAY_LABELED)
p<-as.data.frame(predictions1[c(1:2650)])
head(p)
p$binary<-ifelse(p$prediction>0.3,1,0)
q<-as.data.frame(predictions[c(1:2663)])
head(q)
q$binary<-ifelse(q$predictions>0.3,1,0)
actualResults<-summarise(group_by(q,q$binary),Actual=n())
actualResults<-actualResults[-c(3),]
predictedResults <- summarise(group_by(p,p$binary), Predicted =n())

actualResults<-rename(actualResults,replace = c("q$binary"="label"))
predictedResults<-rename(predictedResults,replace = c("p$binary"="label"))

result<-merge(actualResults,predictedResults,by="label")
head(result)

