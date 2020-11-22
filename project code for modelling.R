library(data.table)
install.packages("Hmisc")
library(Hmisc)
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)
library(magrittr)
library(base)
flights <- read.csv("C://DCU_Data_Analytics//CA660_Statistical_DA//flights.csv")
flights_new <- subset(flights,flights$DIVERTED<=0 & flights$CANCELLED <=0)
#flights_new$route <- paste(flights_new$AIRLINE,flights_new$ORIGIN_AIRPORT,flights_new$DESTINATION_AIRPORT)#defined for shiny
#route_shiny<- count(flights_new,route)# defined for R-shiny
#origin_airport_shiny <- count(flights_new,ORIGIN_AIRPORT) # defined for R-shiny
#destination_airport_shiny <- count(flights_new,DESTINATION_AIRPORT) #defined for R-shiny
#flights_new$exp <- paste(flights_new$AIRLINE,flights_new$TAIL_NUMBER)
#flights_new$exp <- as.factor(flights_new$exp)
str(flights_new)
colnames(flights_new)
#flights_new <- flights_new[-c(1,11,14,19,22,24,25,26)]
flights_new <- flights_new[-c(1,6,7,11,14,19,22,24,25,26)]
flights_new$ARRIVAL_DELAY_CLASS <- ifelse(flights_new$ARRIVAL_DELAY<15,0,1)
flights_new$DEPARTURE_DELAY_CLASS <- ifelse(flights_new$DEPARTURE_DELAY<15,0,1)
flights_new$s.no <- c(1:5714008)
# to create schedule departure and schedule arrival time columns into 24 levels (rounding off to nearest hour)
flights_new$NEW_SCHEDULED_DEPARTURE <- round(flights_new$SCHEDULED_DEPARTURE/100)
flights_new$NEW_SCHEDULED_ARRIVAL <- round(flights_new$SCHEDULED_ARRIVAL/100)
# to create column "frequecy of flights from origin airport"
FFOA <- count(flights_new,flights_new$ORIGIN_AIRPORT) # frequency of flights from original airport 628 levels
n.times <- FFOA$n
FFOA <- FFOA[rep(seq_len(nrow(FFOA)), n.times),] 
flights_new <- flights_new[order(flights_new$ORIGIN_AIRPORT),]
flights_new <- cbind(flights_new,FFOA$n)
flights_new$ORIGIN_AIRPORT_CLASS <- ifelse(flights_new$`FFOA$n`>60000,"OH",
                                           ifelse(flights_new$`FFOA$n`>24000,"OM",
                                                  ifelse(flights_new$`FFOA$n`>4600,"OL","OVL")))
flights_new$ORIGIN_AIRPORT_CLASS <- as.factor(flights_new$ORIGIN_AIRPORT_CLASS)

# to create column "frequecy of flights from destination airport"
FFDA <- count(flights_new,flights_new$DESTINATION_AIRPORT) # frequency of flights from destination airport 629 levels
n.times <- FFDA$n
FFDA <- FFDA[rep(seq_len(nrow(FFDA)), n.times),] 
flights_new <- flights_new[order(flights_new$DESTINATION_AIRPORT),]
flights_new <- cbind(flights_new,FFDA$n)
flights_new$DESTINATION_AIRPORT_CLASS <- ifelse(flights_new$`FFDA$n`>50000,"DH",
                                           ifelse(flights_new$`FFDA$n`>24000,"DM",
                                                  ifelse(flights_new$`FFDA$n`>4600,"DL","DVL")))
flights_new$DESTINATION_AIRPORT_CLASS <- as.factor(flights_new$DESTINATION_AIRPORT_CLASS)
#To sort dataframe into original order after above two operations
flights_new <- flights_new[order(flights_new$s.no),]
flights_new$route <- paste(flights_new$AIRLINE,flights_new$ORIGIN_AIRPORT,flights_new$DESTINATION_AIRPORT)#defined for shiny
route_shiny<- count(flights_new,route)# defined for R-shiny
origin_airport_shiny <- count(flights_new,ORIGIN_AIRPORT) # defined for R-shiny

destination_airport_shiny <- count(flights_new,DESTINATION_AIRPORT) #defined for R-shiny
# To treat NA values
flights_new$AIR_SYSTEM_DELAY[is.na(flights_new$AIR_SYSTEM_DELAY)] <- 0
flights_new$SECURITY_DELAY[is.na(flights_new$SECURITY_DELAY)] <- 0
flights_new$AIRLINE_DELAY[is.na(flights_new$AIRLINE_DELAY)] <- 0
flights_new$LATE_AIRCRAFT_DELAY[is.na(flights_new$LATE_AIRCRAFT_DELAY)] <- 0
flights_new$WEATHER_DELAY[is.na(flights_new$WEATHER_DELAY)] <- 0
summary(flights_new)
str(flights_new)
# Strategic sampling based on month. Later, train and test data will be created individually from each of 12 below data sets
sample1 <- subset(flights_new,flights_new$MONTH==1)
sample2 <- subset(flights_new,flights_new$MONTH==2)
sample3 <- subset(flights_new,flights_new$MONTH==3)
sample4 <- subset(flights_new,flights_new$MONTH==4)
sample5 <- subset(flights_new,flights_new$MONTH==5)
sample6 <- subset(flights_new,flights_new$MONTH==6)
sample7 <- subset(flights_new,flights_new$MONTH==7)
sample8 <- subset(flights_new,flights_new$MONTH==8)
sample9 <- subset(flights_new,flights_new$MONTH==9)
sample10 <- subset(flights_new,flights_new$MONTH==10)
sample11<- subset(flights_new,flights_new$MONTH==11)
sample12 <- subset(flights_new,flights_new$MONTH==12)

# To create train and test data sets
set.seed(2805)
ind1 <- sample(2,nrow(sample1),replace = TRUE,prob = c(0.7,0.3))
sample1_train <- sample1[ind1== 1,]
sample1_test <- sample1[ind1 == 2,]

ind2 <- sample(2,nrow(sample2),replace = TRUE,prob = c(0.7,0.3))
sample2_train <- sample2[ind2== 1,]
sample2_test <- sample2[ind2 == 2,]

ind3 <- sample(2,nrow(sample3),replace = TRUE,prob = c(0.7,0.3))
sample3_train <- sample3[ind3== 1,]
sample3_test <- sample3[ind3 == 2,]

ind4 <- sample(2,nrow(sample4),replace = TRUE,prob = c(0.7,0.3))
sample4_train <- sample4[ind4== 1,]
sample4_test <- sample4[ind4 == 2,]

ind5 <- sample(2,nrow(sample5),replace = TRUE,prob = c(0.7,0.3))
sample5_train <- sample5[ind5== 1,]
sample5_test <- sample5[ind5 == 2,]

ind6 <- sample(2,nrow(sample6),replace = TRUE,prob = c(0.7,0.3))
sample6_train <- sample6[ind6== 1,]
sample6_test <- sample6[ind6 == 2,]

ind7 <- sample(2,nrow(sample7),replace = TRUE,prob = c(0.7,0.3))
sample7_train <- sample7[ind7== 1,]
sample7_test <- sample7[ind7 == 2,]

ind8 <- sample(2,nrow(sample8),replace = TRUE,prob = c(0.7,0.3))
sample8_train <- sample8[ind8== 1,]
sample8_test <- sample8[ind8 == 2,]

ind9 <- sample(2,nrow(sample9),replace = TRUE,prob = c(0.7,0.3))
sample9_train <- sample9[ind9== 1,]
sample9_test <- sample9[ind9 == 2,]

ind10 <- sample(2,nrow(sample10),replace = TRUE,prob = c(0.7,0.3))
sample10_train <- sample10[ind10== 1,]
sample10_test <- sample10[ind10 == 2,]

ind11 <- sample(2,nrow(sample11),replace = TRUE,prob = c(0.7,0.3))
sample11_train <- sample11[ind11== 1,]
sample11_test <- sample11[ind11 == 2,]

ind12 <- sample(2,nrow(sample12),replace = TRUE,prob = c(0.7,0.3))
sample12_train <- sample12[ind12== 1,]
sample12_test <- sample12[ind12 == 2,]

final_train <- rbind(sample1_train,sample2_train,sample3_train,sample4_train,sample5_train,sample6_train,sample7_train,sample8_train,sample9_train,sample10_train,sample11_train,sample12_train)
final_test <- rbind(sample1_test,sample2_test,sample3_test,sample4_test,sample5_test,sample6_test,sample7_test,sample8_test,sample9_test,sample10_test,sample11_test,sample12_test)
#write.csv(final_train,file = "final_train.csv")
#write.csv(final_test,file = "final_test.csv")
colnames(final_train)
prop.table(table(final_train$DEPARTURE_DELAY_CLASS)) #81% and 19%
prop.table(table(final_test$DEPARTURE_DELAY_CLASS)) #81% and 19%
trail1_train <- final_train[c(1:4,9:11,14,17:21,25,26,28,30,8)] 
trail1_test <- final_test[c(1:4,9:11,14,17:21,25,26,28,30,8)] 
#trail1 <- rbind(trail1_train,trail1_test)
str(trail1_train)
summary(trail1_train)
model_mlr1 <- lm(trail1_train$DEPARTURE_DELAY~.,data = trail1_train)
summary(model_mlr1)
prediction_mlr1 <- predict(model_mlr1,trail1_test)
mse_mlr1 <-mean((prediction_mlr1-trail1_test$DEPARTURE_DELAY)^2) 
mse_mlr1
res <- data.frame(prediction_mlr1-trail1_test$DEPARTURE_DELAY)
newtable <- data.frame(prediction_mlr1,trail1_test$DEPARTURE_DELAY)
newtable$prediction_class <- ifelse(newtable$prediction_mlr1<15,0,1)
newtable$test_class <- ifelse(newtable$trail1_test.DEPARTURE_DELAY<15,0,1)
table(newtable$prediction_class,newtable$test_class)
(1388714+242315)/1714543 # accuracy: 95.13%
trail2_train <- final_train[c(2:3,9:11,17:21,25,8)]
trail2_test <- final_test[c(2:3,9:11,17:21,25,8)]
#trail2 <- rbind(trail2_train,trail2_test)
str(trail2_train)
#summary(trail2)
model_mlr2 <- lm(trail2$DEPARTURE_DELAY~.,data = trail2)
summary(model_mlr2)
prediction_mlr2 <- predict(model_mlr2,trail2_test)
mse_mlr2 <-mean((prediction_mlr2-trail2_test$DEPARTURE_DELAY)^2) 
sqrt(mse_mlr2)
# plot(model_mlr2$fitted.values,trail2$DEPARTURE_DELAY)

trail3_train <- final_train[c(1:4,9:11,14,17:21,25,26,28,30,8)]
trail3_test <- final_test[c(1:4,9:11,14,17:21,25,26,28,30,8)]
trail3 <- rbind(trail3_train,trail3_test)
trail3_train$NEW_DEPARTURE_DELAY <- ifelse(trail3_train$DEPARTURE_DELAY>=0,trail3_train$DEPARTURE_DELAY,0)
trail3_test$NEW_DEPARTURE_DELAY <- ifelse(trail3_test$DEPARTURE_DELAY>=0,trail3_test$DEPARTURE_DELAY,0)
trail3_train <- trail3_train[-c(18)]
trail3_test <- trail3_test[-c(18)]
trail3$MONTH <- as.factor(trail3$MONTH)
model_mlr3 <- lm(trail3_train$NEW_DEPARTURE_DELAY~.,data = trail3_train)
summary(model_mlr3)
prediction_mlr3 <- predict(model_mlr3,trail3_test)
mse_mlr3 <-mean((prediction_mlr3-trail3_test$NEW_DEPARTURE_DELAY)^2) 
mse_mlr3
newtable <- data.frame(prediction_mlr3,trail3_test$NEW_DEPARTURE_DELAY)
newtable$prediction_class <- ifelse(newtable$prediction_mlr3<10,0,1)
newtable$test_class <- ifelse(newtable$trail3_test.NEW_DEPARTURE_DELAY<10,0,1)
table(newtable$prediction_class,newtable$test_class)
(1271649+274689)/1714543
trail4_train <- final_train[c(1:4,9:11,14,17:21,25,28,30,8)]
trail4_test <- final_test[c(1:4,9:11,14,17:21,25,28,30,8)]
trail4 <- rbind(trail4_train,trail4_test)
trail4$NEW_DEPARTURE_DELAY <- ifelse(trail4$DEPARTURE_DELAY>=0,trail4$DEPARTURE_DELAY,0)
trail4 <- trail4[-c(17)]
trail4$MONTH <- as.factor(trail4$MONTH)
trail4$DAY_OF_WEEK <- as.factor(trail4$DAY_OF_WEEK)
str(trail4)
summary(trail4)
model_mlr4 <- lm(trail4$NEW_DEPARTURE_DELAY~.,data = trail4)
summary(model_mlr4)

# LOGISTIC REGRESSION
trail6_train <- final_train[c(1:4,9:11,14,17:21,25,26,28,30,8)]
trail6_test <- final_test[c(1:4,9:11,14,17:21,25,26,28,30,8)]
trail6_train$DEPARTURE_DELAY_CLASS <- ifelse(trail6_train$DEPARTURE_DELAY<15,0,1)
trail6_test$DEPARTURE_DELAY_CLASS <- ifelse(trail6_test$DEPARTURE_DELAY<15,0,1)
trail6_train <- trail6_train[-c(18)]
trail6_test <- trail6_test[-c(18)]
model_logr <- glm(trail6_train$DEPARTURE_DELAY_CLASS~.,data = trail6_train) 
summary(model_logr)
exp(coef(model_logr))
prediction_logr <- predict(model_logr,trail6_test)
prediction_logr <- ifelse(prediction_logr<0.5,0,1)
table(prediction_logr,trail6_test$DEPARTURE_DELAY_CLASS)
(1398642+98244)/1714543 # 87.3%

#decision tree
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
trail7_train <- final_train[c(1:4,10,13,25,26,28,30,8)]
trail7_test <- final_test[c(1:4,10,13,25,26,28,30,8)]
#trail7_train <- final_train[c(1:4,9:11,14,17:21,25,26,28,30,8)]
#trail7_test <- final_test[c(1:4,9:11,14,17:21,25,26,28,30,8)]
#trail7_train <- final_train[c(1,4,28,30,8)]
#trail7_test <- final_test[c(1,4,28,30,8)]
#trail7_train <- final_train[-c(16,22:30)]
#trail7_test <- final_test[-c(16,22:30)]
str(trail7_train)
model_decisiontree <- rpart(DEPARTURE_DELAY~.,data=trail7_train,method = "anova")
summary(model_decisiontree)
rpart.plot(model_decisiontree,digits = 3,type = 3,fallen.leaves = TRUE)
printcp(model_decisiontree)
prediction_decisiontree <- predict(model_decisiontree,trail7_test)
mean(abs(trail7_test$DEPARTURE_DELAY-prediction_decisiontree))
sqrt(mean((trail7_test$DEPARTURE_DELAY-prediction_decisiontree)^2))
# xgboost
library(fastDummies)
#install.packages("mlr")
library(mlr)
#install.packages("xgboost")
library(xgboost)
dummy <- dummy_cols(trail7_train[4])
dummy <- dummy[-1]
trail7_train <- cbind(trail7_train,dummy)
dummy <- dummy_cols(trail7_train[9])
dummy <- dummy[-1]
trail7_train <- cbind(trail7_train,dummy)
dummy <- dummy_cols(trail7_train[10])
dummy <- dummy[-1]
trail7_train <- cbind(trail7_train,dummy)
trail7_train <- trail7_train[-c(4,9,10)]
str(trail7_train)
dummy <- dummy_cols(trail7_test[4])
dummy <- dummy[-1]
trail7_test <- cbind(trail7_test,dummy)
dummy <- dummy_cols(trail7_test[9])
dummy <- dummy[-1]
trail7_test <- cbind(trail7_test,dummy)
dummy <- dummy_cols(trail7_test[10])
dummy <- dummy[-1]
trail7_test <- cbind(trail7_test,dummy)
trail7_test <- trail7_test[-c(4,9,10)]
trail7_test <- trail7_test[c(1:8,14,10,9,18,11,12,17,13,16,15,19,20,21,22,24,23,25,26,28,27,29,30)]
str(trail7_test)
setDT(trail7_train)
setDT(trail7_test)
target <- trail7_train$DEPARTURE_DELAY
trail7_train <- trail7_train[,-8]
trail7_train <- as.matrix(trail7_train)
dtrain <- xgb.DMatrix(data=trail7_train,label=target)
target_test <- trail7_test$DEPARTURE_DELAY
trail7_test <- trail7_test[,-8]
trail7_test <- as.matrix(trail7_test)
dtest <- xgb.DMatrix(data = trail7_test,label=target_test)
colnames(dtrain)
colnames(dtest)
params <- list(booster = "gbtree",objective = "reg:linear",eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 30, nfold = 5, showsd = T, stratified = T, print_every_n = 1, early_stop_rounds = 20, maximize = F)
#xgbcv$nfeatures
#xgbcv$best_iteration
summary(xgbcv)
xgbcv$niter
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 20, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_rounds = 10, maximize = F)
xgbpred <- predict (xgb1,dtest)
error1 <- mean((xgbpred-target_test)^2)
summary(xgbpred-target_test)
sqrt(error1)
table1 <- data.frame(xgbpred,target_test)


# xgboost with different variables
trail8_train <- final_train[c(1:4,25,27,28,23)]
trail8_test <- final_test[c(1:4,25,27,28,23)]
str(trail8_train)
trail8_train <- data.matrix(trail8_train)
trail8_train <- data.frame(trail8_train)
str(trail8_train)
trail8_test <- data.matrix(trail8_test)
trail8_test <- data.frame(trail8_test)
str(trail8_test)
setDT(trail8_train)
setDT(trail8_test)
target <- trail8_train$DEPARTURE_DELAY_CLASS
trail8_train <- trail8_train[,-8]
trail8_train <- as.matrix(trail8_train)
dtrain <- xgb.DMatrix(data=trail8_train,label=target)

target_test <- trail8_test$DEPARTURE_DELAY_CLASS
trail8_test <- trail8_test[,-8]
trail8_test <- as.matrix(trail8_test)
dtest <- xgb.DMatrix(data = trail8_test,label=target_test)

params <- list(booster = "gbtree",objective = "reg:linear",eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_rounds = 20, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 100, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_rounds = 10, maximize = F)
xgbpred <- predict (xgb1,dtest)
table1 <- data.frame(xgbpred,target_test) # no improvement with the parameters considered in this model

#neural network
library(neuralnet)
normalize <- function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
trail9_train <- final_train[c(1:6,25,10,13,8)]
str(trail9_train)
trail9_test <- final_test[c(1:6,25,10,13,8)]
trail9_train <- data.matrix(trail9_train)
trail9_train <- data.frame(trail9_train)
trail9_test <- data.matrix(trail9_test)
trail9_test <- data.frame(trail9_test)
final_train9_norm <- as.data.frame(lapply(trail9_train[-10], FUN = normalize))
final_test9_norm <- as.data.frame(lapply(trail9_test[-10], FUN = normalize))  
final_train9_norm$DELAY <- trail9_train$DEPARTURE_DELAY
final_test9_norm$DELAY <- trail9_test$DEPARTURE_DELAY
variables<- paste("DELAY",paste(colnames(final_train9_norm[-10]),collapse='+'),sep = '~')
model_nn <- neuralnet(formula = variables,data=final_train9_norm,stepmax = 1e+08)
# xg boost with original variables
trail10_train <- final_train[c(1:6,10,13,25,17:21,8)]
trail10_test <- final_test[c(1:6,10,13,25,17:21,8)]
trail10_train <- data.matrix(trail10_train)
trail10_train <- data.frame(trail10_train)
trail10_test <- data.matrix(trail10_test)
trail10_test <- data.frame(trail10_test)
setDT(trail10_train)
setDT(trail10_test)
target <- trail10_train$DEPARTURE_DELAY
trail10_train <- trail10_train[,-15]
trail10_train <- as.matrix(trail10_train)
dtrain <- xgb.DMatrix(data=trail10_train,label=target)
target_test <- trail10_test$DEPARTURE_DELAY
trail10_test <- trail10_test[,-15]
trail10_test <- as.matrix(trail10_test)
dtest <- xgb.DMatrix(data = trail10_test,label=target_test)
params <- list(booster = "gbtree",objective = "reg:linear",eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 40, nfold = 5, showsd = T, stratified = T, print_every_n = 2, early_stop_rounds = 20, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 30, watchlist = list(val=dtest,train=dtrain), print_every_n = 2, early_stop_rounds = 10, maximize = F)
xgbpred <- predict (xgb1,dtest)
table1 <- data.frame(xgbpred,target_test)
summary(xgbpred-target_test)

# xg boost with original variables without 17:21
trail10_train <- final_train[c(1:6,10,13,19,20,25,8)]
trail10_test <- final_test[c(1:6,10,13,19,20,25,8)]
trail10_train <- data.matrix(trail10_train)
trail10_train <- data.frame(trail10_train)
trail10_test <- data.matrix(trail10_test)
trail10_test <- data.frame(trail10_test)
setDT(trail10_train)
setDT(trail10_test)
target <- trail10_train$DEPARTURE_DELAY
trail10_train <- trail10_train[,-12]
trail10_train <- as.matrix(trail10_train)
dtrain <- xgb.DMatrix(data=trail10_train,label=target)
target_test <- trail10_test$DEPARTURE_DELAY
trail10_test <- trail10_test[,-12]
trail10_test <- as.matrix(trail10_test)
dtest <- xgb.DMatrix(data = trail10_test,label=target_test)
params <- list(booster = "gbtree",objective = "reg:linear",eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 50, nfold = 5, showsd = T, stratified = T, print_every_n = 2, early_stop_rounds = 20, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 30, watchlist = list(val=dtest,train=dtrain), print_every_n = 2, early_stop_rounds = 10, maximize = F)
xgbpred <- predict (xgb1,dtest)
table1 <- data.frame(xgbpred,target_test)
summary(xgbpred-target_test)
# random forest regressor
trail11_train <- sample10_train[c(1:4,28,30,10,13,25,17:21,8)]
trail11_test <- sample10_test[c(1:4,28,30,10,13,25,17:21,8)]
#write.csv(trail11_train,file = "trail11_train.csv")
#write.csv(trail11_test,file = "trail11_test.csv")
install.packages("randomForest")
library(randomForest)
modelrf <- randomForest(trail11_test[,-15],trail11_test$DEPARTURE_DELAY,na.action=na.roughfix,importance=TRUE)
modelrf
predictionrf <- predict(modelrf,trail11_test[,-15])

# xgboost with delays considered (2 no.)
trail12_train <- final_train[c(1:6,10,13,19,20,25,26,8)]
trail12_test <- final_test[c(1:6,10,13,19,20,25,26,8)]
str(trail12_train)
trail12_train <- data.matrix(trail12_train)
trail12_train <- data.frame(trail12_train)
str(trail12_train)
trail12_test <- data.matrix(trail12_test)
trail12_test <- data.frame(trail12_test)
str(trail12_test)
setDT(trail12_train)
setDT(trail12_test)
target <- trail12_train$DEPARTURE_DELAY
trail12_train <- trail12_train[,-13]
trail12_train <- as.matrix(trail12_train)
dtrain <- xgb.DMatrix(data=trail12_train,label=target)

target_test <- trail12_test$DEPARTURE_DELAY
trail12_test <- trail12_test[,-13]
trail12_test <- as.matrix(trail12_test)
dtest <- xgb.DMatrix(data = trail12_test,label=target_test)

params <- list(booster = "gbtree",objective = "reg:linear",eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 70, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_rounds = 20, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 70 , watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_rounds = 10, maximize = F)
xgbpred <- predict (xgb1,dtest)
table1 <- data.frame(xgbpred,target_test)
#table1$pred_delay <- ifelse(table1$xgbpred<15,0,1)
#table1$actual_delay <- ifelse(table1$target_test<15,0,1)
#table1$accuracy <- table1$pred_delay-table1$actual_delay
#table(table1$accuracy)
summary(xgbpred-target_test)

# classification model
library(caret)
library(caretEnsemble)
library(mice)
library(doParallel)
library(car)
library(klaR)
trail15_train <- final_train[c(1:7,10,13,15,23)]
trail15_test <- final_test[c(1:7,10,13,15,23)]
trail15_train$AIRLINE <- as.numeric(trail15_train$AIRLINE)
trail15_train$ORIGIN_AIRPORT <- as.numeric(trail15_train$ORIGIN_AIRPORT)
trail15_train$DESTINATION_AIRPORT <- as.numeric(trail15_train$DESTINATION_AIRPORT)
trail15_train$DEPARTURE_DELAY_CLASS <- as.factor (trail15_train$DEPARTURE_DELAY_CLASS)
trail15_train$DEPARTURE_DELAY_CLASS <- ifelse(trail15_train$DEPARTURE_DELAY_CLASS==1,"yes","no")
str(trail15_train)
prop.table(table(trail15_train$DEPARTURE_DELAY_CLASS)) # 81.6,18.4
prop.table(table(trail15_test$DEPARTURE_DELAY_CLASS)) # 81.6,18.4
registerDoParallel(3)
getDoParWorkers()
set.seed(123)
my_ctrl <- trainControl(method = "cv", 
                        number = 5,
                        classProbs = TRUE,
                        savePredictions = "final",
                        index = 
                          createResample(trail15_train$DEPARTURE_DELAY_CLASS, 3),
                        sampling = "up",
                        allowParallel = TRUE)

model_list <- caretList(DEPARTURE_DELAY_CLASS ~ .,
                        data = trail15_train,
                        methodList = c("nb"),
                        metric = "Kappa",
                        tuneList = NULL,
                        continue_on_fail = FALSE,  
                        preProcess = c("center", "scale"),
                        trControl = my_ctrl)

trail15_test$AIRLINE <- as.numeric(trail15_test$AIRLINE)
trail15_test$ORIGIN_AIRPORT <- as.numeric(trail15_test$ORIGIN_AIRPORT)
trail15_test$DESTINATION_AIRPORT <- as.numeric(trail15_test$DESTINATION_AIRPORT)
trail15_test$DEPARTURE_DELAY_CLASS <- as.factor (trail15_test$DEPARTURE_DELAY_CLASS)
trail15_test$DEPARTURE_DELAY_CLASS <- ifelse(trail15_test$DEPARTURE_DELAY_CLASS==1,"yes","no")
#Logistic Regression model
confusionMatrix(predict(model_list$glm,trail15_test, type = "raw"), test_data_imp$class)
#Naive Bayes model
confusionMatrix(predict(model_list$nb,test_data_imp, type = "raw"), test_data_imp$class)

#catboost models

#install.packages('devtools')
#devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
library(catboost)
# catboost with original variables
trail12_train <- final_train[c(1:7,10,13,15,8)]
trail12_test <- final_test[c(1:7,10,13,15,8)]
target <- trail12_train$DEPARTURE_DELAY
trail12_train <- trail12_train[,-11]
target_test <- trail12_test$DEPARTURE_DELAY
trail12_test <- trail12_test[,-11]
str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target,cat_features = c(4,5,6))
fit_params <- list(iterations=50,
                   learning_rate=0.01,
                   depth=16,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   boosting_type='Plain',
                   one_hot_max_size = 628,
                   l2_leaf_reg=1,
                   use_best_model=TRUE)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test)
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(target_test,prediction)
summary(target_test-prediction)
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)
save(model,file='model.rda')

# catboost with modified schedule and departure times

trail12_train <- final_train[c(1:6,25,26,10,13,8)]
trail12_test <- final_test[c(1:6,25,26,10,13,8)]
target <- trail12_train$DEPARTURE_DELAY
trail12_train <- trail12_train[,-11]
target_test <- trail12_test$DEPARTURE_DELAY
trail12_test <- trail12_test[,-11]
str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target,cat_features = c(3,4,5))
fit_params <- list(iterations=50,
                   learning_rate=0.01,
                   depth=16,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   boosting_type='Plain',
                   one_hot_max_size = 628,
                   l2_leaf_reg=1,
                   use_best_model=TRUE)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test,cat_features = c(3,4,5))
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(target_test,prediction)
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)

# catboost with modified origin airport,destination airport,schedule and departure times

trail12_train <- final_train[c(1:4,28,30,25,26,10,13,8)]
trail12_test <- final_test[c(1:4,28,30,25,26,10,13,8)]
target <- trail12_train$DEPARTURE_DELAY
trail12_train <- trail12_train[,-11]
target_test <- trail12_test$DEPARTURE_DELAY
trail12_test <- trail12_test[,-11]
str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target,cat_features = c(4))
fit_params <- list(iterations=50,
                   learning_rate=0.02,
                   depth=16,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   boosting_type='Plain',
                   one_hot_max_size = 13,
                   l2_leaf_reg=1,
                   use_best_model=TRUE)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test,cat_features = c(4))
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(target_test,prediction)
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)
# catboost with modified origin airport freq,destination airport freq,schedule and departure times

trail12_train <- final_train[c(1:4,27,29,25,26,10,13,8)]
trail12_test <- final_test[c(1:4,27,29,25,26,10,13,8)]
target <- trail12_train$DEPARTURE_DELAY
trail12_train <- trail12_train[,-11]
target_test <- trail12_test$DEPARTURE_DELAY
trail12_test <- trail12_test[,-11]
str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target,cat_features = c(4))
fit_params <- list(iterations=50,
                   learning_rate=0.02,
                   depth=16,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   boosting_type='Plain',
                   one_hot_max_size = 13,
                   l2_leaf_reg=1,
                   use_best_model=TRUE)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test,cat_features = c(4))
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(target_test,prediction)
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)
# catboost with modified airport,schedule and departure times

trail12_train <- final_train[c(1:4,27:30,25,26,10,13,8)]
trail12_test <- final_test[c(1:4,27:30,25,26,10,13,8)]
target <- trail12_train$DEPARTURE_DELAY
trail12_train <- trail12_train[,-13]
target_test <- trail12_test$DEPARTURE_DELAY
trail12_test <- trail12_test[,-13]
str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target,cat_features = c(4))
fit_params <- list(iterations=100,
                   learning_rate=0.01,
                   depth=16,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   boosting_type='Plain',
                   one_hot_max_size = 13,
                   l2_leaf_reg=1)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test,cat_features = c(4))
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(target_test,prediction)
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)
# catboost with original variables as numerical variables and no categorical variables
trail12_train <- final_train[c(1:7,10,13,15,8)]
trail12_test <- final_test[c(1:7,10,13,15,8)]
trail12_train[] <- lapply(trail12_train, as.numeric)
trail12_test[] <- lapply(trail12_test, as.numeric)
#str(trail12_test)
target <- trail12_train$DEPARTURE_DELAY
trail12_train <- trail12_train[,-11]
target_test <- trail12_test$DEPARTURE_DELAY
trail12_test <- trail12_test[,-11]
#str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target)
fit_params <- list(iterations=100,
                   learning_rate=0.01,
                   depth=16,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   l2_leaf_reg=1,
                   use_best_model=TRUE)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test)
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(target_test,prediction)
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)
# catboost with original variables as numerical variables and no categorical variables(flight no and tail included)
trail12_train <- final_train[c(1:3,24,5,7,8,28,29,12,15,10)]
trail12_test <- final_test[c(1:3,24,5,7,8,28,29,12,15,10)]
trail12_train[] <- lapply(trail12_train, as.numeric)
trail12_test[] <- lapply(trail12_test, as.numeric)
str(trail12_train)
target <- trail12_train$DEPARTURE_DELAY
trail12_train <- trail12_train[,-12]
target_test <- trail12_test$DEPARTURE_DELAY
trail12_test <- trail12_test[,-12]
#str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target)
fit_params <- list(iterations=50,
                   learning_rate=0.01,
                   depth=16,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   l2_leaf_reg=1,
                   use_best_model=TRUE)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test)
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(target_test,prediction)
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)

# catboost class problem
trail12_train <- final_train[c(1:6,25,26,10,13,23)]
trail12_test <- final_test[c(1:6,25,26,10,13,23)]
colnames(trail12_train)
target <- trail12_train$DEPARTURE_DELAY_CLASS
trail12_train <- trail12_train[,-11]
target_test <- trail12_test$DEPARTURE_DELAY_CLASS
trail12_test <- trail12_test[,-11]
str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target,cat_features = c(4,5,6))
fit_params <- list(iterations=50,
                   learning_rate=0.01,
                   depth=16,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   boosting_type='Plain',
                   one_hot_max_size = 628,
                   l2_leaf_reg=1,
                   use_best_model=TRUE)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test,cat_features = c(4,5,6))
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(target_test,prediction)
table2$pred_class <- ifelse(table2$prediction<0.24,0,1) #81% accuracy in class
table(table2$target_test,table2$pred_class)
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)

# catboost exponential model
trail12_train <- final_train[c(1:6,25,26,10,13,8)]
trail12_test <- final_test[c(1:6,25,26,10,13,8)]
trail12_train$DEPARTURE_DELAY <- ifelse(trail12_train$DEPARTURE_DELAY<=0,1,trail12_train$DEPARTURE_DELAY)
target <- log(trail12_train$DEPARTURE_DELAY)
trail12_train <- trail12_train[,-11]
trail12_test$DEPARTURE_DELAY <- ifelse(trail12_test$DEPARTURE_DELAY<=0,1,trail12_test$DEPARTURE_DELAY)
target_test <- log(trail12_test$DEPARTURE_DELAY)
trail12_test <- trail12_test[,-11]
str(trail12_test)
train_pool <- catboost.load_pool(data = trail12_train, label = target,cat_features = c(4,5,6))
fit_params <- list(iterations=30,
                   learning_rate=0.01,
                   depth=12,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 10,
                   od_wait=20,
                   boosting_type='Plain',
                   one_hot_max_size = 628,
                   l2_leaf_reg=1,
                   use_best_model=TRUE)
model <- catboost.train(train_pool,params = fit_params)
real_pool <- catboost.load_pool(trail12_test,cat_features = c(4,5,6))
prediction <- catboost.predict(model, real_pool)
table2 <- data.frame(exp(target_test),exp(prediction))
catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)
summary(table2$exp.prediction.-table2$exp.target_test.)



