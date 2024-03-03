#T-2 1 hidden layer
#1 input
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-2.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:499, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag1 is today
#Lag2 is tomorrow
load_model <- neuralnet(Lag2 ~ Lag1, data = load_train)
plot(load_model)  

load_model_2 <- neuralnet(Lag2 ~ Lag1, data = load_train, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2) 

model_results <- compute(load_model, load_test[1:1])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag2)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag2"] 
load_test_original_lag <- load[431:499,"Lag2"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)

rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)   



#T-2 2 hidden layer
#1 input
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-2.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:499, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag1 is today
#Lag2 is tomorrow
load_model <- neuralnet(Lag2 ~ Lag1, data = load_train, hidden = 2)
plot(load_model)


load_model_2 <- neuralnet(Lag2 ~ Lag1, data = load_train, hidden = 2,act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)


model_results <- compute(load_model, load_test[1:1])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag2)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag2"] 
load_test_original_lag <- load[431:499,"Lag2"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)

MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)

rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)





#T-2 3 hidden layer
#1 input
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-2.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:499, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag1 is today
#Lag2 is tomorrow
load_model <- neuralnet(Lag2 ~ Lag1, data = load_train, hidden = 3)
plot(load_model)  

load_model_2 <- neuralnet(Lag2 ~ Lag1, data = load_train, hidden = 3,act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)  


model_results <- compute(load_model, load_test[1:1])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag2)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag2"] 
load_test_original_lag <- load[431:499,"Lag2"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)

MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)

rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)   



#T-2 6 hidden layer
#1 input
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-2.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:499, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag1 is today
#Lag2 is tomorrow
load_model <- neuralnet(Lag2 ~ Lag1, data = load_train, hidden = 6)
plot(load_model)  

load_model_2 <- neuralnet(Lag2 ~ Lag1, data = load_train, hidden = 6,act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)


model_results <- compute(load_model, load_test[1:1])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag2)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag2"] 
load_test_original_lag <- load[431:499,"Lag2"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)

MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)

rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)   



#T-3 1 hidden layer
#2 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-3.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:498, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag1 is yesterday
#Lag2 is today
#Lag3 is tomorrow
load_model <- neuralnet(Lag3 ~ Lag1 + Lag2, data = load_train)
plot(load_model)  

load_model_2 <- neuralnet(Lag3 ~ Lag1 + Lag2, data = load_train, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2) 


model_results <- compute(load_model, load_test[1:2])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag3)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag3"] 
load_test_original_lag <- load[431:498,"Lag3"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#T-3 2 hidden layer
#2 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-3.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:498, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag1 is yesterday
#Lag2 is today
#Lag3 is tomorrow
load_model <- neuralnet(Lag3 ~ Lag1 + Lag2, data = load_train,hidden = 2)
plot(load_model)  

load_model_2 <- neuralnet(Lag3 ~ Lag1 + Lag2, data = load_train,hidden = 2, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)   


model_results <- compute(load_model, load_test[1:2])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag3)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag3"] 
load_test_original_lag <- load[431:498,"Lag3"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)



#T-3 3 hidden layer
#2 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-3.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:498, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag1 is yesterday
#Lag2 is today
#Lag3 is tomorrow
load_model <- neuralnet(Lag3 ~ Lag1 + Lag2, data = load_train, hidden = 3)
plot(load_model)  

load_model_2 <- neuralnet(Lag3 ~ Lag1 + Lag2, data = load_train,hidden = 3, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)   


model_results <- compute(load_model, load_test[1:2])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag3)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag3"] 
load_test_original_lag <- load[431:498,"Lag3"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)





#T-3 6 hidden layer
#2 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-3.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:498, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag1 is yesterday
#Lag2 is today
#Lag3 is tomorrow
load_model <- neuralnet(Lag3 ~ Lag1 + Lag2, data = load_train, hidden = 6)
plot(load_model)  

load_model_2 <- neuralnet(Lag3 ~ Lag1 + Lag2, data = load_train, hidden = 6, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)  


model_results <- compute(load_model, load_test[1:2])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag3)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag3"] 
load_test_original_lag <- load[431:498,"Lag3"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)





#T-4 1 hidden layer
#3 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-4.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag2 is yesterday
#Lag3 is today
#Lag4 is tomorrow
load_model <- neuralnet(Lag4 ~ Lag1+Lag2+Lag3, data = load_train)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1+Lag2+Lag3, data = load_train, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)


model_results <- compute(load_model, load_test[1:3])
predicted_lag <- model_results$net.result
#used to have lag4vv
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)    



#T-4 2 hidden layer
#3 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-4.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag2 is yesterday
#Lag3 is today
#Lag4 is tomorrow
load_model <- neuralnet(Lag4 ~ Lag1+Lag2+Lag3, data = load_train, hidden = 2)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1+Lag2+Lag3, data = load_train, hidden = 2, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2) 


model_results <- compute(load_model, load_test[1:3])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)



#T-4 3 hidden layer
#3 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-4.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

load_norm <- as.data.frame(lapply(load, normalize))
summary(load_norm)

#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag2 is yesterday
#Lag3 is today
#Lag4 is tomorrow
load_model <- neuralnet(Lag4 ~ Lag1+Lag2+Lag3, data = load_train, hidden = 3)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1+Lag2+Lag3, data = load_train, hidden = 3, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2) 


model_results <- compute(load_model, load_test[1:3])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)   



#T-4 6 hidden layer
#3 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-4.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag2 is yesterday
#Lag3 is today
#Lag4 is tomorrow
load_model <- neuralnet(Lag4 ~ Lag1+Lag2+Lag3, data = load_train, hidden = 6)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1+Lag2+Lag3, data = load_train, hidden = 6, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2) 


model_results <- compute(load_model, load_test[1:3])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)

MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)

rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)



#T-5 1 hidden layer
#4 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-5.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:496, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag3 is yesterday
#Lag4 is today
#Lag5 is tomorrow
load_model <- neuralnet(Lag5 ~ Lag1 + Lag2 + Lag3 +Lag4, data = load_train)
plot(load_model)  

load_model_2 <- neuralnet(Lag5 ~ Lag1 + Lag2 + Lag3 +Lag4, data = load_train, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)  


model_results <- compute(load_model, load_test[1:4])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag5)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag5"] 
load_test_original_lag <- load[431:496,"Lag5"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)



#T-5 2 hidden layer
#4 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-5.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:496, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag3 is yesterday
#Lag4 is today
#Lag5 is tomorrow
load_model <- neuralnet(Lag5 ~ Lag1 + Lag2 + Lag3 +Lag4, data = load_train, hidden=2)
plot(load_model)  

load_model_2 <- neuralnet(Lag5 ~ Lag1 + Lag2 + Lag3 +Lag4, data = load_train, hidden=2, linear.output = FALSE)
plot(load_model_2)  
 


model_results <- compute(load_model, load_test[1:4])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag5)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag5"] 
load_test_original_lag <- load[431:496,"Lag5"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)



#T-5 3 hidden layer
#4 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-5.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:496, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag3 is yesterday
#Lag4 is today
#Lag5 is tomorrow
load_model <- neuralnet(Lag5 ~ Lag1 + Lag2 + Lag3 +Lag4, data = load_train, hidden=3)
plot(load_model)  

load_model_2 <- neuralnet(Lag5 ~ Lag1 + Lag2 + Lag3 +Lag4, data = load_train, hidden=3, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)  


model_results <- compute(load_model, load_test[1:4])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag5)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag5"] 
load_test_original_lag <- load[431:496,"Lag5"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#T-5 6 hidden layer
#4 inputs
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/t-5.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:496, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag3 is yesterday
#Lag4 is today
#Lag5 is tomorrow
load_model <- neuralnet(Lag5 ~ Lag1 + Lag2 + Lag3 +Lag4, data = load_train, hidden=6)
plot(load_model)  

load_model_2 <- neuralnet(Lag5 ~ Lag1 + Lag2 + Lag3 +Lag4, data = load_train, hidden=6, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)   


model_results <- compute(load_model, load_test[1:4])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag5)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag5"] 
load_test_original_lag <- load[431:496,"Lag5"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#T-7 1 hidden layer
#7 inputs
library(readxl)
load <- data.frame(read_excel("C:/Users/laptop/Desktop/ML-CWK/t-7.xlsx"))
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:493, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag6 is yesterday
#Lag7 is today
#Lag8 is tomorrow
load_model <- neuralnet(Lag8 ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 + Lag7, data = load_train)
plot(load_model)

load_model_2 <- neuralnet(Lag8 ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 + Lag7, data = load_train, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)


model_results <- compute(load_model, load_test[1:7])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag8)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag7"] 
load_test_original_lag <- load[431:493,"Lag7"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)



#T-7 2 hidden layer
#7 inputs
library(readxl)
load <- data.frame(read_excel("C:/Users/laptop/Desktop/ML-CWK/t-7.xlsx"))
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:493, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag6 is yesterday
#Lag7 is today
#Lag8 is tomorrow
load_model <- neuralnet(Lag8 ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 + Lag7, data = load_train, hidden=2)
plot(load_model)  

load_model_2 <- neuralnet(Lag8 ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 + Lag7, data = load_train, hidden=2, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)   


model_results <- compute(load_model, load_test[1:7])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag8)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag8"] 
load_test_original_lag <- load[431:493,"Lag8"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred  



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#T-7 3 hidden layer
#7 inputs
library(readxl)
load <- data.frame(read_excel("C:/Users/laptop/Desktop/ML-CWK/t-7.xlsx"))
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:493, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag6 is yesterday
#Lag7 is today
#Lag8 is tomorrow
load_model <- neuralnet(Lag8 ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 + Lag7, data = load_train, hidden=3)
plot(load_model)  

load_model_2 <- neuralnet(Lag8 ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 + Lag7, data = load_train, hidden=3, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2) 


model_results <- compute(load_model, load_test[1:7])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag8)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag8"] 
load_test_original_lag <- load[431:493,"Lag8"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred  


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)



#T-7 6 hidden layer
#7 inputs
library(readxl)
load <- data.frame(read_excel("C:/Users/laptop/Desktop/ML-CWK/t-7.xlsx"))
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:493, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag6 is yesterday
#Lag7 is today
#Lag8 is tomorrow
load_model <- neuralnet(Lag8 ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 + Lag7, data = load_train, hidden=6)
plot(load_model)  

load_model_2 <- neuralnet(Lag8 ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 + Lag7, data = load_train, hidden=6, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)   


model_results <- compute(load_model, load_test[1:7])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag8)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag8"] 
load_test_original_lag <- load[431:493,"Lag8"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred  


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#NARX Model with 1 hidden layer
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/NARX.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag5 is 10:00 in this dataset
#Lag6 is 09:00 in this dataset
load_model <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2) 


model_results <- compute(load_model, load_test[1:5])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   


MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#NARX Model with 3 hidden layers
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/NARX.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag5 is 10:00 in this dataset
#Lag6 is 09:00 in this dataset
load_model <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train, hidden = 3)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train,hidden = 3, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)  


model_results <- compute(load_model, load_test[1:5])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#NARX Model with 5 hidden layers
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/NARX.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag5 is 10:00 in this dataset
#Lag6 is 09:00 in this dataset
load_model <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train, hidden = 5)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train,hidden = 5, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)  


model_results <- compute(load_model, load_test[1:5])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#NARX Model with 7 hidden layers
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/NARX.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag5 is 10:00 in this dataset
#Lag6 is 09:00 in this dataset
load_model <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train, hidden = 7)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train,hidden = 7, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2)   


model_results <- compute(load_model, load_test[1:5])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)




#NARX Model with 9 hidden layers
library(readxl)
load <- read_excel("C:/Users/laptop/Desktop/ML-CWK/NARX.xlsx")
str(load)
summary(load) 

library(ggplot2)
library(reshape2)
library(gridExtra)
load_consumption = melt(load)

tail(load_consumption)

qplot(x=value, data=load_consumption) + facet_wrap(~variable, scales='free')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
load_norm <- as.data.frame(lapply(load, normalize))

summary(load_norm)   


#Training
load_train <- load_norm[1:430, ]
load_test <- load_norm[431:497, ]

library(neuralnet)
library(grid)
library(MASS)
set.seed(12345) 

#Lag5 is 10:00 in this dataset
#Lag6 is 09:00 in this dataset
load_model <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train, hidden = 9)
plot(load_model)

load_model_2 <- neuralnet(Lag4 ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag6, data = load_train,hidden = 9, act.fct = "tanh",linear.output = FALSE)
plot(load_model_2) 


model_results <- compute(load_model, load_test[1:5])
predicted_lag <- model_results$net.result
cor(predicted_lag, load_test$Lag4)


# producing actual predictions

head(predicted_lag)

load_train_original_lag <- load[1:430,"Lag4"] 
load_test_original_lag <- load[431:497,"Lag4"] 


lag_min <- min(load_train_original_lag)
lag_max <- max(load_test_original_lag)

head(load_train_original_lag)


unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


lag_pred <- unnormalize(predicted_lag, lag_min, lag_max)
lag_pred   



MAPEtest <- function(error, load_test_original_lag)
{
  newData <- error/ load_test_original_lag
  newData <- abs(newData)
  dataMean = lapply(newData, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAPEtest(error,load_test_original_lag)


MAEtest <- function(error)
{
  dataAbs <- abs(error)
  dataMean = lapply(dataAbs, mean, na.rm = TRUE)
  return(dataMean)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
MAEtest(error)


rmse <- function(error)#sqrt(mean(error^2))
{
  data = error^(2)
  dataMean = lapply(data, mean, na.rm = TRUE)
  dataSQRT = lapply(dataMean,sqrt)
  return(dataSQRT)
}
error<-(as.data.frame(load_test_original_lag ) - lag_pred)
rmse(error)


