setwd("F://R//Rfiles")
companydata <- read.csv("Company_Data.csv")
View(companydata)
range(companydata$Sales)

#checking na values
sum(is.na(companydata)) 
library(gam)
companydata <- na.gam.replace(companydata)

#converting numerical output to catagorical output
companydata$Sales <- cut(companydata$Sales, breaks = c(0,8,17), labels = c("low", "high"))

#partition of data
library(caret)
sample <- createDataPartition(companydata$Sales, p=0.70, list = FALSE)
?createDataPartition
Training <- companydata[sample,]
Testing <- companydata[-sample,]

#model building
library(randomForest)
model1 <- randomForest(Training$Sales~., data = Training, na.action = na.roughfix, importance = TRUE) 
plot(model1)

#accuracy
table(Training$Sales , predict(model1, Training)) #training accuracy is 100%

a <- table(Testing$Sales, predict(model1, Testing))
acc <- sum(diag(a))/ sum(a)
acc #testing accuracy is 84%
