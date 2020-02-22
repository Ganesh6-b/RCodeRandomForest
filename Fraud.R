setwd("F://R//Rfiles")
Frauddata <- read.csv("Fraud_check.csv")
View(Frauddata)

#checking na values
sum(is.na(Frauddata))

#converting numerical output to catagorical variable
Frauddata$Taxable.Income <- cut(Frauddata$Taxable.Income, breaks = c(10000, 30000, 100000), labels = c("risky", "Good"))
View(Frauddata)

#create partition of data

library(caret)

sample <- createDataPartition(Frauddata$Taxable.Income, p = 0.70, list = F)
Training <- Frauddata[sample,]
Testing <- Frauddata[-sample,]

#building a model
library(randomForest)
model1 <- randomForest(Training$Taxable.Income~. , data = Training, na.action = na.roughfix, importance = TRUE)
plot(model1)

#accuracy
mean(Training$Taxable.Income == predict(model1, Training)) #92% Training accuracy

mean(Testing$Taxable.Income == predict(model1, newdata = Testing)) #77% Testing accuracy

