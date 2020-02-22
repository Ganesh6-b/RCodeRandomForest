setwd("F://R//Rfiles")
data(iris)
View(iris)
iris
#partition of data
library(caret)
sample <- createDataPartition(iris$Species, p = 0.75, list = F)

Training <- iris[sample,]
Testing <- iris[-sample,]

#model building
install.packages("randomForest")
library(randomForest)
forest <- randomForest(Training$Species~., data = Training)
attributes(forest)
plot(forest)
#accuracy
mean(Training$Species == predict(forest, Training)) #training accuracy

predict_test <- predict(forest, newdata = Testing)
mean(Testing$Species == predict_test) #testing accuracy
