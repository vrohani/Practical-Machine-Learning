# Needed Library

library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)

# Loading  and getting the datasets
set.seed(12345)

train <- read.csv("C:\\Courses\\Machine Learning\\Project\\pml-training.csv")
test <-  read.csv("C:\\Courses\\Machine Learning\\Project\\pml-testing.csv")

# Partitioning the training data to two sets (train and test)
trainingdata <- createDataPartition(train$classe, p=0.6, list=FALSE)
training <- train[trainingdata,]
testing <- train[-trainingdata,]

# This part is for cleaning data and Finding the nearzero variance and removing them
zerovar <- nearZeroVar(training)
training <- training[,-zerovar]

zerovar <- nearZeroVar(testing)
testing <- testing[,-zerovar]

zerovar <- nearZeroVar(test)
test <- test[,-zerovar]


#Clean variables with more than 60% NA
mostNA <- sapply(training, function(x) mean(is.na(x))) > .6
training <- training[,mostNA==F]
testing <- testing[,mostNA==F]
test <- test[,mostNA==F]

# To get the same class between testing and training aand test files
training <- training[, -(1:2)]
testing <- testing[, -(1:2)]
test<- test[, -(1:2)]


#----------prediction 1:Decision Trees
modelFit1 <- rpart(classe ~ ., data=training, method="class")
fancyRpartPlot(modelFit1)

predictions1 <- predict(modelFit1, testing, type = "class")
cmtree <- confusionMatrix(predictions1, testing$classe)
cmtree
plot(cmtree$table, col = cmtree$byClass, main = paste("Decision Tree : Accuracy =", round(cmtree$overall['Accuracy'], 4)))

#----------prediction 2:Random Forests
modelFit2 <- randomForest(classe ~ ., data=training)

prediction2 <- predict(modelFit2, testing, type = "class")
cmrf <- confusionMatrix(prediction2, testing$classe)
cmrf
plot(modelFit2)
plot(cmrf$table, col = cmtree$byClass, main = paste("Random Forest : Accuracy =", round(cmrf$overall['Accuracy'], 4)))

#----------prediction 3:Generalized Boosted Regression
fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 1)

gbmFit1 <- train(classe ~ ., data=training, method = "gbm",trControl = fitControl,verbose = FALSE)
gbmFinMod1 <- gbmFit1$finalModel

gbmPredTest <- predict(gbmFit1, newdata=testing)
gbmAccuracyTest <- confusionMatrix(gbmPredTest, testing$classe)
gbmAccuracyTest

# Between all of these model the best and most accurate one was Random Forest with 0.9989 accuracy so for our test data we will
use this model

predictest <- predict(modelFit2, testing, type = "class")
predictest
