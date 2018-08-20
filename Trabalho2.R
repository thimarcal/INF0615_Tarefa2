########################################################################
# INF-0615 - Tarefa 2 - Wine Quality                                   #
# Alunos: Rafael Fernando Ribeiro                                      #
#         Thiago Gomes MarÃ§al Pereira                                  #
########################################################################
#install.packages("glmnet")
library(glmnet)

set.seed(42)
#setwd("/Users/thiagom/Documents/Studies/Unicamp/MDC/INF-615/Tarefas/INF0615_Tarefa2/")
setwd("C:\\Users\\rafaelr\\Documents\\INF015\\Tarefa2\\INF0615_Tarefa2")

confusion_matrix <- function(true_value, predicted_value, print_value=FALSE) {
  #converting to class
  predicted_value[predicted_value >= 0.5] = 1
  predicted_value[predicted_value < 0.5] = 0
  
  #confusion matrix
  cm = as.matrix(table(Actual = true_value, Predicted = predicted_value))

  if (dim(cm)[2] == 1) {
    cm <- cbind(cm, c(0,0))
  }
  
  #ACC = (TP + TN) / total
  ACC = (cm[1,1] + cm[2,2]) / sum(cm)
  
  #TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
  TPR = cm[2,2] / (cm[2,2] + cm[2,1])
  
  #TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
  TNR = cm[1,1] / (cm[1,1] + cm[1,2])
  
  #ACC normalized by class
  #takes into account the number of samples for each class
  ACCNorm_glm = mean(c(TPR, TNR))
  
  if (print_value == TRUE) {
    print(cm)  
    print(paste("Accuracy = ",ACC))
    print(paste("True Positive = ",TPR))
    print(paste("True Negative = ",TNR))
    print(paste("Accuracy Norm.= ",ACCNorm_glm))
  }
  
  ACCNorm_glm
}

###### Logistic with regularization ########
logistic.train <- function(train_data, val_data, test_data) {
  x <- model.matrix(quality~.+0, train_data)
  #and separate class info
  y <- train_data$quality
  
  x_val <- model.matrix(quality~.+0, val_data)
  
  test_val <- model.matrix(quality~.+0, test_data)
  
  # Testing different lambdas
  lambda <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 0.0, 1.0, 10)
  accPerLambda <- data.frame(lambda=lambda, accTrain=numeric(length(lambda)), accVal=numeric(length(lambda)), accTest=numeric(length(lambda)))
  
  for (i in 1:length(lambda)){
    model <- glmnet(x, y,  family="binomial", alpha=0, lambda = lambda[i])
    
    trainPred <- predict(model,newx = x, type="response")
    accPerLambda$accTrain[i] <- confusion_matrix(y, trainPred, TRUE)
    
    valPred <- predict(model,newx = x_val, type="response")
    accPerLambda$accVal[i] <- confusion_matrix(val_data$quality, valPred, TRUE)
    
    #testPred <- predict(model,newx = test_val, type="response")
    #accPerLambda$accTest[i] <- confusion_matrix(test_data$quality, testPred, TRUE)
  }
  
  print(accPerLambda)
}

# Reading data
train_data <- read.csv("wineQuality_train.data", header = TRUE)
val_data<- read.csv("wineQuality_val.data", header = TRUE)
test_data<- read.csv("wineQuality_test.data", header = TRUE)

dim(train_data)
summary(train_data)
dim(val_data)
summary(val_data)
dim(test_data)
summary(test_data)

# Normalize the data (Removing quality)
meanTrainFeatures = colMeans(train_data[,-12]) #mean of each feature
stdTrainFeatures = apply(train_data[,-12], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

train_data[,-12] = sweep(train_data[,-12], 2, meanTrainFeatures, "-")
train_data[,-12] = sweep(train_data[,-12], 2, meanTrainFeatures, "/")

val_data[,-12] = sweep(val_data[,-12], 2, meanTrainFeatures, "-")
val_data[,-12] = sweep(val_data[,-12], 2, meanTrainFeatures, "/")

test_data[,-12] = sweep(test_data[,-12], 2, meanTrainFeatures, "-")
test_data[,-12] = sweep(test_data[,-12], 2, meanTrainFeatures, "/")

train_data$quality <- as.factor(train_data$quality)
val_data$quality <- as.factor(val_data$quality)
test_data$quality <- as.factor(test_data$quality)

summary(train_data)
summary(val_data)

cor(train_data[,-12])
pairs(~., data = train_data[,-12])

logistic.train(train_data, val_data, test_data)


#############################################################
# Balance the Data - Process 1 - decrease the higher        #
#############################################################
bad_wines <- train_data[train_data$quality==0,]
sample_wines <- sample(1:nrow(bad_wines), sum(train_data$quality==1))
bad_wines <- bad_wines[sample_wines,]

balanced_train <- rbind(bad_wines, train_data[train_data$quality==1,])
logistic.train(balanced_train, val_data, test_data)

#############################################################
# Balance the Data - Process 2 - create sample data         #
#############################################################
library(DMwR)
smoted_data <- SMOTE(quality~., train_data, perc.over=400, perc.under = 125)

logistic.train(smoted_data, val_data, test_data)

#############################################################
# Remove Outliers from Original data                        #
#############################################################
train_data <- read.csv("wineQuality_train.data", header = TRUE)
val_data<- read.csv("wineQuality_val.data", header = TRUE)
test_data<- read.csv("wineQuality_test.data", header = TRUE)

# Fixed Acidity
mean <- mean(train_data$fixed.acidity)
sd <- sd(train_data$fixed.acidity)
train_data <- train_data[train_data$fixed.acidity < (mean + 2*sd), ]
train_data <- train_data[train_data$fixed.acidity > (mean - 2*sd), ]

# Volatile Acidity
mean <- mean(train_data$volatile.acidity)
sd <- sd(train_data$volatile.acidity)
train_data <- train_data[train_data$volatile.acidity < (mean + 2*sd), ]
train_data <- train_data[train_data$volatile.acidity > (mean - 2*sd), ]

# Citric Acid
mean <- mean(train_data$citric.acid)
sd <- sd(train_data$citric.acid)
train_data <- train_data[train_data$citric.acid < (mean + 2*sd), ]
train_data <- train_data[train_data$citric.acid > (mean - 2*sd), ]

# Residual Sugar
mean <- mean(train_data$residual.sugar)
sd <- sd(train_data$residual.sugar)
train_data <- train_data[train_data$residual.sugar < (mean + 2*sd), ]
train_data <- train_data[train_data$residual.sugar > (mean - 2*sd), ]

# Chlorides
mean <- mean(train_data$chlorides)
sd <- sd(train_data$chlorides)
train_data <- train_data[train_data$chlorides < (mean + 2*sd), ]
train_data <- train_data[train_data$chlorides > (mean - 2*sd), ]

# Free Sulfur Dioxide
mean <- mean(train_data$free.sulfur.dioxide)
sd <- sd(train_data$free.sulfur.dioxide)
train_data <- train_data[train_data$free.sulfur.dioxide < (mean + 2*sd), ]
train_data <- train_data[train_data$free.sulfur.dioxide > (mean - 2*sd), ]

# Total Sulfur Dioxide
mean <- mean(train_data$total.sulfur.dioxide)
sd <- sd(train_data$total.sulfur.dioxide)
train_data <- train_data[train_data$total.sulfur.dioxide < (mean + 2*sd), ]
train_data <- train_data[train_data$total.sulfur.dioxide > (mean - 2*sd), ]

# Density
mean <- mean(train_data$density)
sd <- sd(train_data$density)
train_data <- train_data[train_data$density < (mean + 2*sd), ]
train_data <- train_data[train_data$density > (mean - 2*sd), ]

# pH
mean <- mean(train_data$pH)
sd <- sd(train_data$pH)
train_data <- train_data[train_data$pH < (mean + 2*sd), ]
train_data <- train_data[train_data$pH > (mean - 2*sd), ]

# Sulphates
mean <- mean(train_data$sulphates)
sd <- sd(train_data$sulphates)
train_data <- train_data[train_data$sulphates < (mean + 2*sd), ]
train_data <- train_data[train_data$sulphates > (mean - 2*sd), ]

# Alcohol
mean <- mean(train_data$alcohol)
sd <- sd(train_data$alcohol)
train_data <- train_data[train_data$alcohol < (mean + 2*sd), ]
train_data <- train_data[train_data$alcohol > (mean - 2*sd), ]


###########################################################################
# Now, run the same previous validations as without removing outliers     #
###########################################################################
  
# Normalize the data (Removing quality)
meanTrainFeatures = colMeans(train_data[,-12]) #mean of each feature
stdTrainFeatures = apply(train_data[,-12], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

train_data[,-12] = sweep(train_data[,-12], 2, meanTrainFeatures, "-")
train_data[,-12] = sweep(train_data[,-12], 2, meanTrainFeatures, "/")

val_data[,-12] = sweep(val_data[,-12], 2, meanTrainFeatures, "-")
val_data[,-12] = sweep(val_data[,-12], 2, meanTrainFeatures, "/")

test_data[,-12] = sweep(test_data[,-12], 2, meanTrainFeatures, "-")
test_data[,-12] = sweep(test_data[,-12], 2, meanTrainFeatures, "/")

train_data$quality <- as.factor(train_data$quality)
val_data$quality <- as.factor(val_data$quality)
test_data$quality <- as.factor(test_data$quality)

summary(train_data)
summary(val_data)
summary(test_data)

cor(train_data[,-12])
pairs(~., data = train_data[,-12])

logistic.train(train_data, val_data, test_data)


#############################################################
# Balance the Data - Process 1 - decrease the higher        #
#############################################################
bad_wines <- train_data[train_data$quality==0,]
sample_wines <- sample(1:nrow(bad_wines), sum(train_data$quality==1))
bad_wines <- bad_wines[sample_wines,]

balanced_train <- rbind(bad_wines, train_data[train_data$quality==1,])
logistic.train(balanced_train, val_data, test_data)

#############################################################
# Balance the Data - Process 2 - create sample data         #
#############################################################
smoted_data <- SMOTE(quality~., train_data, perc.over=400, perc.under = 125)

logistic.train(smoted_data, val_data, test_data)

#############################################################
# Tentativa de melhoria                                     #
#############################################################
train_data <- read.csv("wineQuality_train.data", header = TRUE)
val_data<- read.csv("wineQuality_val.data", header = TRUE)
test_data<- read.csv("wineQuality_test.data", header = TRUE)

# Normalize the data (Removing quality)
meanTrainFeatures = colMeans(train_data[,-12]) #mean of each feature
stdTrainFeatures = apply(train_data[,-12], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

train_data[,-12] = sweep(train_data[,-12], 2, meanTrainFeatures, "-")
train_data[,-12] = sweep(train_data[,-12], 2, meanTrainFeatures, "/")

val_data[,-12] = sweep(val_data[,-12], 2, meanTrainFeatures, "-")
val_data[,-12] = sweep(val_data[,-12], 2, meanTrainFeatures, "/")

test_data[,-12] = sweep(test_data[,-12], 2, meanTrainFeatures, "-")
test_data[,-12] = sweep(test_data[,-12], 2, meanTrainFeatures, "/")

train_data$quality <- as.factor(train_data$quality)
val_data$quality <- as.factor(val_data$quality)
test_data$quality <- as.factor(test_data$quality)

#############################################################
# Balance the Data - Process 2 - create sample data         #
#############################################################
smoted_data <- SMOTE(quality~., train_data, perc.over=400, perc.under = 125)

cor(train_data[,-12])

x <- model.matrix(quality~.+0+I(fixed.acidity^3) + I(volatile.acidity^3)+I(citric.acid) + I(residual.sugar^3)
                  + I(chlorides^3) + I(free.sulfur.dioxide^3) + I(total.sulfur.dioxide^3) + I(density^3)
                  + I(pH^3) + I(sulphates^3) + I(alcohol^3), smoted_data)

y <- smoted_data$quality

x_val <- model.matrix(quality~.+0+I(fixed.acidity^3) + I(volatile.acidity^3)+I(citric.acid) + I(residual.sugar^3)
                      + I(chlorides^3) + I(free.sulfur.dioxide^3) + I(total.sulfur.dioxide^3) + I(density^3)
                      + I(pH^3) + I(sulphates^3) + I(alcohol^3), val_data)

test_val <- model.matrix(quality~.+0+I(fixed.acidity^3) + I(volatile.acidity^3)+I(citric.acid) + I(residual.sugar^3)
                         + I(chlorides^3) + I(free.sulfur.dioxide^3) + I(total.sulfur.dioxide^3) + I(density^3)
                         + I(pH^3) + I(sulphates^3) + I(alcohol^3), test_data)

model <- glmnet(x, y,  family="binomial", alpha=0, lambda = 0.001)

trainPred <- predict(model,newx = x, type="response")
accTrain <- confusion_matrix(y, trainPred)

valPred <- predict(model,newx = x_val, type="response")
accVal <- confusion_matrix(val_data$quality, valPred)

testPred <- predict(model,newx = test_val, type="response")
accTest <- confusion_matrix(test_data$quality, testPred)

accTrain
accVal
accTest

# Final model
x <- model.matrix(quality~.+0, smoted_data)

y <- smoted_data$quality

x_val <- model.matrix(quality~.+0, val_data)

test_val <- model.matrix(quality~.+0,test_data)

model <- glmnet(x, y,  family="binomial", alpha=0, lambda = 0.001)

trainPred <- predict(model,newx = x, type="response")
accTrain <- confusion_matrix(y, trainPred, TRUE)

valPred <- predict(model,newx = x_val, type="response")
accVal <- confusion_matrix(val_data$quality, valPred, TRUE)

testPred <- predict(model,newx = test_val, type="response")
accTest <- confusion_matrix(test_data$quality, testPred, TRUE)
