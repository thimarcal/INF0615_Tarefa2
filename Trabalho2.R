########################################################################
# INF-0615 - Tarefa 2 - Wine Quality                                   #
# Alunos: Rafael Fernando Ribeiro                                      #
#         Thiago Gomes Marçal Pereira                                  #
########################################################################
#install.packages("glmnet")
library(glmnet)

set.seed(42)
setwd("/Users/thiagom/Documents/Studies/Unicamp/MDC/INF-615/Tarefas/INF0615_Tarefa2/")
#setwd("C:\\Users\\rafaelr\\Documents\\INF015\\Tarefa2\\INF0615_Tarefa2")

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
logistic.train <- function(train_data, val_data) {
  x <- model.matrix(quality~.+0, train_data)
  #and separate class info
  y <- train_data$quality
  
  x_val <- model.matrix(quality~.+0, val_data)
  
  # Testing different lambdas
  lambda <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 0.0, 1.0, 10)
  accPerLambda <- data.frame(lambda=lambda, accTrain=numeric(length(lambda)), accVal=numeric(length(lambda)))
  
  for (i in 1:length(lambda)){
    model <- glmnet(x, y,  family="binomial", alpha=0, lambda = lambda[i])
    
    trainPred <- predict(model,newx = x, type="response")
    accPerLambda$accTrain[i] <- confusion_matrix(y, trainPred)
    
    valPred <- predict(model,newx = x_val, type="response")
    accPerLambda$accVal[i] <- confusion_matrix(val_data$quality, valPred)
  }
  
  print(accPerLambda)
}

# Reading data
train_data <- read.csv("wineQuality_train.data", header = TRUE)
val_data<- read.csv("wineQuality_val.data", header = TRUE)
#test_data<- read.csv("wineQuality_test.data", header = TRUE)

dim(train_data)
summary(train_data)
dim(val_data)
summary(val_data)
#dim(test_data)
#summary(test_data)

# Remove Outliers? 
# Max Residual sugar 31.600, 3rd quarter 8.300
# Max free.sulfur.dioxide 131.00 3rd quarter 41.00
# total.sulfur.dioxide min and max seem out of normal range

# Normalize the data (Removing quality)
meanTrainFeatures = colMeans(train_data[,-12]) #mean of each feature
stdTrainFeatures = apply(train_data[,-12], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

train_data[,-12] = sweep(train_data[,-12], 2, meanTrainFeatures, "-")
train_data[,-12] = sweep(train_data[,-12], 2, meanTrainFeatures, "/")

val_data[,-12] = sweep(val_data[,-12], 2, meanTrainFeatures, "-")
val_data[,-12] = sweep(val_data[,-12], 2, meanTrainFeatures, "/")

#test_data[,-12] = sweep(test_data[,-12], 2, meanTrainFeatures, "-")
#test_data[,-12] = sweep(test_data[,-12], 2, meanTrainFeatures, "/")

train_data$quality <- as.factor(train_data$quality)
val_data$quality <- as.factor(val_data$quality)

summary(train_data)
summary(val_data)

cor(train_data[,-12])
pairs(~., data = train_data[,-12])

logistic.train(train_data, val_data)


#############################################################
# Balance the Data - Process 1 - decrease the higher        #
#############################################################
bad_wines <- train_data[train_data$quality==0,]
sample_wines <- sample(1:nrow(bad_wines), sum(train_data$quality==1))
bad_wines <- bad_wines[sample_wines,]

balanced_train <- rbind(bad_wines, train_data[train_data$quality==1,])
logistic.train(balanced_train, val_data)

#############################################################
# Balance the Data - Process 2 - create sample data         #
#############################################################
library(DMwR)
smoted_data <- SMOTE(quality~., train_data, perc.over=400, perc.under = 125)

logistic.train(smoted_data, val_data)


