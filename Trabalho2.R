########################################################################
# INF-0615 - Tarefa 2 - Wine Quality                                   #
# Alunos: Rafael Fernando Ribeiro                                      #
#         Thiago Gomes Marçal Pereira                                  #
########################################################################
#install.packages("glmnet")
library(glmnet)

set.seed(42)
setwd("/Users/thiagom/Documents/Studies/Unicamp/MDC/INF-615/Tarefas/INF0615_Tarefa2/")

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

summary(train_data)
summary(val_data)

