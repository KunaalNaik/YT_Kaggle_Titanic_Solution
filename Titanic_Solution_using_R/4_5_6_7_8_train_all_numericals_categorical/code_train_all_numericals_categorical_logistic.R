#Working Directory
setwd("D:/FunXExcel Channel/17. Kaggle/GitHub/Kaggle_Titanic_R/4_5_6_7_8_train_all_numericals_categorical")

#Import
df_train <- read.csv("train_all_numericals_categorical.csv")
df_test <- read.csv("test_all_numericals_categorical.csv")

#Check train Dataset
str(df_train)
summary(df_train)

#Check train Dataset
str(df_test)
summary(df_test)

#Remove PassengerID, we dont need it
df_train$PassengerId <- NULL

#Assign NA to Survived in test dataset
df_test$Survived <- NA

#Convert Survived in train to factor
df_train$Survived <- as.factor(df_train$Survived)

#I have cut out all the Data Manipulation code since I have done that on Excel

#Check again for missing values
str(df_train)
str(df_test)

#Build model using Logistic Regression
logistic <- glm(Survived ~ .,data = df_train , family = binomial)
summary(logistic)

#Score using training dataset
predict <- predict(logistic, type = 'response')

#Check model performance - Ideally you need to do it on a cross validation test dataset dereived from train dataset
library(ROCR)
ROCRpred <- prediction(predict, df_train$Survived)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Score on test dataset
prediction <- predict(logistic, df_test , type = 'response')

#Since the predictions range between 0 to 1, we need to ensure responses are either 0 or 1
df_test$Survived <- ifelse(prediction >0.5,1,0)

#Prepare dataset as per Kaggle submission file
solution <- data.frame(PassengerID = df_test$PassengerId, Survived = df_test$Survived)

#export to same folder as the input files
write.csv(solution, "submission_train_all_numericals_categorical.csv", row.names = F)
