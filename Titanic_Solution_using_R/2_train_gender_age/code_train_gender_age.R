#Working Directory
setwd("D:/FunXExcel Channel/17. Kaggle/GitHub/Kaggle_Titanic_R/2_train_gender_age")

#Import
df_train <- read.csv("train_gender_age.csv")
df_test <- read.csv("test_gender_age.csv")

#Check train Dataset
str(df_train)
summary(df_train)

#Check train Dataset
str(df_test)
summary(df_test)

#Assign NA to Survived in test dataset
df_test$Survived <- NA

#Convert Survived in train to factor
df_train$Survived <- as.factor(df_train$Survived)

#Replace missing value by mean of age - train
table(is.na(df_train$Age))
df_train$Age[is.na(df_train$Age)] <- round(mean(df_train$Age, na.rm = TRUE))
table(is.na(df_train$Age))

#Replace missing value by mean of age - test
table(is.na(df_test$Age))
df_test$Age[is.na(df_test$Age)] <- round(mean(df_test$Age, na.rm = TRUE))
table(is.na(df_test$Age))

#Check again for missing values
summary(df_train)
summary(df_test)


#Build model using Logistic Regression
logistic <- glm(Survived ~ Age + Sex ,data = df_train , family = binomial)
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
write.csv(solution, "submission_train_gender_age.csv", row.names = F)
