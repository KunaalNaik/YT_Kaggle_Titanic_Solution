#Working Directory
setwd("D:/FunXExcel Channel/17. Kaggle/GitHub/Kaggle_Titanic_R/4_5_6_7_8_train_all_numericals_categorical")

#Library for SVM
library(e1071)

#Import
df_train <- read.csv("train_all_numericals_categorical.csv")
df_test <- read.csv("test_all_numericals_categorical.csv")

#Check train Dataset
str(df_train)
summary(df_train)

#Check train Dataset
str(df_test)
summary(df_test)

#Assign NA to Survived in test dataset
#df_test$Survived <- NA 
#No Need for this in SVM

#Convert Survived in train to factor
df_train$Survived <- as.factor(df_train$Survived)

#I have cut out all the Data Manipulation code since I have done that on Excel

#Check again for missing values
str(df_train)
str(df_test)

#Remove PassengerId since it is not useful 
df_train$PassengerId <- NULL

#Build model using Logistic Regression
SVMmodel<-svm(Survived ~ ., data = df_train,gamma=.05,cost=1.80,type="C-classification")
summary(SVMmodel)

#Score using training dataset
predict <- predict(SVMmodel)

#Check model performance - Ideally you need to do it on a cross validation test dataset dereived from train dataset
#Does not apply for this model

#Score on test dataset
prediction <- predict(SVMmodel, df_test)

#Prepare dataset as per Kaggle submission file
solution <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction)

#export to same folder as the input files
write.csv(solution, "submission_train_all_numericals_categorical_svm.csv", row.names = F)
#0.79425
