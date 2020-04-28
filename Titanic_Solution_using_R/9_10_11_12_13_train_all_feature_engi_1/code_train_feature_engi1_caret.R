#Working Directory
setwd("D:/FunXExcel Channel/17. Kaggle/GitHub/Kaggle_Titanic_R/9_10_11_12_13_train_all_feature_engi_1")

#Library for Random forest
library(caret)

#Import
df_train <- read.csv("train_all_feature_engi_1.csv")
df_test <- read.csv("test_all_feature_engi_1.csv")

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

#I have put out all the Data Manipulation code since I have done that on Excel

#Check again for missing values
str(df_train)
str(df_test)

#Remove PassengerId since it is not useful 
#df_train$PassengerId <- NULL

#Required for other training schemes
#df_train$Survived <- ifelse(df_train$Survived == 1,"Yes","No")

#prepare training scheme
split = 0.80
trainIndex <- createDataPartition(df_train$Survived, p=split, list=FALSE)
train <- df_train[trainIndex,]
test <- df_train[-trainIndex,]

#Build model using Logistic Regression
set.seed(7)
model_logistic <- train(Survived ~ ., data = train, method = "glm")
model_logistic$results$Accuracy

#Build model using Random Forest
set.seed(7)
model_rf <- train(Survived ~ ., data = train, method = "rf")
model_rf$results$Accuracy

#Build model using k-Nearest Neighbors
set.seed(7)
model_knn <- train(Survived ~ ., data = train, method = "knn")
model_knn$results$Accuracy

#Build model using SVM
set.seed(7)
model_svm <- train(Survived ~ ., data = train, method = "svmRadial")
model_svm$results$Accuracy

#Build model using Avg Neural Network
set.seed(7)
model_avNNet <- train(Survived ~ ., data = train, method = "avNNet")
model_avNNet$results$Accuracy

#Combine Accuracy Results of Models
results <- resamples(list(Logit = model_logistic, KNN = model_knn, SVM = model_svm, RF = model_rf, NN = model_avNNet))
summary(results)

#Plot Accuracy
bwplot(results)

#You will need to select the best variables to increace speed and give better results
importance <- varImp(model_avNNet, scale = FALSE)
plot(importance)

#Score using training dataset
#predict <- predict(rf)

#Check model performance - Ideally you need to do it on a cross validation test dataset dereived from train dataset
#Does not apply for this model

#Score on test dataset
prediction_logistic <- predict(model_logistic, newdata = df_test)
prediction_knn <- predict(model_knn, newdata = df_test)
prediction_svm <- predict(model_svm, newdata = df_test)
prediction_rf <- predict(model_rf, newdata = df_test)
prediction_avNNet <- predict(model_avNNet, newdata = df_test)

#Prepare dataset as per Kaggle submission file
solution_logistic <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_logistic)
solution_knn <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_knn)
solution_svm <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_svm)
solution_rf <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_rf)
#solution_avNNet <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_avNNet) #Takes long time to run 

#export to same folder as the input files
write.csv(solution_logistic, "submission1_caret_logistic.csv", row.names = F)
write.csv(solution_knn, "submission1_caret_knn.csv", row.names = F)
write.csv(solution_svm, "submission1_caret_svm.csv", row.names = F)
write.csv(solution_rf, "submission1_caret_rf.csv", row.names = F) #Best
write.csv(solution_avNNet, "submission1_caret_avNNet.csv", row.names = F)
