#Working Directory
setwd("D:/FunXExcel Channel/17. Kaggle/GitHub/Kaggle_Titanic_R/14_15_16_17_18_train_all_feature_engi_2")

#Library for Random forest
library(caret)

#Import
df_train <- read.csv("train_all_feature_engi_2.csv")
df_test <- read.csv("test_all_feature_engi_2.csv")

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

#Build model using Decision Tree
set.seed(7)
model_dt <- train(Survived ~ ., data = train, method = "rpart")
model_dt$results$Accuracy

#Combine Accuracy Results of Models
results <- resamples(list(Logit = model_logistic, KNN = model_knn, DT = model_dt, RF = model_rf))
summary(results)

#Plot Accuracy
bwplot(results)

#RF seems to be doing good

#You will need to select the best variables to increace speed and give better results
importance <- varImp(model_rf, scale = FALSE)
plot(importance)

#Build model using Random Forest
set.seed(7)
model_rf1 <- train(Survived ~ Sex_male + Age + Fare + Title_Mr + FamilySize + 
                     Pclass_3 + LargeFamily + SibSp + Cabin_Missing + 
                     Title_Mrs + Title_Miss + Embarked_S + Cabin_E
                     +Ticket_xxx + Parch + SmallFamily + Title_Officer
                    + Embarked_C + Ticket_STONO
                   , data = train, method = "rf")
model_rf1$results$Accuracy

#Score on test dataset
#prediction_logistic <- predict(model_logistic, newdata = df_test)
#prediction_knn <- predict(model_knn, newdata = df_test)
prediction_dt <- predict(model_dt, newdata = df_test)
prediction_rf <- predict(model_rf, newdata = df_test)
prediction_rf1 <- predict(model_rf1, newdata = df_test)

#Prepare dataset as per Kaggle submission file
#solution_logistic <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_logistic)
#solution_knn <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_knn)
solution_dt <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_dt)
solution_rf <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_rf)
solution_rf1 <- data.frame(PassengerID = df_test$PassengerId, Survived = prediction_rf1)


#export to same folder as the input files
#write.csv(solution_logistic, "submission1_caret_logistic.csv", row.names = F)
#write.csv(solution_knn, "submission1_caret_knn.csv", row.names = F)
write.csv(solution_dt, "submission1_caret_dt.csv", row.names = F)
write.csv(solution_rf, "submission1_caret_rf.csv", row.names = F) #Best
write.csv(solution_rf1, "submission1_caret_rf_fin.csv", row.names = F) #Best

