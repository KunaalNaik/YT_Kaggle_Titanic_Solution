#Working Directory
setwd("D:/FunXExcel Channel/17. Kaggle/GitHub/Kaggle_Titanic_R/1_train_gender")

#Import
df_train <- read.csv("train_gender.csv")
df_test <- read.csv("test_gender.csv")

#Check train Dataset
str(df_train)
summary(df_train)

#Check train Dataset
str(df_test)
summary(df_test)

#Cross tabulate Survived and Sex 
table(df_train$Survived, df_train$Sex)

#More females survived than males

#Simple Model
df_test$Survived <- ifelse(df_test$Sex == "female", 1, 0)

solution <- data.frame(PassengerID = df_test$PassengerId, Survived = df_test$Survived)

write.csv(solution, "submission_train_gender.csv", row.names = F)
