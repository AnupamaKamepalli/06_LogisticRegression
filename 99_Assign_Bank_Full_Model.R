#Logistic Regression,Dataset=bankfull, Regression type = binomial logistic regression 

install.packages("readxl")
library(readxl)
getwd()
bank_D1 <- read_xlsx("bank-full.xlsx")
head(bank_D1)
str(bank_D1)
View(bank_D1)

#Check for NA values.
sum(is.na(bank_D1)) # No Missing Values existed.

#Creating dummy variables for categorical variables.
install.packages("fastDummies")
library(fastDummies)
results2 <- fastDummies::dummy_columns(bank_D1,remove_first_dummy = TRUE,remove_selected_columns = TRUE)
View(results2)
str(results2)

#Model Building.
attach(results2)
bank_M1 <- glm(y_yes~.,data = results2,family = "binomial")
summary(bank_M1)

# Predicting the values for 'Y'. which inturn will be used to calculate the accuracy of the model.
predict_Y <- predict(bank_M1,data=results2,type = "response")
confusion_Y <- table(predict_Y >0.5,results2$y_yes)
confusion_Y

#Model Accuracy Calculation

Accuracy_Y = sum(diag(confusion_Y))/sum(confusion_Y)
Accuracy_Y # 0.901838 

Error_y <- 1- Accuracy_Y
Error_y #  0.09816195

# Another way to calculate error.
Error_Y1 <- sum(confusion_Y[cbind(2:1,1:2)])/sum(confusion_Y)
Error_Y1

#Roc Curve
rocrpred_B1 <- prediction(predict_Y,results2$y_yes)
rocrper_B1 <- performance(rocrpred_B1,'tpr','fpr')
plot(rocrper_B1,colorize=T) # IT could be perfect classifier.


