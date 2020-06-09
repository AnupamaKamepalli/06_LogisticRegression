getwd()
cr_d1 <- read.csv("creditcard.csv")
cr_d1 <- cr_d1[,-1]
View(cr_d1)

#Creating the dummy variables.
install.packages("fastDummies")
library(fastDummies)
cr_d2 <- fastDummies::dummy_columns(cr_d1,remove_first_dummy = TRUE,remove_selected_columns = TRUE)
View(cr_d2)
attach(cr_d2)
#Model Building
cr_M1 <- glm(card_yes~.,data = cr_d2,family = "binomial")
summary(cr_M1)

#Creation of confusion matrix.
pred_card_prob <- predict(cr_M1,data=cr_d2,type = 'response')
confusion_card <- table(pred_card_prob>0.5,cr_d2$card_yes)
confusion_card

#find the accuracy of the model.
accuracy_card <- sum(diag(confusion_card))/sum(confusion_card)
accuracy_card #  0.8597422

#finding the Error.
1- accuracy_card #0.1402578

#Alternate way to calculate the error rate.
Error_card <- sum(confusion_card[cbind(1:2,2:1)])/sum(confusion_card)
Error_card #  0.1402578

#Roc curve
rocrpred_card <- prediction(pred_card_prob,cr_d2$card_yes)
rocrper_card <- performance(rocrpred_card,'tpr','fpr')
plot(rocrper_card,colorize=T)



