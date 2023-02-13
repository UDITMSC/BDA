# Logistic Regression
###############################################################
churn_input = as.data.frame( read.csv("C:/Users/PRIYANKA/Desktop/Sem 2/BDA
Practicals/churn.csv") )
View(churn_input)
head(churn_input)
summary(churn_input)
sum(churn_input$Churned)

Churn_logistic1 <- glm (Churned~Age + Married + Cust_years + Churned_contacts,
 data=churn_input, family=binomial(link="logit"))
summary(Churn_logistic1)
Churn_logistic2 <- glm (Churned~Age + Married + Churned_contacts,
 data=churn_input, family=binomial(link="logit"))
summary(Churn_logistic2)
Churn_logistic3 <- glm (Churned~Age + Churned_contacts,
 data=churn_input, family=binomial(link="logit"))
 
summary(Churn_logistic3)
summary(Churn_logistic2)
pchisq(.9 , 1, lower=FALSE) 

# Receiver Operating Characteristic (ROC) Curve
install.packages("ROCR") #install, if necessary
library(ROCR)
pred = predict(Churn_logistic3, type="response")
predObj = prediction(pred, churn_input$Churned )
predObj
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")
summary(aucObj)
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4)))
