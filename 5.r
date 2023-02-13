library("rpart")
library("rpart.plot")
# Read the data
setwd("C:\Users\PRIYANKA\Desktop\Sem 2\BDA Practicals")
banktrain <- read.table("bank-sample.csv",header=TRUE,sep=",")
View(banktrain)
## drop a few columns to simplify the tree
drops<-c("age", "balance", "day", "campaign", "pdays", "previous", "month")
banktrain <- banktrain [,!(names(banktrain) %in% drops)]
View(banktrain)
summary(banktrain)
# Make a simple decision tree by only keeping the categorical variables
fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + poutcome,
 method="class",
 data=banktrain,
 control=rpart.control(minsplit=1),
 parms=list(split='information'))
summary(fit)
# Plot the tree
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

# include a numeric variable "duration" into the model
fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + duration +
poutcome,
 method="class",
 data=banktrain,
 control=rpart.control(minsplit=1),
 parms=list(split='information'))
summary(fit)
# Plot the tree
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)
# Predict
newdata <- data.frame(job="retired",
 marital="married",
 education="secondary",
 default="no",
 housing="yes",
 loan="no",
 contact = "cellular",
 duration = 598,
 poutcome="unknown")
newdata
predict(fit,newdata=newdata,type=c("class"))
