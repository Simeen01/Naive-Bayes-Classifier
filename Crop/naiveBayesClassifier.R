library(caret)
library(dplyr)
library(ggplot2)
library(psych)
library(naivebayes)

ind=sample(2, nrow(crop_data),replace=T,prob = c(0.8,0.2) )
train=crop_data[ind==1,]
test=crop_data[ind==2, ]
model=naive_bayes(as.factor(crop) ~., data=train)
model1=naive_bayes(as.factor(crop) ~., data=test)
plot(model1)
p=predict(model,train,crop='prob')
head(cbind(p,train))
p1=predict(model,train)
tab1 = table(p1,train$crop)
1-sum(diag(tab1))/sum(tab1)
train$crop