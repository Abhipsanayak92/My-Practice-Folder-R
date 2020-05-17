#objective: To predict the cust will exit or not using Logistic Regression
#y=Exited
data<-read.csv(file.choose())
names(data)
View(data)
str(data)
library(ggplot2)
library(gridExtra)
attach(data)
data$Exited_flag<- as.factor(ifelse(data$Exited==1, "Yes", "No"))
table(data$Exited_flag)
2037/10000 #bad rate is 20% 

data1<-data[c(-1,-2,-3)]#deleting row no, surname and cust id
data1$Exited<-NULL #deleting Exited column
names(data1)
str(data1)
summary(data1) #we can see that there is no missing values

#treatment of outliers
boxplot(data1$Age) ###has outlier
boxplot(data1$Tenure) ##no outlier
boxplot(data1$EstimatedSalary) #no outlier
boxplot(data1$Balance)  #no outlier
boxplot(data1$NumOfProducts) #has outlier
boxplot(data1$HasCrCard) # no outlier
boxplot(data1$IsActiveMember)   #no outlier


#outlier treatment of Age
summary(data1$Age)
upper<-44+1.5*IQR(data1$Age)
upper
data1$Age[data1$Age>upper]<-upper
summary(data1$Age)
boxplot(data1$Age)

lapply(data1, $NumOfProducts, upper)


#outlier treatment for num of products
#summary(data1$NumOfProducts)
#upper<- 2+1.5*IQR(data1$NumOfProducts)
#upper
#data1$NumOfProducts[data1$NumOfProducts>upper]<-upper
#boxplot(data1$NumOfProducts)
#summary(data1$NumOfProducts)

library(caret)
Train<-createDataPartition(data1$Exited_flag, p=0.7, list=FALSE)
training<-data1[Train,]
testing<-data1[-Train,]

sapply(Train, function(x) sum(is.na(x)))
library(car)
logit<-glm(Exited_flag~.,data=training,family='binomial')

summary(logit)
### we can see geography spain, tenure, no of products, has cr card, estimated salary are not impacting y variable as teir p value > alpha
##############credit score has p value= 0.05, it may or may not impact
###############why gender female is not showing in summary? has it been added to intercept
Acc(logit)  # accuracy of training data  = 75.48%

library(car)
vif(logit)
logit2<- step(glm(Exited_flag~.-Gender, data=training, family='binomial'), direction='backward')
######### why gender is getting deleted

summary(logit2)
Acc(logit2) ## accuracy of final model = 76.88%

testing$probs<-predict(logit2,testing,type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$Predict,testing$Exited_Flag)
confusionMatrix(testing$Exited_Flag,testing$Predict)
library(ROCR)
predictTrain=predict(logit,testing,type="response")
ROCRpred=prediction(predictTrain,testing$Exited_Flag)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
library(ROCR)
pred=prediction(testing$probs,testing$Exited_Flag)
as.numeric(performance(pred,"auc")@y.values)
