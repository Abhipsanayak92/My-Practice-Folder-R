
data<-read.csv(file.choose())
names(data)
str(data)
data$Limit<-as.numeric(data$Limit)
data$Rating<-as.numeric(data$Rating)
data$Cards<-as.numeric(data$Cards)
data$Age<-as.numeric(data$Age)
data$Education<- as.numeric(data$Education)
data$Balance<-as.numeric(data$Balance)
data$Gender<-as.numeric(data$Gender) ####why without doing this my output is saying std dev=0

table(data$Gender)
table(data$Student)

table(data$Married)
table(data$Ethnicity)
#data$Gender_flag<-as.numeric(ifelse(data$Gender=="Female",1,0))



data$Student_flag<-as.numeric(ifelse(data$Student=="Yes",1,0))
data$Married_flag<-as.numeric(ifelse(data$Married=="Yes", 1, 0))
data$Ethnicity_flag<-as.numeric(data$Ethnicity)
str(data)
data1<-data[c(-8,-9,-10)]
#data1<-data[c(-7,-8,-9,-10)] will be used if #data$Gender_flag<-as.numeric(ifelse(data$Gender== 'Male',1,0)) will be used
str(data1)
summary(data1)
boxplot(data1)

attach(data1)

summary(data1$Income)
upper<- 57.47+1.5*IQR(data1$Income)
upper
data1$Income[data1$Income>upper]<-upper
boxplot(data1$Income)
summary(data1$Income)


summary(data1$Limit)
upper<-5873+1.5*IQR(data1$Limit)
upper
data1$Limit[data1$Limit>upper]<-upper
boxplot(data1$Limit)
summary(data1$Limit)


summary(data1$Rating)
upper<-437.2+1.5*IQR(data1$Rating)
upper
data1$Rating[data$Rating>upper]<-upper
boxplot(data1$Rating)
summary(data1$Rating)

boxplot(data1)

summary(data1$Cards)
upper<-437.2+1.5*IQR(data1$Cards)
upper
data1$Cards[data1$Cards>upper]<-upper
boxplot(data1$Cards)
summary(data1$Cards)

boxplot(data1)
summary(data1)


library(caret)
Train<-createDataPartition(data1$Balance, p=0.70, list=FALSE)

training<-data1[Train,]
testing<-data1[-Train,]

cor(training) ###we can see there is multi linearity between variables
model<- lm(Balance~., data= training)
summary(model) 

#we can see accuracy of model is 93.55% and p value is <alpha(0.05). so we reject H0
#that means there is linear relationship between balance and other variables
#we see income, limit, cards, student are having more impact on balance as their p value<alpha(0.05)
# rating, age, education,gender, married, ethnicity has no impact on balance as p value>alpha(0.05)

library(car)
vif(model) #we ae using vif to remove multicolinearity in our model
par(mfrow=c(2,2))
plot(model) 
    
hist(training$Balance)
#we found our data is right skewed
# so to get better accuracy and to make this as normal distributed graph, we will do data transformation
hist(log(training$Balance))

#random variable selection using backward method
model1<-step(lm((Balance)~., data=training), direction="backward")
summary(model1)
vif(model1)  
#we found that income, limit, cards, student are impacting balance as their p value <0.05(alpha)
#doubt: how many times we need to do this vif and backward

par(mfrow=c(2,2))

#assumptions
plot(model1) 

#checking auto corelation
library(lmtest)
dwtest(model1)
#here p value>alpha(0.05) means we accept H0
#means there is no auto corelation between variables

#prediction::::: doubt
testing$fitted<- predict(model1, testing)
testing$original<-exp(testing$fitted)
training$newbalanace<- log(training$Balance)
