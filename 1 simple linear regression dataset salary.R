#To determine the salary of the person having 14.5 years of experience in a particular field at Significance level of 0.05.

data<- read.csv(file.choose()) 

View(data)
attach(data)
names(data)
str(data)
data$YrsExp=as.numeric(data$YrsExp)
data$sal=as.numeric(data$sal)
str(data)

data1<-data[c(-1,-2)]
summary(data1)
#to check missing values
sapply(data1, function(x) sum(is.na(x)))
attach(data1)
library(lmtest)
#x=independent=YrsExp, y= dependent=sal
plot(sal~YrsExp)   
plot(data1)

cor(data1)

input<-data1

sal.lm= lm(sal~YrsExp, data= input) #### why data=input, why not data1=input
summary(sal.lm)

#accuracy of model=95.54%. it is overfitting, so we are doing data partitioning

library(caret)
Train<- createDataPartition(data1$sal, p=0.70, list=FALSE)

training<- data1[Train,]
testing<- data1[-Train,]

cor(training)
sal.lm=lm(sal~YrsExp, data= training)
summary(sal.lm)
#####again accuracy is coming 94%. what  again we need to do.

hist(training$sal)
hist(1/(training$sal))

hist(log(training$sal))
summary(log(training$sal))
sal.lm<- step(lm(log(sal)~.,data=training), direction = "backward")
summary(sal.lm)

#now we can find the model is 91% accurate. and p value <alpha.
#so we are rejecting H0. 
library(car)
vif(sal.lm)  ##########output error



y=B0+B1x
y= 25792.2 + 9450.0*14.5 #(without data partition)
y

y1=B0+B1x
y1= 10.481+0.13*14.5
y1 #(after data partition)
y2<-exp(y1)
y2


anova(sal.lm)
#here p value<alpha(0.05), so reject H). means there is linear corelation between sal and yrs of exp

plot(sal~YrsExp)
abline(sal~YrsExp, col="RED")
par(mfrow=c(2,2))

plot(sal.lm)


#conclusion= after 14.5 yrs of exp, salary will be 234685.1 with 0.05 significance level