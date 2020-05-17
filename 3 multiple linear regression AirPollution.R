
#objective: to find linear relationship between particles which affects air pollution in Bhosari
data<-read.csv(file.choose())
names(data)
View(data)
str(data)

data$PM10<-as.numeric(data$PM10)
data$Pb<-as.numeric(data$Pb)
data$Cd<-as.numeric(data$Cd)
data$Cu<-as.numeric(data$Cu)
data$Cr<-as.numeric(data$Cr)
data$Zn<-as.numeric(data$Zn)
data$NOx<-as.numeric(data$NOx)
data$SO2<-as.numeric(data$SO2)
data$Site_flag<-as.numeric(ifelse(data$Site=="SNDT",1,ifelse(data$Site=="Bhosari",2,3)))
#data$Date_flag<-as.factor(ifelse(data$Date))
data$Season_flag<-as.numeric(ifelse(data$Season== "Summer", 1, ifelse(data$Season=="Winter", 2, 3)))
data$Date<-NULL
data$Site<- NULL
data$Season<-NULL
data$Zn<-NULL
data$Cd<-NULL
str(data)
summary(data)
boxplot(data)

boxplot(data$PM10) #has outlier
boxplot(data$Pb) #has outlier
boxplot(data$Cu) #has outlier
boxplot(data$Cr) #has outlier
boxplot(data$NOx) #has outlier
boxplot(data$SO2) #has outlier

summary(data$PM10)
upper<-89.82 + 1.5*IQR(data$PM10); upper
data$PM10[data$PM10>upper]<-upper
boxplot(data$PM10)

summary(data$Pb)
upper<- 0.96 + 1.5*IQR(data$Pb)
data$Pb[data$Pb>upper]<-upper
boxplot(data$Pb)

summary(data$Cu)
upper<- 0.53 + 1.5*IQR(data$Cu)
data$Cu[data$Cu>upper]<- upper
boxplot(data$Cu)

summary(data$Cr)
upper<- 0.58+1.5*IQR(data$Cr)
data$Cr[data$Cr>upper]<- upper
boxplot(data$Cr)

summary(data$NOx)
upper<- 54.70+1.5*IQR(data$NOx)
data$NOx[data$NOx>upper]<- upper
boxplot(data$NOx)

summary(data$SO2)
upper<- 27.42+1.5*IQR(data$SO2)
data$SO2[data$SO2>upper]<-upper
boxplot(data$SO2)
summary(data$SO2)

#checking missing values
sapply(data$PM10, function(x) sum(is.na(x)))##no missing values
summary(data)

str(data)

##data partition
library(caret)
Train<-createDataPartition(data$PM10, p=0.7, list=FALSE)
training<- data[Train,]
testing<-data[-Train,]

cor(training) ##we can see multiple correlation in training data
model<-lm(PM10~., data=training)
summary(model)
##we can see pb, cu, cr,site flag and season flag are having impact on PM10
## and NOX and SO2 are not having impact on pm10
## accuracy of model is 84.15% and p value< alpha(0.05). so we reject Ho
## that means there is linear relationship between pm10 and other variables


###vif
library(car)
vif(model)
par(mfrow=c(2,2))
plot(model)
##from scatter plot we found that our constant variable and normality met

#random variable selection method
model1<-step(lm(PM10~.,-NOx-SO2, data= training), direction="backward")

##model1<-step(lm(PM10~., data=training), direction="backward)

vif(model1)
#assumptions
par(mfrow=c(2,2))

plot(model1)

#check auto correlation
library(lmtest)
dwtest(model1)
##p value>alpha(0.05). so we donot reject H0
## that means there is no auto correlation between variables in a row
ncvTest(model1)
##p value >alpha(0.05). so we dont reject H0
## that means there is no difference between variance

#prediction
testing$fitted<-predict(model1, testing)
testing$original<- exp(testing$fitted)
testing$newPM10<-log(training$PM10)#####doubt

