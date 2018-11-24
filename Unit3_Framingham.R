# Read in the dataset
framingham = read.csv("https://storage.googleapis.com/dimensionless/Analytics/framingham.csv")
sapply(framingham,class)
# Look at structure
str(framingham)
summary(framingham)
framingham<-na.omit(framingham)
names(framingham)
table(train$TenYearCHD)
# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(framingham, split==TRUE)
summary(train)
summary(test)
test = subset(framingham, split==FALSE)
cor(train,use = "pair")
library(car)
# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)
vif(framinghamLog)

framinghamLog = glm(TenYearCHD ~ .-diaBP, data = train, family=binomial)
summary(framinghamLog)
vif(framinghamLog)

# Dropping education and currentSmoker
framinghamLog = glm(TenYearCHD ~ .-diaBP-education-currentSmoker, data = train, family=binomial)
summary(framinghamLog)
vif(framinghamLog)

# Dropping BMI and heartRate
framinghamLog = glm(TenYearCHD ~ .-diaBP-education-currentSmoker-BMI-heartRate, data = train, family=binomial)
summary(framinghamLog)
vif(framinghamLog)

# Dropping BPMeds and diabetes
framinghamLog = glm(TenYearCHD ~ .-diaBP-education-currentSmoker-BMI-heartRate-BPMeds-diabetes, data = train, family=binomial)
framinghamLog = glm(TenYearCHD ~ as.factor(male)+age+cigsPerDay+totChol+sysBP+glucose+education, data = train, family=binomial)
summary(framinghamLog)
vif(framinghamLog)

# Dropping prevalentHyp 
framinghamLog = glm(TenYearCHD ~ .-diaBP-education-currentSmoker-BMI-heartRate-BPMeds-diabetes-prevalentHyp, data = train, family=binomial)
summary(framinghamLog)
vif(framinghamLog)

pred_train<-predict(framinghamLog,type = "response")
table(train$TenYearCHD,framinghamLog$fitted.values>=0.5)
framinghamLog = glm(TenYearCHD ~.-currentSmoker-BPMeds-diaBP-heartRate-diabetes-BMI-prevalentHyp-prevalentStroke-education, data = train, family=binomial)
summary(framinghamLog)
library(ROCR)
ROCRpred<-prediction(predictions = framinghamLog$fitted.values,labels = train$TenYearCHD)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7),cex=0.1)
table(train$TenYearCHD,framinghamLog$fitted.values>=0.2)

# Feature Selection
framingham$male<-as.factor(framingham$male)
framingham$education<-as.factor(framingham$education)
for (i in c(4,6,7,8,9,16))
{
framingham[,i]<-as.factor(framingham[,i])
}
class(framingham$currentSmoker)
sapply(train,class)

# sapply(train,table)
table(train$male,train$TenYearCHD)
chisq.test(train$male,train$TenYearCHD)
chisq.test(train$BPMeds,train$TenYearCHD)
chisq.test(train$education,train$TenYearCHD)
chisq.test(train$currentSmoker,train$TenYearCHD)
chisq.test(train$diabetes,train$TenYearCHD)
table(train$diabetes,train$TenYearCHD)

# Apply t-test 
grp1<-train$age[train$TenYearCHD==0]
grp1
grp2<-train$age[train$TenYearCHD==1]

t.test(x = grp1,y = grp2,mu = 0,alternative = "two.sided",var.equal = FALSE,conf.level = 0.95)
t.test(grp1,grp2,mu = 0,paired = FALSE)
t.test(age~TenYearCHD,data=train,paired=F,conf.level=0.95)
t.test(glucose~TenYearCHD,data=train)
t.test(BMI~TenYearCHD,data=train,paired=FALSE,conf.level=0.95)
t.test(totChol~TenYearCHD,data=train)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, ifelse(predictTest >= 0.5,1,0))

# Accuracy
(1069+11)/(1069+6+187+11)

# Baseline accuracy
table(test$TenYearCHD)
(1069+6)/(1069+6+187+11) 

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0.1,1,0.1),text.adj=c(-0.2,1.7))
