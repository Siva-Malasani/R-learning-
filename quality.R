quality=read.csv("https://storage.googleapis.com/dimensionless/Analytics/quality.csv")
str(quality)
# No. of patients received poor care
table(quality$PoorCare)
c# Or using sqldf 
library(sqldf)
sqldf("SELECT count(*) from quality where PoorCare = 1 ")
sqldf("SELECT count(*) from quality where PoorCare = 0 ")

# SPlit the data into train and test data set randomly. 
# But if we want same datasets, we should first fix the seed. 
set.seed(88)
library(caTools)
split<- sample.split(quality$PoorCare,SplitRatio = 7/10)

split
table(split)
# Subsetting 
qualityTrain<-subset(quality,split==TRUE)
qualityTest<-subset(quality,split==FALSE)
# Check the structure and confirm
str(qualityTrain)
str(qualityTest)
#verification of split ratio
table(qualityTrain$PoorCare)
table(qualityTest$PoorCare)
#  Building the logistic model
QualityLog<-glm(PoorCare~OfficeVisits+Narcotics,data=qualityTrain,family=binomial)
summary(QualityLog)
QualityLog$fitted.values
library(car)
vif(QualityLog)
QualityLog$fitted.values
# Predicting the data
predictTrain<-predict(QualityLog,type = "response")
summary(predictTrain)
predictTrain
#Quick Question

lrg_2 <- glm(PoorCare~StartedOnCombination+ProviderCount,data = qualityTrain,family =binomial)
lrg_2$fitted.values
summary(lrg_2)
# Check whether we are predicting high values for PoorCare.
tapply(predictTrain,qualityTrain$PoorCare,summary)
tapply(predictTrain,INDEX = qualityTrain$PoorCare,mean)
#OR
tapply(predictTrain,qualityTrain$PoorCare,quantile,probs=seq(0,1,0.05))
qualityTrain$PoorCare
# Making Classification tables for different thresholds
# t = 0.5
conf_matrix<-table(qualityTrain$PoorCare,predictTrain>=0.5)
colnames(conf_matrix)<-c("predict=0","predict=1")
# Increase the t-value to 0.7
table(qualityTrain$PoorCare,predictTrain>=0.7)

# Decrease the t-value to 0.2
table(qualityTrain$PoorCare,predictTrain>=0.2)

table(qualityTrain$PoorCare,predictTrain>=0.5)





# ROC in R 
library(ROCR)
library(help=ROCR)
ROCRpred<-prediction(predictTrain,qualityTrain$PoorCare)


subset(qualityTrain$PoorCare,qualityTrain$MemberID==58)
# Using the performance function 

### ROC 
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)


### AUC
ROCRperfTrain<-performance(ROCRpred,"auc")
ROCRperfTrain


# Add colors
plot(ROCRperf,colorize=TRUE)

# Add labels 
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
# Multicollinearity 
library(car)
vif(QualityLog)

# Making Predictions
predictTest<-predict(QualityLog,type="response",newdata = qualityTest)
# For threshold of 0.3
table(qualityTest$PoorCare,predictTest>=0.3)
accuracy 
30/39
0.7692308
summary(predictTest)


table(qualityTrain$PoorCare)
# running on test data
ROCRpredTest<-prediction(predictTest,qualityTest$PoorCare)

ROCRperfTestROC<-performance(ROCRpredTest,"tpr","fpr")
plot(ROCRperfTestROC,colorize=TRUE)

ROCRperfTestAUC<-performance(ROCRpredTest,"auc")
ROCRperfTestAUC


as.numeric(ROCRperfTest@y.values)
auc<-as.numeric(ROCRperfTest@y.values)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
performance(ROCRpredTest,"auc")@y.values
ROCRperfTest@y.values
