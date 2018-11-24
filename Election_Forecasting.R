polling<-read.csv("https://storage.googleapis.com/dimensionless/Analytics/PollingData.csv")
str(polling)
library(dplyr) 
table<-table(polling$State)
subset(names(table),table==2)
summary(polling)
sapply(polling,function(x){sum(is.na(x))})
apply(polling,2,function(x){sum(is.na(x))})
head(polling)
polling_table<-tbl_df(polling)
polling_table
class(polling_table)
table(polling$Year)
summary(polling)
simple<-polling[c("Rasmussen","SurveyUSA","DiffCount","PropR")]
simple<-sqldf("select `Rasmussen`,`SurveyUSA`,`DiffCount`,`PropR` from polling")
summary(simple)
library(mice)
md.pattern(simple)
imp_mice<-mice(simple,seed=144)

imp_mice$imp$SurveyUSA
imp_mice$imp$SurveyUSA
imp_mice$imp$Rasmussen
summary(imp_mice)
imputed<-complete(imp_mice,1)
imputed
imputed<-complete(mice(simple,seed=144))
summary(imputed)
library(lattice)
stripplot(imp_mice,pch=19,cex=1)
stripplot()
# xyplot(imp_mice,SurveyUSA~Rasmussen|.imp, pch=20, cex=0.6)
polling$Rasmussen<-imputed$Rasmussen
polling$SurveyUSA<-imputed$SurveyUSA
summary(polling)
Train<-subset(polling,Year==2004|Year==2008)
# library(sqldf)
# Train<-sqldf("Select * from polling where year=2004 or year=2008 ")
# Test<-sqldf("Select * from polling where year=2012 ")
str(Train)
Test<-subset(polling,Year==2012)
str(Test)
table(Train$Republican)
sign(-20)
sign(100)
sign(0)
table(sign(Train$Rasmussen))
table(sign(Train$Rasmussen))
table(Actual=Train$Republican,Predicted=sign(Train$Rasmussen))
  # Checking for Multi-Collinearity
cor(polling)
str(polling)
cor(polling[-1])
#Building the model
mod1<-glm(Republican~PropR,data=Train,family = binomial)
summary(mod1)
#Making Predictions on Train data
pred1<-predict(mod1,newdata = Train,type = "response")
pred1
table(Train$Republican,pred1>=0.5)
#Improving the Model
mod2<-glm(Republican~SurveyUSA+DiffCount,data=Train,family="binomial")
summary(mod2)
library(car)
vif(mod2)
# Making a model using all variables
mod3<-glm(Republican~.-State-Year,data=Train,family="binomial")
summary(mod3)
vif(mod3)
# Choosing Rasmussen and DiffCount
mod4<-glm(Republican~Rasmussen+DiffCount,data=Train,family="binomial")
summary(mod4)
vif(mod4)
table(Train$Republican,mod4$fitted.values>=0.5)
# Making Predictions
pred4<-predict(mod4,newdata = Test,type ="response")
table(Test$Republican,pred4>=0.5)
str(pred4)
#Making Predictions
pred2<-predict(mod2,newdata = Train,type = "response")
#Confusion Matrix
table(Train$Republican,pred2>=0.5)
#Making Predictions on Test data for smart baseline model
table(Test$Republican,sign(Test$Rasmussen))
pred_test<-predict(mod2,newdata = Test,type = "response")
#Confusion Matrix 
table(Test$Republican,pred_test>=0.5)
# Seperating the Error
#Test2<-cbind(Test,pred_test)
subset(Test,Republican==0 & pred4>=0.5)


#library(sqldf)
#sqldf("select * FROm Test2 WHERE Republican==0 AND pred_test>0.5")
