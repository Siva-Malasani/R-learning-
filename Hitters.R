library(ISLR)
data("Hitters")
str(Hitters)
summary(Hitters)
getwd()
# write.csv(x = Hitters,file = "Hitters.csv")
plot(Hitters$Years,Hitters$Hits,col=Hitters$Salary,pch=19,cex=1)
Hitters<-na.omit(Hitters)
View(Hitters)

par(mfrow=c(1,1))

hist(Hitters$Salary)
hist(log(Hitters$Salary))
Hitters$Salary<-log(Hitters$Salary)
summary(Hitters)

# Split the Data
library(caTools)
set.seed(3000)
split<-sample.split(Hitters$Salary,0.8)
Train<-subset(Hitters,split==T)
Test<-subset(Hitters,split==F)
summary(Train)
unique(Train$Years)
RSS_init<-sum((Train$Salary-mean(Train$Salary))^2)
RSS<-NULL
RSS1<-NULL
RSS2<-NULL
for (i in min(Train$Years):max(Train$Years))
{ 

  RSS1[i]=sum((Train$Salary[Train$Years<=i]-mean(Train$Salary[Train$Years<=i]))^2)
  RSS2[i]=sum((Train$Salary[Train$Years>i]-mean(Train$Salary[Train$Years>i]))^2)
  RSS[i]=RSS1[i]+RSS2[i]
  print(i)
}
i
RSS
which.min(RSS)

RSS<-NULL
RSS1<-NULL
RSS2<-NULL
for (i in 1:max(Train$Hits))
{ 
  
  RSS1[i]=sum((Train$Salary[Train$Hits<=i]-mean(Train$Salary[Train$Hits<=i]))^2)
  RSS2[i]=sum((Train$Salary[Train$Hits>i]-mean(Train$Salary[Train$Hits>i]))^2)
  RSS[i]=RSS1[i]+RSS2[i]
}
RSS
which.min(RSS)
min(RSS)


## Apply CART 
library(rpart)
library(rpart.plot)
model<-rpart(Salary~Years+Hits,data = Train,minbucket=25)
prp(model)
model1<-rpart(Salary~.,data = Train,minbucket=25)
summary(model1)
prp(model1)
printcp(model1)
plotcp(model1)
pred_train1<-predict(model1)
SSE_train<-sum((pred_train1-Train$Salary)^2)
SST_train<-sum((Train$Salary-mean(Train$Salary))^2)
rsq<-1-SSE_train/SST_train
RMSE<-sqrt(sum((pred_train1-Train$Salary)^2)/nrow(Train))

######minbucket=5----------------------------------------------######

model2<-rpart(Salary~.,data = Train,minbucket=5)
summary(model2)
prp(model2)
pred_train2<-predict(model2)
SSE_train2<-sum((pred_train2-Train$Salary)^2)
SST_train<-sum((Train$Salary-mean(Train$Salary))^2)
rsq2<-1-SSE_train2/SST_train
RMSE2<-sqrt(sum((pred_train2-Train$Salary)^2)/nrow(Train))

##########Predictions model1 minbucket =25----------------------------------
pred_test1<-predict(model1,newdata = Test)
RMSE_test1<-sqrt(sum((pred_test1-Test$Salary)^2)/nrow(Test))
RMSE_test1
SSE_test1<-sum((pred_test1-Test$Salary)^2)
SST_test1<-sum((Test$Salary-mean(Train$Salary))^2)
Rsq_test1<-1-SSE_test1/SST_test1
Rsq_test1
########## Predictions model2 minbucket =5
pred_test2<-predict(model2,newdata = Test)
RMSE_test2<-sqrt(sum((pred_test2-Test$Salary)^2)/nrow(Test))
RMSE_test2
SSE_test2<-sum((pred_test2-Test$Salary)^2)
SST_test2<-sum((Test$Salary-mean(Train$Salary))^2)
Rsq_test2<-1-SSE_test2/SST_test2
Rsq_test2

# RMSE = 0.58 (unpruned tree, minbucket=5)
# Rsq = 0.50 (unpruned tree, minbucket=5)
library(rpart.plot)
prp(model)
model$cptable
plotcp(model)
library(caret)
numFolds<-trainControl(method="cv",number=10)
cpGrid<-expand.grid(cp=seq(0.001,0.025,0.001))
class(cpGrid)
set.seed(100)
model_cv<-train(Salary~.,data = Train,trControl=numFolds,tuneGrid=cpGrid,method="rpart")

prp(model_cv$finalModel)
plot(model_cv)
summary(model_cv)
model<-rpart(Salary~.,data=Train,cp=0.011)
prp(model)
plot(Train$Years,Train$Hits,col=Train$Salary)
printcp(model)
plotcp(model)
# prp(model_cv$finalModel)
# Make Predictions
pred_cv<-predict(model_cv,newdata = Test)
#RMSE 
RMSE_cv<-sqrt(sum((pred_cv-Test$Salary)^2)/nrow(Test))
# RMSE = 0.575 
SSE_cv<-sum((pred_cv-Test$Salary)^2)
SST_cv<-sum((mean(Train$Salary)-Test$Salary)^2)
Rsq_cv<-1-SSE_cv/SST_cv
# Rsq<-0.5086
# Apply Randomforest
library(randomForest) 
set.seed(50)
model_rf<-randomForest(Salary~.,data=Train,nodesize=5,ntree=500,mtry=4) 
model_rf

summary(model_rf)
plot(model_rf) 
model_rf$importance
varImpPlot(model_rf)
# Applying cv on random forest
set.seed(50)
model_rfcv<-rfcv(trainx = Train[,c(1:18,20)],trainy = Train$Salary,cv.fold = 10,step=0.95)
summary(model_rfcv)
model_rfcv$error.cv
model_rfcv<-rfcv(Train[,c(1:18,20)],Train$Salary,cv.fold = 10)
plot(model_rfcv$n.var,model_rfcv$error.cv,type="b")
#Making Predictions
pred_rf<-predict(model_rf,newdata = Test)
RMSE_rf<-sqrt(sum((pred_rf-Test$Salary)^2)/nrow(Test))
RMSE_rf
SSE_rf<-sum((pred_rf-Test$Salary)^2)
SST_rf<-sum((Test$Salary-mean(Train$Salary))^2)
Rsq_rf<-1-SSE_rf/SST_rf
Rsq_rf
varImpPlot(model_rf)

model_rfcv$predicted

# Boosted Regression Tree

str(Train)
library(xgboost)
library(Matrix)
object.size(Train)
object.size(sparse)
sparse<-sparse.model.matrix(Salary~. -1,data = Train)
sparse
str(sparse)
summary(sparse)
dim(sparse)
dimnames(sparse)
mat<-as.matrix(sparse)
object.size(mat)
sparse@Dim
sparse@Dimnames
mat
dtrain<-xgb.DMatrix(data=sparse,label=Train$Salary)
class(dtrain)
object.size(dtrain)
sparse_test<-sparse.model.matrix(Salary~.-1,data = Test)
# mat<-as.matrix(sparse_test)
dtest<-xgb.DMatrix(data=sparse_test,label=Test$Salary)
getinfo(dtrain,"label")
# Building the model
watchlist=list(train=dtrain,test=dtest)
model_xgb<-xgb.train(data=dtrain,nrounds=5000,objective="reg:linear",verbose = 1,eta=0.001,max_depth=2,watchlist = watchlist,early_stopping_rounds = 100)
model_xgb

model_xgb2<-xgb.train(data=dtrain,nrounds=5000,objective="reg:linear",verbose = 1,eta=0.001,max_depth=2,watchlist = watchlist,early_stopping_rounds = 100, xgb_model = model_xgb)
model_xgb2


xgb.plot.multi.trees(model_xgb)

imp<-xgb.importance(feature_names = colnames(Train[,-19]),model = model_xgb)
imp
xgb.plot.importance(imp)

### making predictions 

pred_salary<-predict(object = model_xgb2,newdata = dtest)

SSE_xgb<-sum((Test$Salary-pred_salary)^2)
SST_xgb<-sum((Test$Salary - mean(Train$Salary))^2)
Rsqr<- 1- (SSE_xgb/SST_xgb)
Rsqr
RMSE_xgb<-sqrt(SSE_xgb/nrow(Test))


xgb.cv()


# Randomforest using caret
library(caret)
numFolds<-trainControl(method = "oob")
tunegrid <- expand.grid(mtry=c(1:19))
model_rf_caret<-train(Salary~.,data = Train,method="rf",trControl=numFolds,tuneGrid=tunegrid)
model_rf_caret
### mtry is =5 
plot(model_rf_caret)



# Feature Engineering 

hist(Hitters$AtBat)
quantile(x = Hitters$AtBat,probs = seq(0.01,0.1,0.01))
boxplot(Hitters$AtBat)
boxplot(Hitters$Hits)
hist(Hitters$Hits)
boxplot(Hitters$CHits)
plot(Salary~log(CHits),data=Hitters)
boxplot(Hitters$CAtBat)
hist(Hitters$CAtBat)
plot(Salary~.,data=Hitters)
Hitters$CHmRun<-log(Hitters$CHmRun)
plot(Salary~log(CHmRun),data=Hitters)

model<-lm(Salary~CAtBat:Years,data = Train)
summary(model)

plot(Salary~CAtBat:Years,data = Train)
