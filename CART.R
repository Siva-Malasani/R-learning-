
stevens<-read.csv("https://storage.googleapis.com/dimensionless/Analytics/stevens.csv")

a = data.frame('Docket'='abc','Term'=1950,'Circuit'='10th','Issue'='JudicialPower','Petitioner'="AMERICAN.INDIAN",'Respondent'="AMERICAN.INDIAN","LowerCourt"="conser","Unconst"=0,stringsAsFactors = T)

levels(a$LowerCourt)<-levels(Train$LowerCourt)
levels(a$Circuit)<-levels(Train$Circuit)
levels(stevens$Circuit)
sapply(a,class)
names(Train)
str(stevens)
library(readr)
Test[1,]
# Converting Unconst and Reverse as factors.
#stevens$Unconst<-as.factor(stevens$Unconst)
#stevens$Reverse<-as.factor(stevens$Reverse)
str(stevens)
library(caTools)
set.seed(3000)
spl<-sample.split(stevens$Reverse,0.7)
Train<-subset(stevens,spl==TRUE)
#Train$Docket <- NULL
#Train$Term <- NULL
Test<-subset(stevens,spl==FALSE)
#Test2 <-Test[1:5,]
table(Train$Reverse)
str(Train)
str(Test)
library(rpart)
library(rpart.plot)
StevensTree<-rpart( Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,method ="class",cp=0.19)
summary(StevensTree)
plotcp(StevensTree)
prp(StevensTree)

predTrain<-predict(StevensTree,newdata = Train,type = "class")

predTrain
table(Train$Reverse,predTrain)
# Don't use "-" sign with a categorical variable while creating model using rpart, It will show an error
#StevensTree<-rpart(Reverse~.-Docket, data=Train,method ="class",cp=0.2)

pred<-predict(StevensTree,newdata = Test,type="class")
table(Test$Reverse,pred)
summary(StevensTree)
library(rpart.plot)
prp(StevensTree,cex=0.8)
rpart.plot(StevensTree,cex =0.8)
info.gain.rpa
plot(StevensTree)
text(StevensTree,cex=0.8)
plotcp(StevensTree)
prp(prune(StevensTree,cp=0.051))

StevensLog<-glm( Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,family=binomial)
PredictLog<-predict(StevensLog,newdata = Test,type="response")

PredictCart<-predict(StevensTree,newdata = Test,type="class")
str(PredictCart)
PredictCart
table(Test$Reverse,PredictLog>=0.5)
table(Test$Reverse,PredictCart)

(71+41)/(36+22+71+41)

PredictROC<-predict(StevensTree,newdata = Test)
PredictROC1<-predict(StevensTree,newdata = Test,type="prob")
library(ROCR)
PredictROC[,2]
ROCRpred<-prediction(PredictROC[,2],Test$Reverse)
ROCRperf<-performance(ROCRpred,'tpr','fpr')
performance(ROCRpred,'auc')
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1))
tapply(PredictROC[,2],Test$Reverse,summary)

# Quick Question

as.numeric(performance(ROCRpred,'auc')@y.values)
set.seed(3000)
StevensTree2<-rpart( Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,method ="class",minbucket=5)
prp(StevensTree2,cex=0.2)
StevensTree2$cptable
plotcp(StevensTree2)
StevensTree2$cptable

Pruned_tree<-prune.rpart(tree = StevensTree2,cp=0.036)
prp(Pruned_tree)
Pruned_tree$cptable
pred_prune<-predict(Pruned_tree,newdata = Test,type="class")
table(Test$Reverse,pred_prune)
prp(Pruned_tree)
prp(StevensTree2,cex=0.5)

StevensTree3<-rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,method ="class",minbucket=100)
prp(StevensTree3)
summary(StevensTree3)
# Using the tree library
library(tree)
model_tree<-tree(as.factor(Reverse) ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data = Train,minsize=25)
plot(model_tree)
text(model_tree,cex=0.8)
summary(model_tree)
model_tree
# Making Prediction 
pred_tree<-predict(model_tree,Test,type="class")
pred_tree
table(Test$Reverse,pred_tree)
#Pruning the tree
cv.model<-cv.tree(model_tree,FUN = prune.misclass)
cv.model
par(mfrow=c(1,2))
plot(cv.model$size,cv.model$dev,type="b")
plot(cv.model$k,cv.model$dev,type="b")
prune.tree<-prune.misclass(model_tree,best = 4)
prune.tree

plot(prune.tree)
par(mfrow=c(1,1))
text(prune.tree,cex=0.7)
pred_prune<-predict(prune.tree,Test,type="class")
table(Test$Reverse,pred_prune)
# Random Forest
library(randomForest)
set.seed(50)
StevensForest<-randomForest(as.factor(Reverse)~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,ntree=500,mtry=3)
StevensForest
StevensForest$importance
plot(StevensForest)

varImpPlot(StevensForest)
plot(StevensForest$err.rate[,1])
mean(StevensForest$err.rate[,1])
set.seed(50)
cv_rf<-rfcv(trainx = Train[,3:8],trainy = as.factor(Train$Reverse),cv.fold = 10,step=0.95)

cv_rf$error.cv

plot(cv_rf$n.var,cv_rf$error.cv)


plot(cv_rf$n.var,cv_rf$error.cv,type="b")
cv_rf
cv_rf$n.var
cv_rf$error.cv
plot(cv_rf)
(StevensForest)
par(mfrow=c(2,1))
plot(StevensForest)
StevensForest$err.rate
varImpPlot(StevensForest)
summary(StevensForest)
# Converting to factors
Train$Reverse<-as.factor(Train$Reverse)
Test$Reverse<-as.factor(Test$Reverse)

PredictForest<-predict(StevensForest,newdata = Test,type = "class")
PredictForest
table(Test$Reverse,PredictForest)

PredictForest_Train<-predict(StevensForest,newdata = Train,type = "class")
PredictForest
table(Train$Reverse,PredictForest_Train)

# Apply randomForest using caret
library(caret)
numFolds=trainControl(method = "cv",number = 10)
Grid<-expand.grid(mtry=seq(1,6,1))
names(cpGrid)
model_ca_rf<-train(as.factor(Reverse)~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,method="rf",tuneGrid=Grid,ntree=300,trControl = numFolds)

plot(model_ca_rf)
# Quick Question 
set.seed(100)
StevensForest2<-randomForest(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,nodesize=25,ntree=1000)
PredictForest2<-predict(StevensForest2,newdata = Test)
table(Test$Reverse,PredictForest2)


# Quick Question 2
set.seed(200)
StevensForest3<-randomForest(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,nodesize=25,ntree=200)
PredictForest3<-predict(StevensForest3,newdata = Test)
table(Test$Reverse,PredictForest3)
table(Test$Reverse,PredictForest2)

# Cross Validation 
# install.packages("caret")
library("caret")
# install.packages("ggplot2")
# install.packages("class")
# install.packages("e1071")
numFolds<-trainControl(method="cv",number = 10)
numFolds
cpGrid<-expand.grid(cp=seq(0.01,1,0.01))
class(cpGrid)
set.seed(50)
model<-train(as.factor(Reverse)~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

model
summary(model)

model$finalModel
prp(model$finalModel)
plot(model)
pred_cv<-predict(model,newdata = Test)
model$modelInfo

prp(model$finalModel)
model$finalModel
table(Test$Reverse,pred_cv)
StevensTreeCV<-rpart( Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,method ="class",cp=0.17)
prp(StevensTreeCV)

PredictTreeCV<-predict(StevensTreeCV,newdata = Test)
PredictTreeCV
table(Test$Reverse,PredictTreeCV[,2]>0.5)
prp(StevensTreeCV)
plot(model)
table(Test$Reverse)

tapply(Test$Reverse,Test$LowerCourt,table)
#####################

lbr<-subset(Train,Train$LowerCourt=="liberal")
table(Train$Reverse)
table(lbr$Reverse)

model<-train(as.factor(Reverse)~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,method="glm",trControl=numFolds)
prp(model$finalModel)
summary(model)
pred_glm<-predict(model,Test,type="prob")
table(Test$Reverse,pred_glm[,2]>=0.5)
(67+47)/170
??sample.split


# XGBOOST
library(xgboost)
library(Matrix)
# Creating sparse matrix
require(Matrix)
sparse_matrix<-sparse.model.matrix(Reverse~.-1-Docket-Term,data = Train)
sparse_matrix@Dim
sparse_matrix@Dimnames[[2]]
mat<-as.matrix(sparse_matrix)
class(sparse_matrix)
dtrain<-xgb.DMatrix(data=sparse_matrix,label=Train$Reverse)
class(dtrain)
colnames(dtrain)
object.size(sparse_matrix)
object.size(Train)
object.size(dtrain)
object.size(mat)
# Repeating same for test data
sparse_matrix_test<-sparse.model.matrix(Reverse~.-Docket-Term,data=Test)
sparse_matrix_test2<-sparse.model.matrix(Reverse~.-Docket-Term,data=Test2)
mat2 <- as.matrix(sparse_matrix_test2)
sparse_matrix_test2@Dim
dtest<-xgb.DMatrix(data=sparse_matrix_test,label=Test[,9])
dtest2<-xgb.DMatrix(data=sparse_matrix_test2,label=Test2[,9])

object.size(dtest)
getinfo(dtrain,"label")
# Applying the model
watchlist=list(train = dtrain, test=dtest)

model_xgb<-xgb.train(data=dtrain,eta=0.01,max_depth=6,objective="binary:logistic",nrounds = 1000,verbose=1,watchlist = watchlist,early_stopping_rounds = 100)

model_xgb$evaluation_log
class(Train$Reverse)
#cv<-xgb.cv(data=dtrain,eta=0.01,max_depth=1,objective="binary:logistic",verbose=1,nfold = 10,prediction = T,nround=100)
pred_xgb<-predict(model_xgb,dtest)
table(Test$Reverse,pred_xgb>=0.5)
imp<-xgb.importance(colnames(sparse_matrix),model = model_xgb)

#xgb.plot.tree(colnames(sparse_matrix),model = model_xgb,n_first_tree = 1)
xgb.plot.importance(imp,cex=0.5)

model_cv<-xgb.cv(params=list(eta=0.01,max_depth=1,objective="binary:logistic"),nrounds = 1000,nfold = 5,data = dtrain,showsd = T,stratified = T,print_every_n = 10,early_stopping_rounds = 20,maximize = F)

params=list(eta=0.01,objective="binary:logistic",max_depth=1)

xgb.cv(params = params,data = dtrain,nfold = 10,nrounds=1000,stratified = T,early_stopping_rounds = 100,showsd = F,maximize = F,print_every_n = 10)

# Applying SVM 

library(e1071)

SVMfit <- svm(as.factor(Reverse)~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,kernel="linear",cost =100)
summary(SVMfit)
tune.out<-tune(svm,as.factor(Reverse)~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,kernel='linear',ranges=list(cost=c(0.1,1,10,100,1000)),data=Train)

tune.out$best.model

pred_cv_svm<- predict(tune.out$best.model,newdata=Test)
table(Test$Reverse,pred_cv_svm)


pred_svm<-predict(SVMfit,newdata = Test)
table(Test$Reverse,pred_svm)

SVMfit <- svm(as.factor(Reverse)~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,kernel="radial",cost =100,gamma=0.5)

summary(SVMfit)

pred_svm<-predict(SVMfit,newdata = Test)
table(Test$Reverse,pred_svm)

tune.out<-tune(svm,as.factor(Reverse)~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,kernel='radial',ranges=list(cost=c(0.1,1,10,100),gamma=c(0.25,0.5,1,2,3)),data=Train)

tune.out$best.model

tune.out$performances$error

tune.out$best.performance
pred_cv_svm<- predict(tune.out$best.model,newdata=Test)
table(Test$Reverse,pred_cv_svm)
# Only using LowerCourt
Train$Reverse<-as.factor(Train$Reverse)
SVMfit <- svm(Reverse~LowerCourt,data=Train,kernel="linear",cost =100,gamma=0.5)

summary(SVMfit)
#plot(SVMfit,Train,formula = Reverse~LowerCourt)

pred_svm<-predict(SVMfit,newdata = Test)
table(Test$Reverse,pred_svm)

# Applying kernel = "poly"
set.seed(1)
tune.out<-tune(svm,as.factor(Reverse)~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,kernel='poly',ranges=list(cost=c(0.1,1,10,100),degree=c(1,2,3,4,5)),data=Train)

tune.out$best.model

tune.out$performances$error

tune.out$best.performance
pred_cv_svm<- predict(tune.out$best.model,newdata=Test)
table(Test$Reverse,pred_cv_svm)



# Applying Naive Bayes

model_nb<- naiveBayes(as.factor(Reverse)~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train)
model_nb

pred_test<-predict(model_nb,newdata = Test)

table(Test$Reverse,pred_test)


