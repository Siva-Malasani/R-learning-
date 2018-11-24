NBA=read.csv("https://storage.googleapis.com/dimensionless/Analytics/NBA_train.csv")
names(NBA_train)
str(NBA)
table(NBA$W,NBA$Playoffs)
tapply(NBA$W,NBA$Playoffs, mean)
NBA_test=read.csv("https://storage.googleapis.com/dimensionless/Analytics/NBA_test.csv")
NBA$PTSdiff=NBA$PTS-NBA$oppPTS
str(NBA)
str(NBA_test)
plot(NBA$PTSdiff,NBA$W)
WinsReg = lm(W~PTSdiff,NBA)
summary(WinsReg)
NBA_test$PTSdiff<-NBA_test$PTS-NBA_test$oppPTS
pred<-predict(WinsReg,newdata = NBA_test)
SSE<-sum((NBA_test$W-pred)^2)
SST<-sum((NBA_test$W-mean(NBA$W))^2)
1-SSE/SST
NBA$X2P
# Eq. of line 
# W = 41+ 0.0326*PTSdiff
# IF we want to win atleast 42 games then 
# 42 = 41 + 0.0326*PTSdiff 
# PTSdiff = (42-41)/0.0326 = 30.67 
# So we need to score atleast 31 more points then we allow to win atleast 42 games.
# Lets predict PTS scored 
PointsReg=lm(PTS~X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK,NBA)
summary(PointsReg)
#PointsReg=lm(PTS~X2PA + X3PA + FTA + AST ,NBA)
summary(PointsReg)
str(PointsReg)
SSE<-sum(PointsReg$residuals^2)
RMSE<-sqrt(SSE/nrow(NBA))
mean(NBA$PTS)
# Lets make a model without TOV
PointsReg2=lm(PTS~X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK,NBA)
summary(PointsReg2)
# Remove DRB (defensive rebounds)
PointsReg3=lm(PTS~X2PA + X3PA + FTA + AST + ORB + STL + BLK,NBA)
summary(PointsReg3)
vif(PointsReg3)
# remove BLOCKS 
PointsReg4=lm(PTS~X2PA + X3PA + FTA + AST + ORB + STL,NBA)
cor(NBA[,-c(1,2)])
# Best Model

points_rg <- lm(PTS~FG+FT+X3P,data = NBA) 
library(car)
vif(points_rg)
summary(points_rg)
summary(PointsReg4)
SSE_4=sum(PointsReg4$residuals^2)
RMSE_4=sqrt(SSE_4/nrow(NBA))
RMSE_4
NBA_test<-read.csv("NBA_test.csv")
PointsPrediction<-predict(points_rg,newdata=NBA_test)
summary(PointsPrediction)
SSE<-sum((PointsPrediction-NBA_test$PTS)^2)
SST<-sum((mean(NBA$PTS)-NBA_test$PTS)^2)
Rsq<-1-SSE/SST
RMSE=sqrt(SSE/nrow(NBA_test))
RMSE
mean(NBA$PTS)
NBA_test$PTS
nrow(NBA_test)
library(car)
vif(points_rg)
mod1<-lm(PTS~.-SeasonEnd-Team-W-Playoffs-oppPTS,NBA)
summary(mod1)
mod2<-lm(PTS~FG+X2P,NBA)
summary(mod2)
vif(mod2)
mod3<-lm(PTS~FG+X3P,NBA)
summary(mod3)
vif(mod3)
mod4<-lm(PTS~FG+X3P+FT,NBA)
summary(mod4)
vif(mod4)
pred<-predict(mod4,NBA_test)
sum((pred-NBA_test$PTS)^2)
