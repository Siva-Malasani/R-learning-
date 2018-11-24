

# Read in data
baseball = read.csv("https://storage.googleapis.com/dimensionless/Analytics/baseball.csv")
str(baseball)
summary(baseball)
plot(baseball$W,baseball$Playoffs)
table(W=baseball$W,Playoffs=baseball$Playoffs)
table(baseball$Playoffs)
table(baseball$W)
tapply(baseball$W,baseball$Playoffs,mean)
library(ggplot2)
ggplot(aes(Year,W,fill=as.factor(Playoffs)),data = baseball)+geom_point()
# Subset to only include moneyiball years
moneyball = subset(baseball, Year < 2002) 
tapply(moneyball$W,moneyball$Playoffs,mean)
str(moneyball)
plot(x = moneyball$W,moneyball$Playoffs)
table(moneyball$W,moneyball$Playoffs)
# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
cor(moneyball$RS,moneyball$W)
cor(moneyball$RA,moneyball$W)
cor(moneyball$RD,moneyball$W)
# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)
abline(h = 95,col="red")
plot(moneyball$RS, moneyball$W)
plot(moneyball$RA, moneyball$W)
cor(moneyball$W,moneyball$RD)
# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)
predict(WinsReg,newdata = data.frame(RD=99))
data.frame(RD=99)
str(moneyball)

# Regression model to predict runs scored
model1<-lm(RS~OBP,data=moneyball)
summary(model1)
model2<-lm(RS~SLG,data=moneyball)
summary(model2)
model3<-lm(RS~BA,data=moneyball)
summary(model3)
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
# Check for multicollinearity
library(car)
vif(RunsReg)
cor(moneyball)
cor(moneyball$BA,moneyball$SLG)
cor(moneyball[,c("OBP","SLG","BA","RS")])
RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
vif(RunsReg)

RunsAllowed<-lm(RA~OOBP+OSLG,data = moneyball)
summary(RunsAllowed)
vif(RunsAllowed)

#Making Predictions
newdata<-data.frame(OBP=0.369,SLG=0.374)

predict(RunsReg,newdata = data.frame(OBP=0.311,SLG=0.405))
predict(WinsReg,newdata = data.frame(RD=805-622))
predict(object = WinsReg,newdata = data.frame(RD=805-622),interval = "prediction",se.fit = T,level = 0.95)
table(baseball$Year,baseball$Playoffs)
cor(baseball$W,baseball$RankSeason,use = "p")
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012=c(94,88,95,88,93,94,98,97,93,94)
wins2013=c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)

table(moneyball$W>=95,moneyball$Playoffs)
