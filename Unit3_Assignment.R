# Read in data
baseball = read.csv("https://storage.googleapis.com/dimensionless/Analytics/baseball.csv")
str(baseball)
# Ans:- Prob 1.1 :- 1232
nrow(table(baseball$Year))
#Ans:-Prob 1.2:- 47
#Prob 1.3
# Subset to only include playoffs data
baseball = subset(baseball, Playoffs == 1)
str(baseball)
#Ans:- 244
#Prob 1.4
table(baseball$Year)
#Ans:-2,4,8,10
# Prob 2.1
Playoffstable<-table(baseball$Year)
names(Playoffstable)
str(names(Playoffstable))
# Ans :- B option
#Prob 2.2
Playoffstable[c("1990","2001")]
#Ans:- 8th option
#Prob 2.3
# library(sqldf)
# NumCompetitors<-sqldf("Select count(*) NumCompetitors,Year from baseball group by Year")
# NumCompetitors
# baseball<-sqldf("SELECT b.*,n.NumCompetitors from baseball b Join NumCompetitors n on n.Year = b.Year")
# baseball$NumCompetitors
baseball$NumCompetitors<-Playoffstable[as.character(baseball$Year)]
#Prob 2.4
playoffs_8<-subset(baseball,NumCompetitors==8)
str(playoffs_8)
head(playoffs_8)
#OR
table(baseball$NumCompetitors)
#Ans:128
#Prob 3.1
baseball<-sqldf("SELECT *, CASE when RankPlayoffs =1 Then 1 else 0 end AS WorldSeries   from baseball where Playoffs = 1 ")
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
#Ans:197
#prob 3.2
model1<-glm(WorldSeries~Year,data=baseball,family=binomial)
summary(model1) # Year is significant

model2<-glm(WorldSeries~RS,data=baseball,family = binomial)
summary(model2)
# RS is not significant

model3<-glm(WorldSeries~RA,data=baseball,family = binomial)
summary(model3)
# RA is significant

model4<-glm(WorldSeries~W,data=baseball,family = binomial)
summary(model4)
# W is not significant

model5<-glm(WorldSeries~OBP,data=baseball,family = binomial)
summary(model5)
# OBP is not significant

model6<-glm(WorldSeries~SLG,data=baseball,family = binomial)
summary(model6)
# SLG is not significant

model7<-glm(WorldSeries~SLG,data=baseball,family = binomial)
summary(model7)
# SLG is not significant

model8<-glm(WorldSeries~BA,data=baseball,family = binomial)
summary(model8)
# BA is not significant

model9<-glm(WorldSeries~RankSeason,data=baseball,family = binomial)
summary(model9)
# RankSeason is significant

model10<-glm(WorldSeries~OOBP,data=baseball,family = binomial)
summary(model10)
# OOBP is not significant

model11<-glm(WorldSeries~OSLG,data=baseball,family = binomial)
summary(model11)
# OSLG is not significant

model12<-glm(WorldSeries~NumCompetitors,data=baseball,family = binomial)
summary(model12)
# NumCompetitors is significant

model13<-glm(WorldSeries~League,data=baseball,family = binomial)
summary(model13)
# League is  not significant
#Ans :- Year,RA,RankSeason,NumCompetitors
#Prob 4.1
model_multi<-glm(WorldSeries~Year+RA+NumCompetitors+RankSeason,data=baseball,family = binomial)
summary(model_multi)

#Prob 4.2
cor(baseball[c("Year","RA","RankSeason","NumCompetitors")])
#Ans:- Year/NumCompetitors
#Prob 4.3

model1<-glm(WorldSeries~Year,data=baseball,family=binomial)
summary(model1) # Year is significant

model3<-glm(WorldSeries~RA,data=baseball,family = binomial)
summary(model3)
# RA is significant

model9<-glm(WorldSeries~RankSeason,data=baseball,family = binomial)
summary(model9)
# RankSeason is significant

model13<-glm(WorldSeries~NumCompetitors,data=baseball,family = binomial)
summary(model13)
# NumCompetitors is  significant

model_Yr_RA<-glm(WorldSeries~Year+RA,data=baseball,family = binomial)
summary(model_Yr_RA)

# None of the models with two independent variables had both variables significant, 
# so none seem promising as compared to a simple bivariate model. 
# Indeed the model with the lowest AIC value is the model with just 
# NumCompetitors as the independent variable.
# 
# This seems to confirm the claim made by Billy Beane in 
# Moneyball that all that matters in the Playoffs is luck, 
# since NumCompetitors has nothing to do with the quality of the teams!

# ----------------------End------------------------
