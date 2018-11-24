# Read in data
wine = read.csv("https://storage.googleapis.com/dimensionless/Analytics/wine.csv")
str(wine)
summary(wine)
View(wine)
# Linear Regression (one variable)
model1<-lm(formula=Price~AGST,data = wine)
model1 = lm(Price ~ AGST, data=wine)
plot(Price~AGST,data=wine)
abline(model1,col="red")
summary(model1)
model1$coefficients
model1$residuals
# Sum of Squared Errors

SSE = sum(model1$residuals^2)
SSE
RMSE<-sqrt(SSE/nrow(wine))
# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE
SST<-sum((wine$Price-mean(wine$Price))^2)
# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE

#Quick Question
model_quick <- lm(Price~ HarvestRain + WinterRain,data=wine)
summary(model_quick)
model_quick$coefficients

# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain+WinterRain+Age, data=wine)
summary(model4)
summary(model3)
anova(model3,model4)
# Correlations
cor(wine$WinterRain, wine$Price)
cor.test(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

# VIF 
library(car)
vif(model1)
vif(model2)
vif(model3)
vif(model4)
vif(model5)
# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
model6<- lm(Price ~ AGST + HarvestRain + WinterRain+ FrancePop, data=wine)
summary(model6)
model7<-lm(Price~Age+FrancePop,data=wine)
summary(model7)
# Read in test set
wineTest = read.csv("https://storage.googleapis.com/dimensionless/Analytics/wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest = predict(model4,newdata = wineTest)
predictTest
wineTest$Price
# Compute R-squared
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price- mean(wine$Price))^2)
1 - SSE/SST

# Prediction using  model2
predict_model2<-predict(model2,wineTest)
SSE = sum((wineTest$Price - predict_model2)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

write.csv(wineTest,"wine_write.csv")
featurePlot(x=wine[,3:7],y=wine$Price,plot="scatter")
