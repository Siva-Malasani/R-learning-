elantra=read.csv("https://storage.googleapis.com/dimensionless/Analytics/elantra.csv")
elantra_train=subset(elantra,elantra$Year<=2012)
elantra_train$Month<-as.factor(elantra_train$Month) 
str(elantra_train)
elantra_test<-subset(elantra,elantra$Year>=2013)
str(elantra_test)
elantra_test$Month<-as.factor(elantra_test$Month) 
elantra_sales<-lm(ElantraSales~Unemployment+CPI_all+CPI_energy+Queries,data=elantra_train)
summary(elantra_sales)

# Adding Month as variable
elantra_sales<-lm(ElantraSales~Month+ Unemployment+CPI_all+CPI_energy+Queries,data=elantra_train)
summary(elantra_sales)

# Month as a Factor

elantra_sales<-lm(ElantraSales~ Unemployment+CPI_all+CPI_energy+Queries+as.factor(Month),data=elantra_train)
summary(elantra_sales)
library(car)
vif(elantra_sales)
#Reducing Model
# Removing Queries
elantra_sales<-lm(ElantraSales~as.factor(Month) + Unemployment+CPI_all+CPI_energy,data=elantra_train)
summary(elantra_sales)
cor(elantra_train[,1:7])
cor(elantra_train$CPI_energy,elantra_train$CPI_all)
# Making Predictions
sales_predict<-predict(elantra_sales,newdata = elantra_test)
sales_predict
#SSE
SSE<- sum((sales_predict-elantra_test$ElantraSales)^2)
SSE
SST<-sum((elantra_test$ElantraSales-mean(elantra_train$ElantraSales))^2)
SST

# Baseline method prediction 
mean(elantra_train$ElantraSales)

# Out of Sample R-square
1-(SSE/SST)

# Largest absolute error
pred_error<- abs(elantra_test$ElantraSales-sales_predict)
max_error<-max(pred_error)
elantra_test[which.max(pred_error),]
elantra_test$Year[5]
elantra_test$Month[5]
table(Elantra_Test$Month_factor,Elantra_Test$Prediction_Error)
library("dplyr")
  elantra_test %>%
  group_by(Month,Year) %>%
  summarise(max_Prediction_error = max(pred_error)) %>%
  arrange(desc(max_Prediction_error))

elantra$Month<-as.factor(elantra$Month)  
  
library(Matrix)
sparse_mat<-sparse.model.matrix(ElantraSales~Unemployment+CPI_all+CPI_energy+Queries+Month,data = elantra)
mat<-as.matrix(sparse_mat)
mat
