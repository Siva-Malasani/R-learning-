# Read in the data

tweets = read.csv("https://storage.googleapis.com/dimensionless/Analytics/tweets.csv", stringsAsFactors=FALSE)
hist(tweets$Avg)
str(tweets)


# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -0.5)

table(tweets$Negative)
class(tweets$Negative)

# Install new packages

library(tm)
library(SnowballC)


# Create corpus
 
corpus = Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus[[1]]
corpus[[1]]$content
corpus[[1]]$meta
summary(corpus)

corpus[[1]][1]


# Convert to lower-case

corpus = tm_map(corpus,tolower)

corpus[[1]]$content

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function)

#corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)
#corpus = tm_map(corpus, PlainTextDocument)

corpus[[1]]$content
corpus[[2]]

# Look at stop words 
stopwords(kind = "en")
# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
#corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]$content
# Stem document 
corpus = tm_map(corpus, stemDocument)
#corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]$content
corpus[[2]]$content

#corpus = tm_map(corpus, PlainTextDocument)
# Create matrix

frequencies = DocumentTermMatrix(corpus)

frequencies$dimnames
frequencies$ncol
#summary(frequencies$j)
str(frequencies$j)
frequencies$v
head(frequencies$j)
head(frequencies$i)
frequencies
# Look at matrix 
View(inspect(frequencies))
View(inspect(frequencies[1000:1005,505:515]))

# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)
tweetsdf<-as.data.frame(as.matrix(frequencies))
tweetsdf$Negatives <- tweets$Negative

# Remove sparse terms
class(frequencies)
sparse = removeSparseTerms(frequencies, 0.98)
sparse$dimnames
sparse = removeSparseTerms(frequencies, 0.99)
sparse$dimnames
sparse = removeSparseTerms(frequencies, 0.995)
sparse$dimnames
class(sparse)
# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))
colSums(tweetsSparse)
# Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
colnames(tweetsdf) = make.names(colnames(tweetsdf))
# Add dependent variable

tweetsSparse$Negative = tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
splitdf = sample.split(tweetsdf$Negatives,SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

traindf <-subset(tweetsdf, splitdf==TRUE)
testdf<-subset(tweetsdf, splitdf==FALSE)


# Applying Naive Bayes 
trainSparse
testSparse
library(e1071)
model <-naiveBayes(Negative~.,data=trainSparse,laplace = 1)
summary(model)
pred_train<-predict(model,newdata = trainSparse)
table(trainSparse$Negative,pred_train)
pred_train
FALSE TRUE
FALSE    82  514
TRUE      3  228
(82+228)/(82+514+3+228)
0.3748489

pred_test<-predict(model,newdata = testSparse)
table(testSparse$Negative,pred_test)
pred_test
FALSE TRUE
FALSE    32  223
TRUE      1   98
(32+98)/(32+223+1+98)
0.3672316

model2 <-naiveBayes(Negatives~.,data=traindf,laplace = 1)
summary(model2)

pred_test<-predict(model2,newdata = testdf)
table(testdf$Negatives,pred_test)
pred_test
FALSE TRUE
FALSE     0  255
TRUE      0   99
99/310
0.3193548

table(pred_test)
table(testdf$Negatives)

# Build SVM 
library(e1071)
svmfit <-svm(Negatives~.,data = traindf,kernel='linear',cost=0.1)
pred_svm_train <-predict(svmfit,newdata = traindf)
table(traindf$Negatives,pred_svm_train)
pred_svm_train
FALSE TRUE
FALSE   592    4
TRUE     82  149
(592+149)/(592+4+82+149)
0.8960097

pred_svm_test <-predict(svmfit,newdata = testdf)
table(testdf$Negatives,pred_svm_test)
pred_svm_test
FALSE TRUE
FALSE   252    3
TRUE     73   26

(252+26)/(252+3+73+26)
0.7853107


svm_cv <- tune(svm,Negatives~.,data = traindf,kernel='linear',ranges =list(cost=c(0.1,1,10,100,1000)))
svm_cv$best.model

pred_svm<-predict(svm_cv$best.model,newdata=testdf)
table(testdf$Negatives,pred_svm)
pred_svm
FALSE TRUE
FALSE   235   20
TRUE     47   52

(235+52)/(235+20+47+52)
0.8107345


# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)

# Compute accuracy
predictCART
FALSE TRUE
FALSE   247    8
TRUE     70   29
(247+29)/(247+8+70+29)

0.779661
# Baseline accuracy 

table(testSparse$Negative)
FALSE  TRUE 
255    99 

255/(255+99)
0.720339

# Random forest model sparse 

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)


# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
predictRF
FALSE TRUE
FALSE   231   24
TRUE     52   47


# Accuracy:
(231+47)/(231+24+52+47)
0.7853107

##Random forest model dataframe

model_rf = randomForest(Negatives ~ . , data=traindf)
predictRF_df = predict(model_rf, newdata=testdf)
table(testdf$Negatives, predictRF_df)
predictRF_df
FALSE TRUE
FALSE   251    4
TRUE     65   34

(251+34)/(251+4+65+34)
0.8050847


#############TFIDF


tfidf <-  DocumentTermMatrix(corpus,control = list(weighting = function(x){weightTfIdf(x, normalize = FALSE)}))


IDF <- as.data.frame(as.matrix(tfidf))

IDF$Negative <- tweets$Negative

model3<- naiveBayes(Negative~.,data=IDF)
pred_tfidf<-predict(model3,IDF)
table(IDF$Negative,pred_tfidf)
FALSE TRUE
FALSE     0  851
TRUE      0  330

330/851
0.3877791

split= sample.split (IDF$Negative, SplitRatio =0.7)

trainIDF =subset(IDF, split==TRUE)
testIDF = subset(IDF, split == FALSE)

IDF_svm <- svm(Negative~.,data=trainIDF, kernal="linear", cost=0.1)
pred_svm<- predict(IDF_svm, testIDF)
table(testIDF$Negative, pred_svm)
pred_svm
FALSE TRUE
FALSE   255    0
TRUE     99    0
255/(255+99)
0.720339


# Binomial Naive Bayes

X <- as.data.frame(as.matrix(sapply(tweetsdf[-3290],function(x){as.factor(ifelse(x>0,1,0))})))

View(X[1:100,1:20])

X$Negative <- tweetsdf$Negatives
set.seed(42)
X_split = sample.split(X$Negative, SplitRatio = 0.7)
X_train = subset(X,X_split==T)
X_test = subset(X,X_split==F)

model_nb<- naiveBayes(Negative~.,data=X_train,laplace = 1)
pred_nb <-predict(model_nb,newdata = X_test)
table(X_test$Negative,pred_nb)


