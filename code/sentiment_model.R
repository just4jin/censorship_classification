source("feature.R")

######################################################
#
#                  Model - Sentiment
#
######################################################

dim(senti_score)

# combine dtms with censor indicator
sentiment_data <- cbind(senti_score,data$censor_indicator) 

# randomize sentiment data 
sentiment_data <- sentiment_data[sample(1:nrow(sentiment_data),nrow(sentiment_data),replace=FALSE),]

# subsample for train and test datasets 8:2
set.seed(1)
trainIndicator = rbinom(length(sentiment_data[,3]), size=1, prob=0.8) 

train_sentiment = sentiment_data[trainIndicator == 1,] 
test_sentiment = sentiment_data[trainIndicator == 0,] 


#*************************************************************************
#
#      Upsampling on Minority Class & Downsampling on Majority Class
#
#*************************************************************************

# train sets downsample majority
set.seed(1)

# upsample minority 
train_minority <- train_sentiment[which(train_sentiment[3]==1),] 
train_minority1 <- train_sentiment[sample(1:nrow(train_minority),
                                       length(train_minority[,3]),replace=T),]
train_minority2 <- train_sentiment[sample(1:nrow(train_minority),
                                       length(train_minority[,3]),replace=T),]
train_us <- rbind(train_minority, train_minority1,train_minority2)

# downsample majority
train_majority <- train_sentiment[which(train_sentiment[3]==0),] 
majorityIndicator = rbinom(length(train_majority[,3]), size=1, prob=0.5) 
train_ds <- train_majority[majorityIndicator==1,]

dim(train_ds)
dim(train_us)

# construct full datasets after sampling
train_sentiment_ud <- rbind(train_us, train_ds) 

# randomize
train_sentiment_ud <- train_sentiment_ud[sample(1:nrow(train_sentiment_ud),nrow(train_sentiment_ud),replace=FALSE),]

dim(train_sentiment_ud)

##################################################
#
#               naive bayes model 
#
##################################################

# naive bayes model without resampling techniques
nb_sentiment_ud = NaiveBayes(train_sentiment_ud[,-3],as.factor(train_sentiment_ud[,3]))
nb_sentiment_ud_pred <- predict(nb_sentiment_ud,test_sentiment)
table(nb_sentiment_ud_pred$class,truth=test_sentiment[,3])
confusionMatrix(nb_sentiment_ud_pred$class, test_sentiment[,3])
pred <- prediction(nb_sentiment_ud_pred$posterior[,2],test_sentiment[,3])
perf <- performance(pred, "tpr", "fpr")
perf.auc <- performance(pred,"auc") 
auc <- perf.auc@y.values
auc # 0.6151021
plot(perf)
png("nb_sentiment_roc_ud.png")

#######################################################
#
#           L1 & L2 Logistic Regression (type=0,6)
#
######################################################

x <- train_sentiment_ud[,-3]
y <- as.factor(train_sentiment_ud[,3])

lib_sentiment_l1_cv<-LiblineaR(x, y,type=6,cross=10) # accuracy 0.8919885
lib_sentiment_l2_cv<-LiblineaR(x, y,type=0,cross=10) # accuracy 0.8919885
lib_sentiment_l1<-LiblineaR(x, y,type=6,bias=TRUE)
lib_sentiment_l2<-LiblineaR(x, y,type=0,bias=TRUE)
lib_sentiment_l1_pred<-predict(lib_sentiment_l1,test_sentiment,proba=TRUE)
lib_sentiment_l2_pred<-predict(lib_sentiment_l2,test_sentiment,proba=TRUE)

table(lib_sentiment_l1_pred$predictions,truth=test_sentiment[,3])
table(lib_sentiment_l2_pred$predictions,truth=test_sentiment[,3])
confusionMatrix(lib_sentiment_l1_pred$predictions, test_sentiment[,3])
confusionMatrix(lib_sentiment_l2_pred$predictions, test_sentiment[,3])


# L1
pred <- prediction(lib_sentiment_l1_pred$probabilities[,2],test_sentiment[,3])
perf <- performance(pred, "tpr", "fpr")
plot(perf)
perf.auc <- performance(pred,"auc") 
png("L1_sentiment_ROC.png")
auc <- perf.auc@y.values
auc

# L2
pred <- prediction(lib_sentiment_l2_pred$probabilities[,2],test_sentiment[,3])
perf <- performance(pred, "tpr", "fpr")
plot(perf)
perf.auc <- performance(pred,"auc") 
png("L2_sentiment_ROC.png")
auc <- perf.auc@y.values
auc 
