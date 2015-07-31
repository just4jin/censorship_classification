source("feature.R")

##################################################################
#
#  Classification (Naive Bayes, Logistic w/ L1 & L2 Regulation)
#
##################################################################


######################################################
#
#     Model - Text
#
######################################################

dim(dtms)

# combine dtms with censor indicator
text_data <- cbind(dtms,data$censor_indicator) 

# randomize text data 
text_data <- text_data[sample(1:nrow(text_data),nrow(text_data),replace=FALSE),]

# subsample for train and test datasets
set.seed(1)
trainIndicator = rbinom(length(data[,14]), size=1, prob=0.8) 
train_text = text_data[trainIndicator == 1,] 
test_text = text_data[trainIndicator == 0,] 

##################################################
#
#               naive bayes model 
#
##################################################
x <- as.data.frame(train_text[,-50])
y <- as.factor(train_text[,50])

nb_text = naiveBayes(x,y)
table(nb_text_pred,truth=test_text[,50])
confusionMatrix(nb_text_pred, as.factor(test_text[,50]))

nb_text_pred_raw<-predict(nb_text,test_text[,-50],type='raw')
pred <- prediction(nb_text_pred_raw[,2],test_text[,50])
perf <- performance(pred, "tpr", "fpr")
perf.auc <- performance(pred,"auc") 
auc <- perf.auc@y.values
auc # 0.587983
plot(perf)
png("nb_text_roc.png")

#*************************************************************************
#
#      Upsampling on Minority Class & Downsampling on Majority Class
#
#*************************************************************************

# train sets downsample majority
set.seed(1)

# upsample minority 
train_minority <- train_text[which(train_text[50]==1),] 
train_minority1 <- train_text[sample(1:nrow(train_minority),
                                         length(train_minority[,50]),replace=T),]
train_minority2 <- train_text[sample(1:nrow(train_minority),
                                     length(train_minority[,50]),replace=T),]
train_us <- rbind(train_minority, train_minority1,train_minority2)

# downsample majority
train_majority <- train_text[which(train_text[50]==0),] 
majorityIndicator = rbinom(length(train_majority[,50]), size=1, prob=0.5) 
train_ds <- train_majority[majorityIndicator==1,]

dim(train_ds)
dim(train_us)

# construct full datasets after sampling
train_text_ud <- rbind(train_us, train_ds) 

# randomize
train_text_ud <- train_text_ud[sample(1:nrow(train_text_ud),nrow(train_text_ud),replace=FALSE),]

dim(train_text_ud)

# naive bayes model with resampling techniques
nb_text_ud = NaiveBayes(train_text_ud[,-50],as.factor(train_text_ud[,50]))
nb_text_ud_pred <- predict(nb_text_ud,test_text)
table(nb_text_ud_pred$class,truth=test_text[,50])
confusionMatrix(nb_text_ud_pred$class, test_text[,50])
pred <- prediction(nb_text_ud_pred$posterior[,2],test_text[,50])
perf <- performance(pred, "tpr", "fpr")
perf.auc <- performance(pred,"auc") 
auc <- perf.auc@y.values
auc # 0.5881988
plot(perf)
png("nb_text_roc_ud.png")

##########################################################
#
#           L1 & L2 Logistic Regression (type=0,6)
#
#########################################################
x = train_text_ud[,-50]
y = as.factor(train_text_ud[,50])
lib_text_l1_cv<-LiblineaR(x, y,type=6,cross=10) # accuracy 0.8899917
lib_text_l2_cv<-LiblineaR(x, y,type=0,cross=10) # accuracy 0.8899917
lib_text_l1<-LiblineaR(x, y,type=6)
lib_text_l2<-LiblineaR(x, y,type=0)
lib_text_l1_pred<-predict(lib_text_l1,test_text,proba=TRUE)
lib_text_l2_pred<-predict(lib_text_l2,test_text,proba=TRUE)
table(lib_text_l1_pred$predictions,truth=test_text[,50])
table(lib_text_l2_pred$predictions,truth=test_text[,50])
confusionMatrix(lib_text_l2_pred$predictions, test_text[,50])
confusionMatrix(lib_text_l2_pred$predictions, test_text[,50])


# L1 
pred <- prediction(lib_text_l1_pred$probabilities[,2],test_text[,50])
perf <- performance(pred, "tpr", "fpr")
perf.auc <- performance(pred,"auc") 
auc <- perf.auc@y.values
auc #  0.8859119
plot(perf)
png("L1_Log_ROC.png")

# L2
pred <- prediction(lib_text_l2_pred$probabilities[,2],test_text[,50])
perf <- performance(pred, "tpr", "fpr")
plot(perf)
perf.auc <- performance(pred,"auc") 
png("L2_Log_ROC.png")
auc <- perf.auc@y.values
auc # 0.8947931

