source("feature.R")

######################################################
#
#                  Model - Social
#
######################################################

dim(social)

# combine dtms with censor indicator
social_data <- cbind(social,data$censor_indicator) 

# randomize social data 
social_data <- social_data[sample(1:nrow(social_data),nrow(social_data),replace=FALSE),]

# subsample for train and test datasets 8:2
set.seed(1)
trainIndicator = rbinom(length(social_data[,12]), size=1, prob=0.8) 

train_social = social_data[trainIndicator == 1,] 
test_social = social_data[trainIndicator == 0,] 


#*************************************************************************
#
#      Upsampling on Minority Class & Downsampling on Majority Class
#
#*************************************************************************

# train sets downsample majority
set.seed(1)

# upsample minority 
train_minority <- train_social[which(train_social[12]==1),] 
train_minority1 <- train_social[sample(1:nrow(train_minority),
                                     length(train_minority[,12]),replace=T),]
train_minority2 <- train_social[sample(1:nrow(train_minority),
                                     length(train_minority[,12]),replace=T),]
train_us <- rbind(train_minority, train_minority1,train_minority2)

# downsample majority
train_majority <- train_social[which(train_social[12]==0),] 
majorityIndicator = rbinom(length(train_majority[,12]), size=1, prob=0.5) 
train_ds <- train_majority[majorityIndicator==1,]

dim(train_ds)
dim(train_us)

# construct full datasets after sampling
train_social_ud <- rbind(train_us, train_ds) 

# randomize
train_social_ud <- train_social_ud[sample(1:nrow(train_social_ud),nrow(train_social_ud),replace=FALSE),]

dim(train_social_ud)

##################################################
#
#               naive bayes model 
#
##################################################

# naive bayes model without resampling techniques
nb_social_ud = NaiveBayes(train_social_ud[,-12],as.factor(train_social_ud[,12]))
nb_social_ud_pred <- predict(nb_social_ud,test_social)
table(nb_social_ud_pred$class,truth=test_social[,12])
confusionMatrix(nb_social_ud_pred$class, test_social[,12])
pred <- prediction(nb_social_ud_pred$posterior[,2],test_social[,12])
perf <- performance(pred, "tpr", "fpr")
perf.auc <- performance(pred,"auc") 
auc <- perf.auc@y.values
auc # 0.9559007
plot(perf)
png("nb_social_roc_ud.png")

#######################################################
#
#           L1 & L2 Logistic Regression (type=0,6)
#
######################################################

x <- train_social_ud[,-12]
y <- as.factor(train_social_ud[,12])

lib_social_l1_cv<-LiblineaR(x, y,type=6,cross=10) # accuracy 0.9413629
lib_social_l2_cv<-LiblineaR(x, y,type=0,cross=10) # accuracy 0.890448
lib_social_l1<-LiblineaR(x, y,type=6,bias=TRUE)
lib_social_l2<-LiblineaR(x, y,type=0,bias=TRUE)
lib_social_l1_pred<-predict(lib_social_l1,test_social,proba=TRUE)
lib_social_l2_pred<-predict(lib_social_l2,test_social,proba=TRUE)

table(lib_social_l1_pred$predictions,truth=test_social[,12])
table(lib_social_l2_pred$predictions,truth=test_social[,12])
confusionMatrix(lib_social_l1_pred$predictions, test_social[,12])
confusionMatrix(lib_social_l2_pred$predictions, test_social[,12])


# L1
pred <- prediction(lib_social_l1_pred$probabilities[,2],test_social[,12])
perf <- performance(pred, "tpr", "fpr")
plot(perf)
perf.auc <- performance(pred,"auc") 
png("L1_Social_ROC.png")
auc <- perf.auc@y.values
auc # 0.8603821

# L2
pred <- prediction(lib_social_l2_pred$probabilities[,2],test_social[,12])
perf <- performance(pred, "tpr", "fpr")
plot(perf)
perf.auc <- performance(pred,"auc") 
png("L2_Social_ROC.png")
auc <- perf.auc@y.values
auc # 0.9559007
