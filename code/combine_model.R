source("feature.R")

################################################################
#
#             Construct Full Datasets
#
################################################################
# combine dtms, social and sentiment with censor indicator
data_full <- cbind(dtms, social, senti_score, data$censor_indicator) 

# randomize text data 
data_full <- data_full[sample(1:nrow(data_full),nrow(data_full),replace=FALSE),]

dim(data_full)

# # subsample - train:test = 8:2
set.seed(1)
trainIndicator = rbinom(length(data_full[,63]), size=1, prob=0.8) 

train_full = data_full[trainIndicator == 1,] 
test_full = data_full[trainIndicator == 0,] 


#*************************************************************************
#
#      Upsampling on Minority Class & Downsampling on Majority Class
#
#*************************************************************************

# train sets downsample majority
set.seed(1)

# upsample minority 
train_minority <- train_full[which(train_full[63]==1),] 
train_minority1 <- train_full[sample(1:nrow(train_minority),
                                       length(train_minority[,63]),replace=T),]
train_minority2 <- train_full[sample(1:nrow(train_minority),
                                       length(train_minority[,63]),replace=T),]
train_us <- rbind(train_minority, train_minority1,train_minority2)

# downsample majority
train_majority <- train_full[which(train_full[63]==0),] 
majorityIndicator = rbinom(length(train_majority[,63]), size=1, prob=0.5) 
train_ds <- train_majority[majorityIndicator==1,]

dim(train_ds)
dim(train_us)

# construct full datasets after sampling
train_full_ud <- rbind(train_us, train_ds) 

# randomize
train_full_ud <- train_full_ud[sample(1:nrow(train_full_ud),nrow(train_full_ud),replace=FALSE),]

dim(train_full_ud)


##################################################
#
#               naive bayes model 
#
##################################################

# naive bayes model with resampling techniques
nb_full_ud = NaiveBayes(train_full_ud[,-63],as.factor(train_full_ud[,63]))
nb_full_ud_pred <- predict(nb_full_ud,test_full)
table(nb_full_ud_pred$class,truth=test_full[,63])
confusionMatrix(nb_full_ud_pred$class, test_full[,63])
pred10 <- prediction(nb_full_ud_pred$posterior[,2],test_full[,63])
perf10 <- performance(pred10, "tpr", "fpr")
perf10.auc <- performance(pred10,"auc") 
auc <- perf10.auc@y.values
auc # 0.8725469
plot(perf)
png("nb_full_roc_ud.png")


#######################################################
#
#           L1 & L2 Logistic Regression (type=0,6)
#
######################################################

x <- train_full_ud[,-63]
y <- as.factor(train_full_ud[,63])

lib_full_l1_cv<-LiblineaR(x, y,type=6,cross=10) # accuracy 0.9498474
lib_full_l2_cv<-LiblineaR(x, y,type=0,cross=10) # accuracy 0.8899308
lib_full_l1<-LiblineaR(x, y,type=6,bias=TRUE)
lib_full_l2<-LiblineaR(x, y,type=0)
lib_full_l1_pred<-predict(lib_full_l1,test_full,proba=TRUE)
lib_full_l2_pred<-predict(lib_full_l2,test_full,proba=TRUE)

table(lib_full_l1_pred$predictions,truth=test_full[,63])
table(lib_full_l2_pred$predictions,truth=test_full[,63])
confusionMatrix(lib_full_l1_pred$predictions, test_full[,63])
confusionMatrix(lib_full_l2_pred$predictions, test_full[,63])


# L1
pred11 <- prediction(lib_full_l1_pred$probabilities[,2],test_full[,63])
perf11 <- performance(pred11, "tpr", "fpr")
plot(perf11)
perf11.auc <- performance(pred11,"auc") 
png("L1_full_ROC.png")
auc <- perf11.auc@y.values
auc # 0.8565421

# L2
pred12 <- prediction(lib_full_l2_pred$probabilities[,2],test_full[,63])
perf12 <- performance(pred12, "tpr", "fpr")
plot(perf12)
perf12.auc <- performance(pred12,"auc") 
png("L2_full_ROC.png")
auc <- perf12.auc@y.values
auc # 0.5652492

# feature coefficients
x<-as.matrix(x)
y<-as.factor(y)

glm.out = glm(y~x,family=binomial(logit))
summary(glm.out)


