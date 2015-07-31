install.packages("Rwordseg")
install.packages("LiblineaR")
install.packages("maxent")
install.packages("ROCR")
install.packages("LiblineaR")
install.packages("Rweibo")
install.packages("jieba")
install.packages("RTextTools")
library("klaR") 
library("maxent")
library("LiblineaR")
library("ROCR")
library("caret")
library("Rwordseg")
library("Rweibo")
library("tm")
library("RTextTools")
library("jiebaR")
library("e1071")
library("MASS")
library("AUC")
library("rpart")
library("ROCR") 

# set up work directory
setwd("~./code")

#**************************************************************************
#                 Read Data
#**************************************************************************

# read in data
data <- read.csv("./data/data.csv",sep=',',header=T, quote = "\"",  encoding='UTF-8')

# variable names & size
names(data)
dim(data)

# exclude NA values
data <- na.omit(data)

## ratio of censored vs. uncensored
sum(data$censor_indicator==0)/sum(data$censor_indicator==1) 

#**************************************************************************
#                 Text Features
#**************************************************************************

# text data cleansing
data$text=gsub("[0-9 0 1 2 3 4 5 6 7 8 9]","",data$text)
data$text=gsub("[a-zA-Z]","",data$text)
data$text = gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","", data$text)
data$text = gsub(pattern="@(\\w+)[,: ]","", data$text)
data$text = gsub('[[:punct:]]', '', data$text)
data$text = gsub('[[:cntrl:]]', '', data$text)
data$text = gsub('\\d+', '', data$text)
data$text=gsub(pattern="我在(\\w*)","",data$text)
data$text=gsub(pattern="我在这里(\\w*)","",data$text)
data$text=gsub(pattern="发表了博文(\\w*)","",data$text)
data$text=gsub(pattern="我上传了视频(\\w*)","",data$text)
data$text=gsub(pattern="视频(\\w*)","",data$text)
data$text=gsub(pattern="转发微博(\\w*)","",data$text)
data$text=gsub(pattern="转发微博","",data$text)
data$text=gsub(pattern="转发(\\w*)","",data$text)
data$text=gsub(pattern="回复(\\w*)","",data$text)
data$text=gsub(pattern="最新消息(\\w*)","",data$text)
data$text=gsub(pattern="(\\w*)美图秀秀","",data$text)
data$text=gsub(pattern="分享(\\w*)","",data$text)

# remove duplicates
data <- data[-which(duplicated(data$text)),] # 186345

# install word segmentation dictionary
installDict("./Dict/word_segmentation/宣传舆论学词库.scel","sougou_1")
installDict("./Dict/word_segmentation/网络流行新词官方推荐.scel","sougou_2")
installDict("./Dict/word_segmentation/网络流行语.scel","sougou_3")
installDict("./Dict/word_segmentation/真正的打字好秘书.scel","sougou_4")
installDict("./Dict/word_segmentation/政治学词库.scel","sougou_5")
installDict("./Dict/word_segmentation/五笔版网络流行新词官方推荐.scel","sougou_6")
installDict("./Dict/word_segmentation/2009年度百位华人公共知识分子.scel","sougou_7")
installDict("./Dict/word_segmentation/社会主义词汇.scel","sougou_8")
installDict("./Dict/word_segmentation/personalDict.txt","sougou_9")
installDict("./Dict/word_segmentation/2009年度百位华人公共知识分子.scel","sougou_10")

# word segmentation
doc_CN=list()
for(j in 1:length(data$text)){
  doc_CN[[j]]=c(segmentCN(data$text[j]))
}

# remove stopwords
stw <- readLines("./Dict/stopwords/stopwords.txt",encoding='UTF-8')
stw <- c(stw,"http","cn","www","里","称","不能","不要")
stopwords_CN<-as.vector(stw)

for(j in 1:length(data$text)){
  doc_CN[[j]] <-doc_CN[[j]][!(doc_CN[[j]] %in% stopwords_CN)]
}

# build corpus
corpus=Corpus(VectorSource(doc_CN))

# build document term matrix (dtm) with tf-idf weighting
control=list(removePunctuation=TRUE,minDocFreq=2, wordLengths = c(2, Inf), 
             stopwords=TRUE, weighting = weightTfIdf)
dtm <-DocumentTermMatrix(corpus,control)
# <<DocumentTermMatrix (documents: 165757, terms: 67415)>>
#   Non-/sparse entries: 1567225/11172940930
# Sparsity           : 100%
# Maximal term length: 11
# Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)

# reduce sparcity
dtms<-removeSparseTerms(dtm,0.99) 
# <<DocumentTermMatrix (documents: 186345, terms: 49)>>
#   Non-/sparse entries: 156769/8974136
# Sparsity           : 98%
# Maximal term length: 3
# Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)

findFreqTerms(dtms,900)
# [1] "一天"   "一种"   "一起"   "不错"   "世界"   "中国"   "人生"   "今天"   "关注"  
# [10] "发现"   "哈哈哈" "喜欢"   "嘻嘻"   "围观"   "地址"   "女人"   "孩子"   "工作"  
# [19] "希望"   "幸福"   "开心"   "强烈"   "很多"   "感觉"   "手机"   "推荐"   "支持"  
# [28] "日本"   "时间"   "明天"   "最后"   "朋友"   "期待"   "没有"   "活动"   "现在"  
# [37] "生活"   "男人"   "看到"   "看看"   "知道"   "给力"   "觉得"   "起来"   "需要"  
# [46] "鼓掌"

# write out dtm and dtms
write.csv(as.data.frame(inspect(dtm)) , file="dtm.csv")
write.csv(as.data.frame(inspect(dtms)) , file="dtms.csv")

# convert to data frame
dtms<-as.data.frame(inspect(dtms))

#*****************************************************************
#         Social Features
#*****************************************************************

# social attributes
image <- as.factor(data$image)
source <- as.factor(data$source)
province <- as.factor(data$province)
gender <- as.factor(data$gender)
verified <- as.factor(data$verified)
retweet_mid_indicator<-as.factor(data$retweet_mid_indicator)
retweet_uid_indicator<-as.factor(data$retweet_uid_indicator)
geo<-as.factor(data$geo)
created_at<-as.factor(data$created_at)
uid<-as.factor(data$uid)
delete<-as.factor(data$delete_indicator)

# social attrbutes
social <- cbind(image,source,gender,province,verified,retweet_uid_indicator,retweet_mid_indicator,geo,created_at,uid,delete)
social<-as.data.frame(social)

#*****************************************************************
#         Sentiment Features
#*****************************************************************

# install sentiment dictionary
installDict("./Dict/sentiment_words/HowNet_positive_review.txt","pos_1")
installDict("./Dict/sentiment_words/HowNet_positive_sentiment.txt","pos_2")
installDict("./Dict/sentiment_words/NTUSD_positive_simplified.txt","pos_3")
installDict("./Dict/sentiment_words/pos.txt","pos_4")
installDict("./Dict/sentiment_words/HowNet_negative_review.txt","neg_1")
installDict("./Dict/sentiment_words/HowNet_negative_sentiment.txt","neg_2")
installDict("./Dict/sentiment_words/NTUSD_negative_simplified.txt","neg_3")
installDict("./Dict/sentiment_words/neg.txt","neg_4")

# load dictionary of polarity words
pos_1 <- readLines("./Dict/sentiment_words/NTUSD_positive_simplified.txt", encoding='UTF-8')
pos_2 <- readLines("./Dict/sentiment_words/HowNet_positive_review.txt", encoding='UTF-8')
pos_3 <- readLines("./Dict/sentiment_words/HowNet_positive_sentiment.txt",encoding='UTF-8')
pos_4 <- readLines("./Dict/sentiment_words/pos.txt",encoding='UTF-8')
pos <- c(pos_1,pos_2,pos_3, pos_4)
pos <- pos[-which(duplicated(pos))]

neg_1 <- readLines("./Dict/sentiment_words/NTUSD_negative_simplified.txt", encoding='UTF-8')
neg_2 <- readLines("./Dict/sentiment_words/HowNet_negative_review.txt", encoding='UTF-8')
neg_3 <- readLines("./Dict/sentiment_words/HowNet_negative_sentiment.txt",encoding='UTF-8')
neg_4 <- readLines("./Dict/sentiment_words/neg.txt",encoding='UTF-8')
neg <- c(neg_1,neg_2,neg_3, neg_4)
neg <- neg[-which(duplicated(neg))]

# sentiment calculation
scores=rep(0,times=length(doc_CN))
score.pos=scores
score.neg=scores

for (j in 1:length(doc_CN)){
  
  # number of positive words in each row
  score.pos[j]<-sum(doc_CN[[j]] %in% pos)/length(doc_CN[[j]])
  # number of negative words in each row
  score.neg[j]<-sum(doc_CN[[j]] %in% neg)/length(doc_CN[[j]])
} 

score.pos<-as.numeric(score.pos)
score.neg<-as.numeric(score.neg)
score.pos[is.nan(score.pos)] <- 0
score.neg[is.nan(score.neg)] <- 0
positive<- as.integer((score.pos-score.neg)>0)
negative<- as.integer((score.pos-score.neg)<0)
senti_label <- as.data.frame(cbind(positive, negative))
senti_score <- as.data.frame(cbind(score.pos, score.neg))
