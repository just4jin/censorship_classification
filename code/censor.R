#*******************************************************************
#
#         descriptive data analysis on censored data
#
#*******************************************************************

# read in censored dataset
censor <- read.csv("censor.csv",sep=';',header=T, quote = "\"",  encoding='UTF-8')

# exclude NA values
censor <- na.omit(censor)

# text data cleansing
censor$text=gsub("[0-9 0 1 2 3 4 5 6 7 8 9]","",censor$text)
censor$text=gsub("[a-zA-Z]","",censor$text)
censor$text = gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","", censor$text)
censor$text = gsub(pattern="@(\\w+)[,: ]","", censor$text)
censor$text = gsub('[[:punct:]]', '', censor$text)
censor$text = gsub('[[:cntrl:]]', '', censor$text)
censor$text = gsub('\\d+', '', censor$text)
censor$text=gsub(pattern="我在(\\w*)","",censor$text)
censor$text=gsub(pattern="我在(\\w*)","",censor$text)
censor$text=gsub(pattern="我在这里(\\w*)","",censor$text)
censor$text=gsub(pattern="发表了博文(\\w*)","",censor$text)
censor$text=gsub(pattern="我上传了视频(\\w*)","",censor$text)
censor$text=gsub(pattern="视频(\\w*)","",censor$text)
censor$text=gsub(pattern="转发微博(\\w*)","",censor$text)
censor$text=gsub(pattern="转发微博","",censor$text)
censor$text=gsub(pattern="转发(\\w*)","",censor$text)
censor$text=gsub(pattern="回复(\\w*)","",censor$text)
censor$text=gsub(pattern="最新消息(\\w*)","",censor$text)
censor$text=gsub(pattern="(\\w*)美图秀秀","",censor$text)
censor$text=gsub(pattern="分享(\\w*)","",censor$text)

# remove duplicates
censor <- censor[-which(duplicated(censor$text)),] # 11919

# word segmentation
doc_CN=list()
for(j in 1:length(censor)){
  doc_CN[[j]]=c(segmentCN(censor[j]))
}

# remove stopwords
stw <- readLines("./Dict/stopwords/stopwords.txt",encoding='UTF-8')
stw <- c(stw,"http","cn","www","里","称","不能","不要")
stopwords_CN<-as.vector(stw)

for(j in 1:length(censor)){
  doc_CN[[j]] <-doc_CN[[j]][!(doc_CN[[j]] %in% stopwords_CN)]
}

# build corpus
corpus=Corpus(VectorSource(doc_CN))

# build document term matrix (dtm)
control=list(removePunctuation=TRUE,minDocFreq=2, wordLengths = c(2, Inf), 
             stopwords=TRUE, weighting = weightTfIdf)
dtm <-DocumentTermMatrix(corpus,control)

# <<DocumentTermMatrix (documents: 11919, terms: 20812)>>
#   Non-/sparse entries: 124744/247933484
# Sparsity           : 100%
# Maximal term length: 9
# Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)

inspect(dtm[1:2,1:5])
findFreqTerms(dtm,100)
# [1] "中国"   "人民"   "今天"   "关注"   "北京"   "历史"   "吃惊"   "哈哈哈" "嘻嘻"  
# [10] "围观"   "国家"   "威武"   "媒体"   "孩子"   "宁波"   "官员"   "希望"   "幸福"  
# [19] "应该"   "律师"   "必须"   "怒骂"   "思考"   "抓狂"   "推广"   "支持"   "政府"  
# [28] "教育"   "新闻"   "日本"   "时代"   "求证"   "没有"   "爱国"   "现在"   "看到"  
# [37] "看看"   "真相"   "知道"   "社会"   "给力"   "继续"   "蜡烛"   "表叔"   "表哥"  
# [46] "警察"   "评论"   "话筒"   "起来"   "轉發"   "辟谣"   "鄙视"   "钓鱼岛" "问题"  
# [55] "领导"   "香港"   "鼓掌"  
findFreqTerms(dtms, 200)
# [1] "中国"   "人民"   "吃惊"   "哈哈哈" "围观"   "威武"   "宁波"   "日本"   "没有"  
# [10] "表哥"   "话筒"   "香港"  

# Associate terms
findAssocs(dtm, "中国", corlimit=0.09)
#         中国
# 新闻界   0.11
# 豆腐     0.11
# 一道     0.10
# 女婿     0.10
# 新闻记者 0.10
# 此风     0.10
# 肆意     0.10
# 身影     0.10
# 不配     0.09
# 同类     0.09
# 官二代   0.09
# 法国     0.09
# 百年     0.09
# 苍蝇     0.09
# 获得     0.09

findAssocs(dtm, "钓鱼岛", corlimit=0.1)
#           钓鱼岛
# 收复       0.32
# 争端       0.20
# 寸土不让   0.20
# 占领       0.19
# 日日       0.19
# 买不起     0.17
# 决不       0.15
# 收回       0.15
# 社保       0.15
# 不长       0.14
# 小脑       0.14
# 棉袄       0.14
# 洗澡       0.14
# 解放       0.14
# 出征       0.13
# 放弃       0.13
# 法拉利     0.13
# 派到       0.13
# 钓鱼台     0.13
# 养老       0.11
# 城管       0.10

# remove  sparse terms
dtms<-removeSparseTerms(dtm,0.999)
# <<DocumentTermMatrix (documents: 11919, terms: 2141)>>
#   Non-/sparse entries: 77254/25441325
# Sparsity           : 100%
# Maximal term length: 7
# Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)

findFreqTerms(dtms,100)
# [1] "中国"   "人民"   "今天"   "关注"   "北京"   "历史"   "吃惊"   "哈哈哈" "嘻嘻"  
# [10] "围观"   "国家"   "威武"   "媒体"   "孩子"   "宁波"   "官员"   "希望"   "幸福"  
# [19] "应该"   "律师"   "必须"   "怒骂"   "思考"   "抓狂"   "推广"   "支持"   "政府"  
# [28] "教育"   "新闻"   "日本"   "时代"   "求证"   "没有"   "爱国"   "现在"   "看到"  
# [37] "看看"   "真相"   "知道"   "社会"   "给力"   "继续"   "蜡烛"   "表叔"   "表哥"  
# [46] "警察"   "评论"   "话筒"   "起来"   "轉發"   "辟谣"   "鄙视"   "钓鱼岛" "问题"  
# [55] "领导"   "香港"   "鼓掌"  
findFreqTerms(dtms, 200)
# [1] "中国"   "人民"   "吃惊"   "哈哈哈" "围观"   "威武"   "宁波"   "日本"   "没有"  
# [10] "表哥"   "话筒"   "香港" 

# write out dtm and dtms
write.csv(as.data.frame(inspect(dtm)) , file="dtm_censor.csv")
write.csv(as.data.frame(inspect(dtms)) , file="dtms_censor.csv")
