install.packages("sqldf")
library(sqldf)
library(Rwordseg)
library(Rweibo)
library("Rwordseg")
library("Rweibo")
library("tm")
library("RTextTools")
library("jiebaR")
library("devtools")
install.packages("wordcloud")
library(wordcloud)

# read a CSV file 
twt<-read.csv("data.csv",sep=",",header=T,encoding="utf-8") 

# data cleaning
twt$text=gsub("[0-9 0 1 2 3 4 5 6 7 8 9]","",twt$text)
twt$text=gsub("[a-zA-Z]","",twt$text)
# remove url
twt$text = gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","", twt$text)
# remove reposted uid (i.e. @???????)
twt$text = gsub(pattern="@(\\w+)[,: ]","", twt$text)
twt$text <- lapply(twt$text, removeEnglish)
twt$text = gsub('[[:punct:]]', '', twt$text)
twt$text = gsub('[[:cntrl:]]', '', twt$text)
twt$text = gsub('\\d+', '', twt$text)
twt$text=gsub(pattern="我在 :(\\w*)","",twt$text)
twt$text=gsub(pattern="我在:(\\w*)","",twt$text)
twt$text=gsub(pattern="我在这里 :(\\w*)","",twt$text)
twt$text=gsub(pattern="我在这里:(\\w*)","",twt$text)
twt$text=gsub(pattern="发表了博文(\\w*)","",twt$text)
twt$text=gsub(pattern="我上传了视频(\\w*)","",twt$text)
twt$text=gsub(pattern="转发微博(\\w*)","",twt$text)
twt$text=gsub(pattern="(\\w*)美图秀秀","",twt$text)
twt$text=gsub(pattern="回复(\\w*)","",twt$text)

#stopwords
stw <- readLines("./Dict/stopwords/stopwords.txt",encoding='UTF-8')

# select all rows based on days
date <- as.character(as.Date(twt$created_at))
text <- twt$text
# twt$date <- twt$date[which(duplicated(twt1$text))]
# text <- twt[!duplicated(twt$text)]
twt1 <- as.data.frame(cbind(date,text ))
# twt1<-twt1[order(twt1$date),]
twt1 <- subset(twt1, !duplicated(twt1[,2]) )

               
day="2012-09-16"
date_calc <- function(day){
  doc <- twt1[which(twt1$date==day),]
  doc_CN=list()
  for(j in 1:length(doc$text)){
    doc_CN[[j]]=c(segmentCN(as.character(doc$text[j])))
  }  
  
  
  doc_CN=list()
  for(j in 1:length(twt$text)){
    doc_CN[[j]]=c(segmentCN(as.character(twt$text[j])))
  } 
  
  # build up corpus
  corpus=Corpus(VectorSource(doc_CN))
  
  # build up Term document Matrix (tdm)
  control=list(removePunctuation=TRUE,minDocFreq=2,
               stopwords=FALSE,wordLengths = c(1,Inf))#,weighting = weightTf)
#   tdm=TermDocumentMatrix(corpus,control)
#   dtm=DocumentTermMatrix(corpus,control)
#   findFreqTerms(dtm, 10)
#   findAssocs(dtm, "一个", 0.5)
#   inspect(removeSparseTerms(dtm, 0.6))
#   inspect(dtm)
#   wordcloud(findFreqTerms(dtm, 5),encoding="utf-8")
#   library(tm)
#   install.packages("cluster")
#   library(cluster)
#   dtm<-removeSparseTerms(dtm, 0.9)
#   dist_dtm<- dissimilarity(dtm,method="cosine")
#   hc <- hclust(dist_dtm, method = 'ave')
#   plot(hc, xlab = '')
#   
#   install.packages("topicmodels")
#   library(topicmodels)
  # match sentiment scale and set weights
  scores=rep(0,times=length(doc_CN))
  score.pos=scores
  score.neg=scores
  net_score=scores
  net_score1=scores
  size=scores

#   score.pos=rep(0,times=length(doc_CN))
#   score.neg=rep(0,times=length(doc_CN))
#   net_score=rep(0,times=length(doc_CN))
#   net_score1=rep(0,times=length(doc_CN))
#   weibo.scores=rep(0,times=length(doc_CN))
  
  for (j in 1:length(doc_CN)){
    # remove stopwords
    doc_CN[[j]] <-doc_CN[[j]][!(doc_CN[[j]] %in% stw)]
    # scores for each of the weibo posts
    scores[j] <-sum(doc_CN[[j]] %in% pos)-sum(doc_CN[[j]] %in% neg)
    size[j]=length(doc_CN[[j]])
    
    # number of positive words in each row
    score.pos[j]<-sum(doc_CN[[j]] %in% pos)
    # number of negative words in each row
    score.neg[j]<-sum(doc_CN[[j]] %in% neg)
    # number of rows having positive score makes up the net score
    net_score[j] <- ((score.pos[j]-score.neg[j])>0)# pos
    net_score1[j] <- ((score.pos[j]-score.neg[j])<0)# neg
    # total number of instances in the corpus
    length<-length(score.pos-score.neg)
  } 
total_pos_score <- sum(net_score)/length
total_neg_score <- sum(net_score1)/length
mean <- mean(scores/size)
return(list(mean,total_pos_score,total_neg_score))
}


cat("corpus mean:", mean,"\n")
cat("total_pos_score",total_pos_score,"\n")
cat("total_neg_score",total_neg_score,"\n")


date17 <- date_calc("2012-08-01")
date18 <- date_calc("2012-08-02")
date19 <- date_calc("2012-08-03")
date20 <- date_calc("2012-08-04")
date21 <- date_calc("2012-08-05")
date22 <- date_calc("2012-08-06")
date23 <- date_calc("2012-08-07")
date24 <- date_calc("2012-08-08")
date25 <- date_calc("2012-08-09")
date26 <- date_calc("2012-08-10")
date27 <- date_calc("2012-08-11")
date28 <- date_calc("2012-08-12")
date29 <- date_calc("2012-08-13")
date30 <- date_calc("2012-08-14")
date31 <- date_calc("2012-08-15")

# date_calc("2012-07-20")
corpus_mean<-c(-0.01623619,-0.01311224, -0.004474429, -0.01838253)
pos_score <- c(0.2890625,0.2578125,0.3808801,0.25)
neg_score<-c(0.5078125,0.453125 ,0.4324734 ,0.5546875)

# plot(corpus_mean, type='b', xaxt="n",xlab="Date",ylab="Sentiment Score",
#      main="Average Sentiment Score per Day")
# axis(1, at = seq(1,4,1),
#      labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-07-19"), "days"))
# 
# plot(neg_score, type='b', xaxt="n",xlab="Date",ylab="Sentiment Score",
#      main="Negative Sentiment Score per Day")
# axis(1, at = seq(1,4,1),
#      labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-07-19"), "days"))

a <- rbind( 

            date_calc("2012-08-01"),
           date_calc("2012-08-02"),
            date_calc("2012-08-03"),
           date_calc("2012-08-04"),
            date_calc("2012-08-05"),
            date_calc("2012-08-06"),
            date_calc("2012-08-07"),
            date_calc("2012-08-08"),
            date_calc("2012-08-09"),
            date_calc("2012-08-10"),
           date_calc("2012-08-11"),
            date_calc("2012-08-12"),
            date_calc("2012-08-13"),
            date_calc("2012-08-14"),
            date_calc("2012-08-15")
)
a1 <-rbind(
          date_calc("2012-08-16"),
          date_calc("2012-08-17"),
          date_calc("2012-08-18"),
          date_calc("2012-08-19"),
          date_calc("2012-08-20"),
          date_calc("2012-08-21"),
          date_calc("2012-08-22"),
          date_calc("2012-08-23"),
          date_calc("2012-08-24"),
          date_calc("2012-08-25"),
          date_calc("2012-08-26"),
          date_calc("2012-08-27"),
          date_calc("2012-08-28"),
          date_calc("2012-08-29"),
          date_calc("2012-08-30"),
          date_calc("2012-08-31"),
          date_calc("2012-09-01"),
          date_calc("2012-09-02"),
          date_calc("2012-09-03"),
          date_calc("2012-09-04"),
          date_calc("2012-09-05"),
          date_calc("2012-09-06"),
          date_calc("2012-09-07"),
          date_calc("2012-09-08"),
          date_calc("2012-09-09"),
          date_calc("2012-09-10"),
          date_calc("2012-09-11"),
          date_calc("2012-09-12"),
          date_calc("2012-09-13")
  )

a2 <- rbind(
  date_calc("2012-09-14"),
  date_calc("2012-09-15"),
  date_calc("2012-09-16"),
  date_calc("2012-09-17"),
  date_calc("2012-09-18"),
  date_calc("2012-09-19"),
  date_calc("2012-09-20"),
  date_calc("2012-09-21"),
  date_calc("2012-09-22"),
  date_calc("2012-09-23"),
  date_calc("2012-09-24"),
  date_calc("2012-09-25"),
  date_calc("2012-09-26"),
  date_calc("2012-09-27"),
  date_calc("2012-09-28"),
  date_calc("2012-09-29"),
  date_calc("2012-09-30"),
  date_calc("2012-10-01"),
  date_calc("2012-10-02"),
  date_calc("2012-10-03"),
  date_calc("2012-10-04"),
  date_calc("2012-10-05"),
  date_calc("2012-10-06"),
  date_calc("2012-10-07"),
  date_calc("2012-10-08"),
  date_calc("2012-10-09")
)

a3 <- rbind(
  date_calc("2012-10-10"),
  date_calc("2012-10-11"),
  date_calc("2012-10-12"),
  date_calc("2012-10-13"),
  date_calc("2012-10-14"),
  date_calc("2012-10-15"),
  date_calc("2012-10-16"),
  date_calc("2012-10-17"),
  date_calc("2012-10-18"),
  date_calc("2012-10-19"),
  date_calc("2012-10-20"),
  date_calc("2012-10-21"),
  date_calc("2012-10-22"),
  date_calc("2012-10-23"),
  date_calc("2012-10-24"),
  date_calc("2012-10-25"),
  date_calc("2012-10-26"),
  date_calc("2012-10-27"),
  date_calc("2012-10-28")
  
  )

  )
corpus_mean<-c(date1[[1]],date2[[1]],date3[[1]],date4[[1]],date5[[1]],)
pos_score<-c(date1[[2]],date2[[2]],date3[[2]],date4[[2]])
neg_score<-c(date1[[3]],date2[[3]],date3[[3]],date4[[3]])

corpus_mean <- unlist(c(a[,1],a1[,1],a2[,1],a3[,1]))
pos_score <- unlist(c(a[,2],a1[,2],a2[,2],a3[,2]))
neg_score <- unlist(c(a[,3],a1[,3],a2[,3],a3[,3]))

#*********************************************
plot(corpus_mean,type="b",xaxt='n',ylab="mean",main="Mean")
axis(1, at = seq(1,105,1),labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-10-28"), "days"))

plot(pos_score,type="b",xaxt='n',ylab="score",main="Positive Sentiment Score")
axis(1, at = seq(1,105,1),labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-10-28"), "days"))

plot(neg_score,type="b",xaxt='n',ylab="score",main="Negative Sentiment Score")
axis(1, at = seq(1,105,1),labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-10-28"), "days"))
#*********************************************

date_07_16 <- twt1[which(twt1$date=="2012-07-16"),]
date_07_17 <- twt1[which(twt1$date=="2012-07-17"),]
date_07_18 <- twt1[which(twt1$date=="2012-07-18"),]
date_07_21 <- twt1[which(twt1$date=="2012-07-21"),]
# word segmentation
doc_CN=list()
for(j in 1:length(date_07_16$text)){
  doc_CN[[j]]=c(segmentCN(as.character(date_07_16$text[j])))
}


#*******************************************
#
#     mood 7 categories
#
#*******************************************
mood <- read.csv("mood.csv",sep=";",header=T, encoding="utf-8")
head(mood)
tail(mood)
dim(mood)
mood_pos<-mood[which(mood[,4]==1),]
mood_pos_strength<-
mood_neg<-mood[which(mood[,4]==2),]
mood_neu<-mood[which(mood[,4]==0),]

mood[1,3]
mood<-as.matrix(mood)

happy1 <- mood[which(mood[,2]=="PA"),]
happy2 <- mood[which(mood[,2]=="PE"),]
good1 <- mood[which(mood[,2]=="PD"),]
good2 <- mood[which(mood[,2]=="PG"),]
good3 <- mood[which(mood[,2]=="PK"),]
good4<- mood[which(mood[,2]=="PH"),]
good5 <- mood[which(mood[,2]=="PB"),]
fear1 <- mood[which(mood[,2]=="NI"),]
fear2 <- mood[which(mood[,2]=="NG"),]
fear3 <- mood[which(mood[,2]=="NC"),]
sad1 <- mood[which(mood[,2]=="NB"),]
sad2 <- mood[which(mood[,2]=="PF"),]
sad3 <- mood[which(mood[,2]=="NJ"),]
sad4 <- mood[which(mood[,2]== "NH"),]
hate1 <- mood[which(mood[,2]=="NE"),]
hate2 <- mood[which(mood[,2]=="NN"),]
hate3 <- mood[which(mood[,2]=="NL"),]
hate4 <- mood[which(mood[,2]=="NK"),]
hate5 <- mood[which(mood[,2]=="ND"),]
surprise <- mood[which(mood[,2]=="PC"),]#
angery <- mood[which(is.na(mood[,2])),]#

# dim(happy)[1]+dim(fear)[1]+dim(sad)[1]+dim(hate)[1]+dim(angery)[1]+dim(good)[1]+dim(surprise)[1]
sad <- rbind(sad1,sad2,sad3,sad4)
fear <- rbind(fear1,fear2,fear3)
hate <- rbind(hate1,hate2,hate3,hate4,hate5)
good <- rbind(good1,good2,good3,good4,good5)
happy <- rbind(happy1,happy2)
mood1<-rbind(happy,sad,good,hate,fear,angery,surprise)

hp <- as.character(happy[,1])
hp_strength <-as.numeric(happy[,3])
gd <- as.character(good[,1])
gd_strength <-as.numeric(good[,3])
ag <- as.character(angery[,1])
ag_strength <-as.numeric(angery[,3])
sd <- as.character(sad[,1])
sd_strength <-as.numeric(sad[,3])
fr <- as.character(fear[,1])
fr_strength <-as.numeric(fear[,3])
ht <- as.character(hate[,1])
ht_strength <-as.numeric(hate[,3])
sp <- as.character(surprise[,1])
sp_strength <-as.numeric(surprise[,3])

day="2012-07-16"
label=hp
mood_calc <- function(day,label){
  doc <- twt1[which(twt1$date==day),]
  doc_CN=list()
  for(j in 1:length(doc$text)){
    doc_CN[[j]]=c(segmentCN(as.character(doc$text[j])))
  }  
  
  # match sentiment scale and set weights
  happy_msg=rep(0,times=length(doc_CN))
  net_happy=happy_msg
  net_score=happy_msg
  happy_size=happy_msg
  
  for (j in 1:length(doc_CN)){
    
    # remove stopwords
    doc_CN[[j]] <-doc_CN[[j]][!(doc_CN[[j]] %in% stw)]
    
    # scores for each of the weibo posts
    happy_msg[j] <-sum(doc_CN[[j]] %in% label)
    happy_size[j]=length(doc_CN[[j]])
    
    # number of happy words in each row
    net_happy[j]<-sum(doc_CN[[j]] %in% label)

    net_score[j] <- (net_happy[j]>0)# happy msg
    # total number of instances in the corpus
    length<-length(net_score)
  } 
  h_score <- sum(net_score)/length
  mean <- mean(happy_msg/happy_size)
  return(list(mean,h_score))
}
# happy  
h_d1 <- mood_calc("2012-07-16",hp)
h_d2 <- mood_calc("2012-07-17",hp)
h_d3 <- mood_calc("2012-07-18",hp)
h_d4 <- mood_calc("2012-07-19",hp)

h_mean<-c(h_d1[[1]],h_d2[[1]],h_d3[[1]],h_d4[[1]])
h_score<-c(h_d1[[2]],h_d2[[2]],h_d3[[2]],h_d4[[2]])

# good
g_d1 <- mood_calc("2012-07-16",gd)
g_d2 <- mood_calc("2012-07-17",gd)
g_d3 <- mood_calc("2012-07-18",gd)
g_d4 <- mood_calc("2012-07-19",gd)

g_mean<-c(g_d1[[1]],g_d2[[1]],g_d3[[1]],g_d4[[1]])
g_score<-c(g_d1[[2]],g_d2[[2]],g_d3[[2]],g_d4[[2]])

# angery
a_d1 <- mood_calc("2012-07-16",ag)
a_d2 <- mood_calc("2012-07-17",ag)
a_d3 <- mood_calc("2012-07-18",ag)
a_d4 <- mood_calc("2012-07-19",ag)

a_mean<-c(a_d1[[1]],a_d2[[1]],a_d3[[1]],a_d4[[1]])
a_score<-c(a_d1[[2]],a_d2[[2]],a_d3[[2]],a_d4[[2]])

# sad
s_d1 <- mood_calc("2012-07-16",sd)
s_d2 <- mood_calc("2012-07-17",sd)
s_d3 <- mood_calc("2012-07-18",sd)
s_d4 <- mood_calc("2012-07-19",sd)

s_mean<-c(s_d1[[1]],s_d2[[1]],s_d3[[1]],s_d4[[1]])
s_score<-c(s_d1[[2]],s_d2[[2]],s_d3[[2]],s_d4[[2]])

# fear
f_d1 <- mood_calc("2012-07-16",fr)
f_d2 <- mood_calc("2012-07-17",fr)
f_d3 <- mood_calc("2012-07-18",fr)
f_d4 <- mood_calc("2012-07-19",fr)

f_mean<-c(f_d1[[1]],f_d2[[1]],f_d3[[1]],f_d4[[1]])
f_score<-c(f_d1[[2]],f_d2[[2]],f_d3[[2]],f_d4[[2]])

# hate
ht_d1 <- mood_calc("2012-07-16",ht)
ht_d2 <- mood_calc("2012-07-17",ht)
ht_d3 <- mood_calc("2012-07-18",ht)
ht_d4 <- mood_calc("2012-07-19",ht)

ht_mean<-c(ht_d1[[1]],ht_d2[[1]],ht_d3[[1]],ht_d4[[1]])
ht_score<-c(ht_d1[[2]],ht_d2[[2]],ht_d3[[2]],ht_d4[[2]])

# surprise
sp_d1 <- mood_calc("2012-07-16",sp)
sp_d2 <- mood_calc("2012-07-17",sp)
sp_d3 <- mood_calc("2012-07-18",sp)
sp_d4 <- mood_calc("2012-07-19",sp)

sp_mean<-c(sp_d1[[1]],sp_d2[[1]],sp_d3[[1]],sp_d4[[1]])
sp_score<-c(sp_d1[[2]],sp_d2[[2]],sp_d3[[2]],sp_d4[[2]])

plot(sp_mean,type="b",main="Surprised Sentiment Mean")
plot(sp_score,type="b",main="Surprised Sentiment Score")

par(mfrow=c(4,2))
plot(h_score,type="b",xaxt="n", xlab="",ylab="score",main="Happy")
plot(g_score,type="b",xaxt="n",xlab="",ylab="score",main="Good")
plot(ht_score,type="b",xaxt="n",xlab="",ylab="score",main="Hate")
plot(a_score,type="b",xaxt="n",xlab="",ylab="score",main="Angery")
plot(f_score,type="b",xaxt="n",xlab="",ylab="score",main="Fear")
plot(s_score,type="b",xaxt="n",xlab="",ylab="score",main="Sad")
plot(sp_score,type="b",xaxt="n",xlab="",ylab="score",main="Surprised")

par(new=T)
axis(1, at = seq(1,4,1),labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-07-19"), "days"))

par(mfrow=c(4,2))
plot(h_mean,type="b",xaxt="n", xlab="",ylab="mean",main="Happy")
plot(g_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Good")
plot(ht_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Hate")
plot(a_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Angery")
plot(f_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Fear")
plot(s_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Sad")
plot(sp_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Surprised")
