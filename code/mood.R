source(senti_analysis.R)


 day="2012-08-01"
 label=hp
# label_strength=ht_strength
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
    
    # number of words in each row
    happy_size[j]=length(doc_CN[[j]])
    
    # number of happy words in each row
    net_happy[j]<-sum(doc_CN[[j]] %in% label)
    
    net_score[j] <- (net_happy[j]>0)# happy msg

  } 
  # total number of instances in the corpus
  length<-length(net_score)
  h_score <- sum(net_score)/length
  mean <- mean(happy_msg/happy_size)
  return(list(mean,h_score))
}

#**********************************
#       happy
#**********************************
h <- rbind( 
  mood_calc("2012-08-01",hp),
  mood_calc("2012-08-02",hp),
  mood_calc("2012-08-03",hp),
  mood_calc("2012-08-04",hp),
  mood_calc("2012-08-05",hp),
  mood_calc("2012-08-06",hp),
  mood_calc("2012-08-07",hp),
  mood_calc("2012-08-08",hp),
  mood_calc("2012-08-09",hp),
  mood_calc("2012-08-10",hp),
  mood_calc("2012-08-11",hp),
  mood_calc("2012-08-12",hp),
  mood_calc("2012-08-13",hp),
  mood_calc("2012-08-14",hp),
  mood_calc("2012-08-15",hp)
)
h1 <-rbind(
  mood_calc("2012-08-16",hp),
  mood_calc("2012-08-17",hp),
  mood_calc("2012-08-18",hp),
  mood_calc("2012-08-19",hp),
  mood_calc("2012-08-20",hp),
  mood_calc("2012-08-21",hp),
  mood_calc("2012-08-22",hp),
  mood_calc("2012-08-23",hp),
  mood_calc("2012-08-24",hp),
  mood_calc("2012-08-25",hp),
  mood_calc("2012-08-26",hp),
  mood_calc("2012-08-27",hp),
  mood_calc("2012-08-28",hp),
  mood_calc("2012-08-29",hp),
  mood_calc("2012-08-30",hp),
  mood_calc("2012-08-31",hp),
  mood_calc("2012-09-01",hp),
  mood_calc("2012-09-02",hp),
  mood_calc("2012-09-03",hp),
  mood_calc("2012-09-04",hp),
  mood_calc("2012-09-05",hp),
  mood_calc("2012-09-06",hp),
  mood_calc("2012-09-07",hp),
  mood_calc("2012-09-08",hp),
  mood_calc("2012-09-09",hp),
  mood_calc("2012-09-10",hp),
  mood_calc("2012-09-11",hp),
  mood_calc("2012-09-12",hp),
  mood_calc("2012-09-13",hp)
)

h2 <- rbind(
  mood_calc("2012-09-14",hp),
  mood_calc("2012-09-15",hp),
  mood_calc("2012-09-16",hp),
  mood_calc("2012-09-17",hp),
  mood_calc("2012-09-18",hp),
  mood_calc("2012-09-19",hp),
  mood_calc("2012-09-20",hp),
  mood_calc("2012-09-21",hp),
  mood_calc("2012-09-22",hp),
  mood_calc("2012-09-23",hp),
  mood_calc("2012-09-24",hp),
  mood_calc("2012-09-25",hp),
  mood_calc("2012-09-26",hp),
  mood_calc("2012-09-27",hp),
  mood_calc("2012-09-28",hp),
  mood_calc("2012-09-29",hp),
  mood_calc("2012-09-30",hp),
  mood_calc("2012-10-01",hp),
  mood_calc("2012-10-02",hp),
  mood_calc("2012-10-03",hp),
  mood_calc("2012-10-04",hp),
  mood_calc("2012-10-05",hp),
  mood_calc("2012-10-06",hp),
  mood_calc("2012-10-07",hp),
  mood_calc("2012-10-08",hp),
  mood_calc("2012-10-09",hp)
)

h3 <- rbind(
  mood_calc("2012-10-10",hp),
  mood_calc("2012-10-11",hp),
  mood_calc("2012-10-12",hp),
  mood_calc("2012-10-13",hp),
  mood_calc("2012-10-14",hp),
  mood_calc("2012-10-15",hp),
  mood_calc("2012-10-16",hp),
  mood_calc("2012-10-17",hp),
  mood_calc("2012-10-18",hp),
  mood_calc("2012-10-19",hp),
  mood_calc("2012-10-20",hp),
  mood_calc("2012-10-21",hp),
  mood_calc("2012-10-22",hp),
  mood_calc("2012-10-23",hp),
  mood_calc("2012-10-24",hp),
  mood_calc("2012-10-25",hp),
  mood_calc("2012-10-26",hp),
  mood_calc("2012-10-27",hp),
  mood_calc("2012-10-28",hp)
)

hp_mean <- unlist(c(h[,1],h1[,1],h2[,1],h3[,1]))
hp_score <- unlist(c(h[,2],h1[,2],h2[,2],h3[,2]))

plot(hp_mean,type="b",ylab="mean",main="Happy")
# plot(hp_score,type="b", ylab="score",main="Happy ")

#**********************************
#       good
#**********************************
g <- rbind( 
 
  mood_calc("2012-08-01",gd),
  mood_calc("2012-08-02",gd),
  mood_calc("2012-08-03",gd),
  mood_calc("2012-08-04",gd),
  mood_calc("2012-08-05",gd),
  mood_calc("2012-08-06",gd),
  mood_calc("2012-08-07",gd),
  mood_calc("2012-08-08",gd),
  mood_calc("2012-08-09",gd),
  mood_calc("2012-08-10",gd),
  mood_calc("2012-08-11",gd),
  mood_calc("2012-08-12",gd),
  mood_calc("2012-08-13",gd),
  mood_calc("2012-08-14",gd),
  mood_calc("2012-08-15",gd)
)
g1 <-rbind(
  mood_calc("2012-08-16",gd),
  mood_calc("2012-08-17",gd),
  mood_calc("2012-08-18",gd),
  mood_calc("2012-08-19",gd),
  mood_calc("2012-08-20",gd),
  mood_calc("2012-08-21",gd),
  mood_calc("2012-08-22",gd),
  mood_calc("2012-08-23",gd),
  mood_calc("2012-08-24",gd),
  mood_calc("2012-08-25",gd),
  mood_calc("2012-08-26",gd),
  mood_calc("2012-08-27",gd),
  mood_calc("2012-08-28",gd),
  mood_calc("2012-08-29",gd),
  mood_calc("2012-08-30",gd),
  mood_calc("2012-08-31",gd),
  mood_calc("2012-09-01",gd),
  mood_calc("2012-09-02",gd),
  mood_calc("2012-09-03",gd),
  mood_calc("2012-09-04",gd),
  mood_calc("2012-09-05",gd),
  mood_calc("2012-09-06",gd),
  mood_calc("2012-09-07",gd),
  mood_calc("2012-09-08",gd),
  mood_calc("2012-09-09",gd),
  mood_calc("2012-09-10",gd),
  mood_calc("2012-09-11",gd),
  mood_calc("2012-09-12",gd),
  mood_calc("2012-09-13",gd)
)

g2 <- rbind(
  mood_calc("2012-09-14",gd),
  mood_calc("2012-09-15",gd),
  mood_calc("2012-09-16",gd),
  mood_calc("2012-09-17",gd),
  mood_calc("2012-09-18",gd),
  mood_calc("2012-09-19",gd),
  mood_calc("2012-09-20",gd),
  mood_calc("2012-09-21",gd),
  mood_calc("2012-09-22",gd),
  mood_calc("2012-09-23",gd),
  mood_calc("2012-09-24",gd),
  mood_calc("2012-09-25",gd),
  mood_calc("2012-09-26",gd),
  mood_calc("2012-09-27",gd),
  mood_calc("2012-09-28",gd),
  mood_calc("2012-09-29",gd),
  mood_calc("2012-09-30",gd),
  mood_calc("2012-10-01",gd),
  mood_calc("2012-10-02",gd),
  mood_calc("2012-10-03",gd),
  mood_calc("2012-10-04",gd),
  mood_calc("2012-10-05",gd),
  mood_calc("2012-10-06",gd),
  mood_calc("2012-10-07",gd),
  mood_calc("2012-10-08",gd),
  mood_calc("2012-10-09",gd)
)

g3 <- rbind(
  mood_calc("2012-10-10",gd),
  mood_calc("2012-10-11",gd),
  mood_calc("2012-10-12",gd),
  mood_calc("2012-10-13",gd),
  mood_calc("2012-10-14",gd),
  mood_calc("2012-10-15",gd),
  mood_calc("2012-10-16",gd),
  mood_calc("2012-10-17",gd),
  mood_calc("2012-10-18",gd),
  mood_calc("2012-10-19",gd),
  mood_calc("2012-10-20",gd),
  mood_calc("2012-10-21",gd),
  mood_calc("2012-10-22",gd),
  mood_calc("2012-10-23",gd),
  mood_calc("2012-10-24",gd),
  mood_calc("2012-10-25",gd),
  mood_calc("2012-10-26",gd),
  mood_calc("2012-10-27",gd),
  mood_calc("2012-10-28",gd)
)

gd_mean <- unlist(c(g[,1],g1[,1],g2[,1],g3[,1]))
gd_score <- unlist(c(g[,2],g1[,2],g2[,2],g3[,2]))

plot(gd_mean,type="b",main="Good")
# plot(gd_score,type="b",main="Good")

#**********************************
#       angry
#**********************************
agy <- rbind( 
 
  mood_calc("2012-08-01",ag),
  mood_calc("2012-08-02",ag),
  mood_calc("2012-08-03",ag),
  mood_calc("2012-08-04",ag),
  mood_calc("2012-08-05",ag),
  mood_calc("2012-08-06",ag),
  mood_calc("2012-08-07",ag),
  mood_calc("2012-08-08",ag),
  mood_calc("2012-08-09",ag),
  mood_calc("2012-08-10",ag),
  mood_calc("2012-08-11",ag),
  mood_calc("2012-08-12",ag),
  mood_calc("2012-08-13",ag),
  mood_calc("2012-08-14",ag),
  mood_calc("2012-08-15",ag)
)
agy1 <-rbind(
  mood_calc("2012-08-16",ag),
  mood_calc("2012-08-17",ag),
  mood_calc("2012-08-18",ag),
  mood_calc("2012-08-19",ag),
  mood_calc("2012-08-20",ag),
  mood_calc("2012-08-21",ag),
  mood_calc("2012-08-22",ag),
  mood_calc("2012-08-23",ag),
  mood_calc("2012-08-24",ag),
  mood_calc("2012-08-25",ag),
  mood_calc("2012-08-26",ag),
  mood_calc("2012-08-27",ag),
  mood_calc("2012-08-28",ag),
  mood_calc("2012-08-29",ag),
  mood_calc("2012-08-30",ag),
  mood_calc("2012-08-31",ag),
  mood_calc("2012-09-01",ag),
  mood_calc("2012-09-02",ag),
  mood_calc("2012-09-03",ag),
  mood_calc("2012-09-04",ag),
  mood_calc("2012-09-05",ag),
  mood_calc("2012-09-06",ag),
  mood_calc("2012-09-07",ag),
  mood_calc("2012-09-08",ag),
  mood_calc("2012-09-09",ag),
  mood_calc("2012-09-10",ag),
  mood_calc("2012-09-11",ag),
  mood_calc("2012-09-12",ag),
  mood_calc("2012-09-13",ag)
)

agy2 <- rbind(
  mood_calc("2012-09-14",ag),
  mood_calc("2012-09-15",ag),
  mood_calc("2012-09-16",ag),
  mood_calc("2012-09-17",ag),
  mood_calc("2012-09-18",ag),
  mood_calc("2012-09-19",ag),
  mood_calc("2012-09-20",ag),
  mood_calc("2012-09-21",ag),
  mood_calc("2012-09-22",ag),
  mood_calc("2012-09-23",ag),
  mood_calc("2012-09-24",ag),
  mood_calc("2012-09-25",ag),
  mood_calc("2012-09-26",ag),
  mood_calc("2012-09-27",ag),
  mood_calc("2012-09-28",ag),
  mood_calc("2012-09-29",ag),
  mood_calc("2012-09-30",ag),
  mood_calc("2012-10-01",ag),
  mood_calc("2012-10-02",ag),
  mood_calc("2012-10-03",ag),
  mood_calc("2012-10-04",ag),
  mood_calc("2012-10-05",ag),
  mood_calc("2012-10-06",ag),
  mood_calc("2012-10-07",ag),
  mood_calc("2012-10-08",ag),
  mood_calc("2012-10-09",ag)
)

agy3 <- rbind(
  mood_calc("2012-10-10",ag),
  mood_calc("2012-10-11",ag),
  mood_calc("2012-10-12",ag),
  mood_calc("2012-10-13",ag),
  mood_calc("2012-10-14",ag),
  mood_calc("2012-10-15",ag),
  mood_calc("2012-10-16",ag),
  mood_calc("2012-10-17",ag),
  mood_calc("2012-10-18",ag),
  mood_calc("2012-10-19",ag),
  mood_calc("2012-10-20",ag),
  mood_calc("2012-10-21",ag),
  mood_calc("2012-10-22",ag),
  mood_calc("2012-10-23",ag),
  mood_calc("2012-10-24",ag),
  mood_calc("2012-10-25",ag),
  mood_calc("2012-10-26",ag),
  mood_calc("2012-10-27",ag),
  mood_calc("2012-10-28",ag)
)

ag_mean <- unlist(c(agy[,1],agy1[,1],agy2[,1],agy3[,1]))
ag_score <- unlist(c(agy[,2],agy1[,2],agy2[,2],agy3[,2]))

plot(ag_mean,type="b",ylab="mean",main="Angry")
# plot(ag_score,type="b",ylab="score",main="Angry")

#**********************************
#       sad
#**********************************
s <- rbind( 

  mood_calc("2012-08-01",sd),
  mood_calc("2012-08-02",sd),
  mood_calc("2012-08-03",sd),
  mood_calc("2012-08-04",sd),
  mood_calc("2012-08-05",sd),
  mood_calc("2012-08-06",sd),
  mood_calc("2012-08-07",sd),
  mood_calc("2012-08-08",sd),
  mood_calc("2012-08-09",sd),
  mood_calc("2012-08-10",sd),
  mood_calc("2012-08-11",sd),
  mood_calc("2012-08-12",sd),
  mood_calc("2012-08-13",sd),
  mood_calc("2012-08-14",sd),
  mood_calc("2012-08-15",sd)
)
s1 <-rbind(
  mood_calc("2012-08-16",sd),
  mood_calc("2012-08-17",sd),
  mood_calc("2012-08-18",sd),
  mood_calc("2012-08-19",sd),
  mood_calc("2012-08-20",sd),
  mood_calc("2012-08-21",sd),
  mood_calc("2012-08-22",sd),
  mood_calc("2012-08-23",sd),
  mood_calc("2012-08-24",sd),
  mood_calc("2012-08-25",sd),
  mood_calc("2012-08-26",sd),
  mood_calc("2012-08-27",sd),
  mood_calc("2012-08-28",sd),
  mood_calc("2012-08-29",sd),
  mood_calc("2012-08-30",sd),
  mood_calc("2012-08-31",sd),
  mood_calc("2012-09-01",sd),
  mood_calc("2012-09-02",sd),
  mood_calc("2012-09-03",sd),
  mood_calc("2012-09-04",sd),
  mood_calc("2012-09-05",sd),
  mood_calc("2012-09-06",sd),
  mood_calc("2012-09-07",sd),
  mood_calc("2012-09-08",sd),
  mood_calc("2012-09-09",sd),
  mood_calc("2012-09-10",sd),
  mood_calc("2012-09-11",sd),
  mood_calc("2012-09-12",sd),
  mood_calc("2012-09-13",sd)
)

s2 <- rbind(
  mood_calc("2012-09-14",sd),
  mood_calc("2012-09-15",sd),
  mood_calc("2012-09-16",sd),
  mood_calc("2012-09-17",sd),
  mood_calc("2012-09-18",sd),
  mood_calc("2012-09-19",sd),
  mood_calc("2012-09-20",sd),
  mood_calc("2012-09-21",sd),
  mood_calc("2012-09-22",sd),
  mood_calc("2012-09-23",sd),
  mood_calc("2012-09-24",sd),
  mood_calc("2012-09-25",sd),
  mood_calc("2012-09-26",sd),
  mood_calc("2012-09-27",sd),
  mood_calc("2012-09-28",sd),
  mood_calc("2012-09-29",sd),
  mood_calc("2012-09-30",sd),
  mood_calc("2012-10-01",sd),
  mood_calc("2012-10-02",sd),
  mood_calc("2012-10-03",sd),
  mood_calc("2012-10-04",sd),
  mood_calc("2012-10-05",sd),
  mood_calc("2012-10-06",sd),
  mood_calc("2012-10-07",sd),
  mood_calc("2012-10-08",sd),
  mood_calc("2012-10-09",sd)
)

s3 <- rbind(
  mood_calc("2012-10-10",sd),
  mood_calc("2012-10-11",sd),
  mood_calc("2012-10-12",sd),
  mood_calc("2012-10-13",sd),
  mood_calc("2012-10-14",sd),
  mood_calc("2012-10-15",sd),
  mood_calc("2012-10-16",sd),
  mood_calc("2012-10-17",sd),
  mood_calc("2012-10-18",sd),
  mood_calc("2012-10-19",sd),
  mood_calc("2012-10-20",sd),
  mood_calc("2012-10-21",sd),
  mood_calc("2012-10-22",sd),
  mood_calc("2012-10-23",sd),
  mood_calc("2012-10-24",sd),
  mood_calc("2012-10-25",sd),
  mood_calc("2012-10-26",sd),
  mood_calc("2012-10-27",sd),
  mood_calc("2012-10-28",sd)
)

sd_mean <- unlist(c(s[,1],s1[,1],s2[,1],s3[,1]))
sd_score <- unlist(c(s[,2],s1[,2],s2[,2],s3[,2]))

plot(sd_mean,type="b",ylab="mean",main="Sad")
plot(sd_score,type="b",ylab="score",main="Sad")

#**********************************
#       fear
#**********************************
f <- rbind( 
  
  mood_calc("2012-08-01",fr),
  mood_calc("2012-08-02",fr),
  mood_calc("2012-08-03",fr),
  mood_calc("2012-08-04",fr),
  mood_calc("2012-08-05",fr),
  mood_calc("2012-08-06",fr),
  mood_calc("2012-08-07",fr),
  mood_calc("2012-08-08",fr),
  mood_calc("2012-08-09",fr),
  mood_calc("2012-08-10",fr),
  mood_calc("2012-08-11",fr),
  mood_calc("2012-08-12",fr),
  mood_calc("2012-08-13",fr),
  mood_calc("2012-08-14",fr),
  mood_calc("2012-08-15",fr)
)
f1 <-rbind(
  mood_calc("2012-08-16",fr),
  mood_calc("2012-08-17",fr),
  mood_calc("2012-08-18",fr),
  mood_calc("2012-08-19",fr),
  mood_calc("2012-08-20",fr),
  mood_calc("2012-08-21",fr),
  mood_calc("2012-08-22",fr),
  mood_calc("2012-08-23",fr),
  mood_calc("2012-08-24",fr),
  mood_calc("2012-08-25",fr),
  mood_calc("2012-08-26",fr),
  mood_calc("2012-08-27",fr),
  mood_calc("2012-08-28",fr),
  mood_calc("2012-08-29",fr),
  mood_calc("2012-08-30",fr),
  mood_calc("2012-08-31",fr),
  mood_calc("2012-09-01",fr),
  mood_calc("2012-09-02",fr),
  mood_calc("2012-09-03",fr),
  mood_calc("2012-09-04",fr),
  mood_calc("2012-09-05",fr),
  mood_calc("2012-09-06",fr),
  mood_calc("2012-09-07",fr),
  mood_calc("2012-09-08",fr),
  mood_calc("2012-09-09",fr),
  mood_calc("2012-09-10",fr),
  mood_calc("2012-09-11",fr),
  mood_calc("2012-09-12",fr),
  mood_calc("2012-09-13",fr)
)

f2 <- rbind(
  mood_calc("2012-09-14",fr),
  mood_calc("2012-09-15",fr),
  mood_calc("2012-09-16",fr),
  mood_calc("2012-09-17",fr),
  mood_calc("2012-09-18",fr),
  mood_calc("2012-09-19",fr),
  mood_calc("2012-09-20",fr),
  mood_calc("2012-09-21",fr),
  mood_calc("2012-09-22",fr),
  mood_calc("2012-09-23",fr),
  mood_calc("2012-09-24",fr),
  mood_calc("2012-09-25",fr),
  mood_calc("2012-09-26",fr),
  mood_calc("2012-09-27",fr),
  mood_calc("2012-09-28",fr),
  mood_calc("2012-09-29",fr),
  mood_calc("2012-09-30",fr),
  mood_calc("2012-10-01",fr),
  mood_calc("2012-10-02",fr),
  mood_calc("2012-10-03",fr),
  mood_calc("2012-10-04",fr),
  mood_calc("2012-10-05",fr),
  mood_calc("2012-10-06",fr),
  mood_calc("2012-10-07",fr),
  mood_calc("2012-10-08",fr),
  mood_calc("2012-10-09",fr)
)

f3 <- rbind(
  mood_calc("2012-10-10",fr),
  mood_calc("2012-10-11",fr),
  mood_calc("2012-10-12",fr),
  mood_calc("2012-10-13",fr),
  mood_calc("2012-10-14",fr),
  mood_calc("2012-10-15",fr),
  mood_calc("2012-10-16",fr),
  mood_calc("2012-10-17",fr),
  mood_calc("2012-10-18",fr),
  mood_calc("2012-10-19",fr),
  mood_calc("2012-10-20",fr),
  mood_calc("2012-10-21",fr),
  mood_calc("2012-10-22",fr),
  mood_calc("2012-10-23",fr),
  mood_calc("2012-10-24",fr),
  mood_calc("2012-10-25",fr),
  mood_calc("2012-10-26",fr),
  mood_calc("2012-10-27",fr),
  mood_calc("2012-10-28",fr)
)

fr_mean <- unlist(c(f[,1],f1[,1],f2[,1],f3[,1]))
fr_score <- unlist(c(f[,2],f1[,2],f2[,2],f3[,2]))

plot(fr_mean,type="b",ylab="mean",main="Fear")
plot(fr_score,type="b",ylab="score",main="Fear")


#**********************************
#       hate
#**********************************
hate <- rbind( 
 
  mood_calc("2012-08-01",ht),
  mood_calc("2012-08-02",ht),
  mood_calc("2012-08-03",ht),
  mood_calc("2012-08-04",ht),
  mood_calc("2012-08-05",ht),
  mood_calc("2012-08-06",ht),
  mood_calc("2012-08-07",ht),
  mood_calc("2012-08-08",ht),
  mood_calc("2012-08-09",ht),
  mood_calc("2012-08-10",ht),
  mood_calc("2012-08-11",ht),
  mood_calc("2012-08-12",ht),
  mood_calc("2012-08-13",ht),
  mood_calc("2012-08-14",ht),
  mood_calc("2012-08-15",ht)
)
hate1 <-rbind(
  mood_calc("2012-08-16",ht),
  mood_calc("2012-08-17",ht),
  mood_calc("2012-08-18",ht),
  mood_calc("2012-08-19",ht),
  mood_calc("2012-08-20",ht),
  mood_calc("2012-08-21",ht),
  mood_calc("2012-08-22",ht),
  mood_calc("2012-08-23",ht),
  mood_calc("2012-08-24",ht),
  mood_calc("2012-08-25",ht),
  mood_calc("2012-08-26",ht),
  mood_calc("2012-08-27",ht),
  mood_calc("2012-08-28",ht),
  mood_calc("2012-08-29",ht),
  mood_calc("2012-08-30",ht),
  mood_calc("2012-08-31",ht),
  mood_calc("2012-09-01",ht),
  mood_calc("2012-09-02",ht),
  mood_calc("2012-09-03",ht),
  mood_calc("2012-09-04",ht),
  mood_calc("2012-09-05",ht),
  mood_calc("2012-09-06",ht),
  mood_calc("2012-09-07",ht),
  mood_calc("2012-09-08",ht),
  mood_calc("2012-09-09",ht),
  mood_calc("2012-09-10",ht),
  mood_calc("2012-09-11",ht),
  mood_calc("2012-09-12",ht),
  mood_calc("2012-09-13",ht)
)

hate2 <- rbind(
  mood_calc("2012-09-14",ht),
  mood_calc("2012-09-15",ht),
  mood_calc("2012-09-16",ht),
  mood_calc("2012-09-17",ht),
  mood_calc("2012-09-18",ht),
  mood_calc("2012-09-19",ht),
  mood_calc("2012-09-20",ht),
  mood_calc("2012-09-21",ht),
  mood_calc("2012-09-22",ht),
  mood_calc("2012-09-23",ht),
  mood_calc("2012-09-24",ht),
  mood_calc("2012-09-25",ht),
  mood_calc("2012-09-26",ht),
  mood_calc("2012-09-27",ht),
  mood_calc("2012-09-28",ht),
  mood_calc("2012-09-29",ht),
  mood_calc("2012-09-30",ht),
  mood_calc("2012-10-01",ht),
  mood_calc("2012-10-02",ht),
  mood_calc("2012-10-03",ht),
  mood_calc("2012-10-04",ht),
  mood_calc("2012-10-05",ht),
  mood_calc("2012-10-06",ht),
  mood_calc("2012-10-07",ht),
  mood_calc("2012-10-08",ht),
  mood_calc("2012-10-09",ht)
)

hate3 <- rbind(
  mood_calc("2012-10-10",ht),
  mood_calc("2012-10-11",ht),
  mood_calc("2012-10-12",ht),
  mood_calc("2012-10-13",ht),
  mood_calc("2012-10-14",ht),
  mood_calc("2012-10-15",ht),
  mood_calc("2012-10-16",ht),
  mood_calc("2012-10-17",ht),
  mood_calc("2012-10-18",ht),
  mood_calc("2012-10-19",ht),
  mood_calc("2012-10-20",ht),
  mood_calc("2012-10-21",ht),
  mood_calc("2012-10-22",ht),
  mood_calc("2012-10-23",ht),
  mood_calc("2012-10-24",ht),
  mood_calc("2012-10-25",ht),
  mood_calc("2012-10-26",ht),
  mood_calc("2012-10-27",ht),
  mood_calc("2012-10-28",ht)
)

ht_mean <- unlist(c(hate[,1],hate1[,1],hate2[,1],hate3[,1]))
ht_score <- unlist(c(hate[,2],hate1[,2],hate2[,2],hate3[,2]))

plot(ht_mean,type="b",ylab="mean",main="Hate")
plot(ht_score,type="b",ylab="score",main="Hate")

#**********************************
#       hate
#**********************************
spr <- rbind( 
   mood_calc("2012-08-01",sp),
  mood_calc("2012-08-02",sp),
  mood_calc("2012-08-03",sp),
  mood_calc("2012-08-04",sp),
  mood_calc("2012-08-05",sp),
  mood_calc("2012-08-06",sp),
  mood_calc("2012-08-07",sp),
  mood_calc("2012-08-08",sp),
  mood_calc("2012-08-09",sp),
  mood_calc("2012-08-10",sp),
  mood_calc("2012-08-11",sp),
  mood_calc("2012-08-12",sp),
  mood_calc("2012-08-13",sp),
  mood_calc("2012-08-14",sp),
  mood_calc("2012-08-15",sp)
)
spr1 <-rbind(
  mood_calc("2012-08-16",sp),
  mood_calc("2012-08-17",sp),
  mood_calc("2012-08-18",sp),
  mood_calc("2012-08-19",sp),
  mood_calc("2012-08-20",sp),
  mood_calc("2012-08-21",sp),
  mood_calc("2012-08-22",sp),
  mood_calc("2012-08-23",sp),
  mood_calc("2012-08-24",sp),
  mood_calc("2012-08-25",sp),
  mood_calc("2012-08-26",sp),
  mood_calc("2012-08-27",sp),
  mood_calc("2012-08-28",sp),
  mood_calc("2012-08-29",sp),
  mood_calc("2012-08-30",sp),
  mood_calc("2012-08-31",sp),
  mood_calc("2012-09-01",sp),
  mood_calc("2012-09-02",sp),
  mood_calc("2012-09-03",sp),
  mood_calc("2012-09-04",sp),
  mood_calc("2012-09-05",sp),
  mood_calc("2012-09-06",sp),
  mood_calc("2012-09-07",sp),
  mood_calc("2012-09-08",sp),
  mood_calc("2012-09-09",sp),
  mood_calc("2012-09-10",sp),
  mood_calc("2012-09-11",sp),
  mood_calc("2012-09-12",sp),
  mood_calc("2012-09-13",sp)
)

spr2 <- rbind(
  mood_calc("2012-09-14",sp),
  mood_calc("2012-09-15",sp),
  mood_calc("2012-09-16",sp),
  mood_calc("2012-09-17",sp),
  mood_calc("2012-09-18",sp),
  mood_calc("2012-09-19",sp),
  mood_calc("2012-09-20",sp),
  mood_calc("2012-09-21",sp),
  mood_calc("2012-09-22",sp),
  mood_calc("2012-09-23",sp),
  mood_calc("2012-09-24",sp),
  mood_calc("2012-09-25",sp),
  mood_calc("2012-09-26",sp),
  mood_calc("2012-09-27",sp),
  mood_calc("2012-09-28",sp),
  mood_calc("2012-09-29",sp),
  mood_calc("2012-09-30",sp),
  mood_calc("2012-10-01",sp),
  mood_calc("2012-10-02",sp),
  mood_calc("2012-10-03",sp),
  mood_calc("2012-10-04",sp),
  mood_calc("2012-10-05",sp),
  mood_calc("2012-10-06",sp),
  mood_calc("2012-10-07",sp),
  mood_calc("2012-10-08",sp),
  mood_calc("2012-10-09",sp)
)

spr3 <- rbind(
  mood_calc("2012-10-10",sp),
  mood_calc("2012-10-11",sp),
  mood_calc("2012-10-12",sp),
  mood_calc("2012-10-13",sp),
  mood_calc("2012-10-14",sp),
  mood_calc("2012-10-15",sp),
  mood_calc("2012-10-16",sp),
  mood_calc("2012-10-17",sp),
  mood_calc("2012-10-18",sp),
  mood_calc("2012-10-19",sp),
  mood_calc("2012-10-20",sp),
  mood_calc("2012-10-21",sp),
  mood_calc("2012-10-22",sp),
  mood_calc("2012-10-23",sp),
  mood_calc("2012-10-24",sp),
  mood_calc("2012-10-25",sp),
  mood_calc("2012-10-26",sp),
  mood_calc("2012-10-27",sp),
  mood_calc("2012-10-28",sp)
)

sp_mean <- unlist(c(spr[,1],spr1[,1],spr2[,1],spr3[,1]))
sp_score <- unlist(c(spr[,2],spr1[,2],spr2[,2],spr3[,2]))

plot(sp_mean,type="b",ylab="mean",main="Surprised")
plot(sp_score,type="b",ylab="score",main="Surprised")

#******************************************
#     Mood Plots
#******************************************

par(mfrow=c(1,1))
plot(hp_mean,type="b",xaxt="n", xlab="",ylab="mean",main="Happy")
plot(gd_mean ,type="b",xaxt="n",xlab="",ylab="mean",main="Good")
plot(ht_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Hate")
plot(ag_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Angery")
plot(fr_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Fear")
plot(sd_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Sad")
plot(sp_mean,type="b",xaxt="n",xlab="",ylab="mean",main="Surprised")

# lends <- c("happy","good","hate","angry","fear","sad","shocked")
matplot (seq(1,105,1), cbind (hp_score, gd_score,ht_score,ag_score,fr_score,sd_score,sp_score), 
         pch = 10,col=c(1:6,8),#col= c("black","orange","blue2","red","green3","purple","yellow1")
         ,type="o", xlab="days", 
         ylab="sentiment score",main="Mood Score",lty = c(1,1,1,1,1,1,1))
# col= c("black","red","blue3","red3","green3","purple","grey3"),
matplot (seq(1,105,1), cbind (hp_mean, gd_mean,ht_mean,ag_mean,fr_mean,sd_mean,sp_mean), col=c(1:6,8),
         pch = 10,type="o", xlab="days", 
         ylab="sentiment mean",main="Mood Mean",lty = 1:7,cex=0.9)
lends <-legend(x=87,y=0.8,col=c(1:6,8), #col= c("black","red","blue3","red3","green3","purple","grey"),
               lty = 1:7, lwd = c(1,1,1,1,1,1,1),
               c("Happy","Supportive","Hateful","Angry","Fearful","Sad","Surprised"),cex = 0.7,pch=1)

lends<-legend(x=87,y=0.10,col = 1:7, lty = 1:7,pch = "*", ncol = 4,c("Happy","Supportive","Hateful","Angry","Fearful","Sad","Surprised"))
dev.off()
# text(cbind(70, 3), lends, col= 1:7, cex = 1.5)
# text(cbind(70, 3), lends, col= 1:7, cex = 1.5)
lends<-legend(2000,9.5, c("red1","blue1","black","blue2","green3","purple","red3"),col= c("red1","blue1","black","blue2","green3","purple","red3"),lend=lends)
# ?matplot
par(mfrow=c(4,2))
plot(hp_score,type="b",xaxt="n", xlab="",ylab="score",main="Happy")
plot(gd_score,type="b",xaxt="n",xlab="",ylab="score",main="Good")
plot(ht_score,type="b",xaxt="n",xlab="",ylab="score",main="Hate")
plot(ag_score,type="b",xaxt="n",xlab="",ylab="score",main="Angery")
plot(fr_score,type="b",xaxt="n",xlab="",ylab="score",main="Fear")
plot(sd_score,type="b",xaxt="n",xlab="",ylab="score",main="Sad")
plot(sp_score,type="b",xaxt="n",xlab="",ylab="score",main="Surprised")

par(new=T)
axis(1, at = seq(1,105,1),labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-10-28"), "days"))

# mood dictionary distribution
plot(c(length(ag),length(hp),length(gd),length(ht),length(sd),length(fr),length(sp)),type="o")
#*************************************************
#         Happy + Good
#*************************************************
hpgd_mean <- (hp_mean+gd_mean)/2
hpgd_score <- (hp_score+gd_score)/2
plot(hpgd_mean,type="b",main="positive")
plot(hp_mean,type="b",main="happy")
#*************************************************
#         Hate + Angry
#*************************************************
htag_mean <- (ht_mean+ag_mean)/2
htag_score <- (ht_score+ag_score)/2
#*************************************************
#         Fear + Sad
#*************************************************
frsd_mean <- (fr_mean+sd_mean)/2
frsd_score <- (fr_score+sd_score)/2

negative_mean <- (frsd_mean+htag_mean)/2
negative_score <- (frsd_score+htag_score)/2

# plot
matplot (seq(1,105,1), cbind (hp_score,htag_score,frsd_score), 
         pch = 10,col= c("black","blue2","purple"),type="l", xlab="days", 
         ylab="sentiment score",main="Mood Score")#,lend=lends)

matplot (seq(1,105,1), cbind (hp_mean,htag_mean,frsd_mean), 
         pch = 10,col= c("black","blue2","purple"),type="l", xlab="days", 
         ylab="sentiment Mean",main="Mood Mean")#,lend=lends)


matplot (seq(1,105,1), cbind (negative_score
                        ,hpgd_score), 
         pch = 10,col= c("red3","blue2"),type="l", xlab="days", 
         ylab="sentiment mean",main="Polarity Mean")#,lend=lends)

matplot (seq(1,105,1), cbind (negative_mean
                              ,hpgd_mean), 
         pch = 10,col= c("red3","blue2"),type="l", xlab="days", 
         ylab="sentiment mean",main="Polarity Mean")#,lend=lends)

#*********************************************
pol_score <- pos_score/neg_score
matplot(seq(1,105,1), cbind(pos_score,neg_score,pol_score),pch=10,
        col= c("blue2","black","red3")
        ,type="l",xaxt='n',ylab="mean",main="Mean")
axis(1, at = seq(1,105,1),labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-10-28"), "days"))

plot(pos_score,type="b",xaxt='n',ylab="score",main="Positive Sentiment Score")
axis(1, at = seq(1,105,1),labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-10-28"), "days"))

plot(neg_score,type="b",xaxt='n',ylab="score",main="Negative Sentiment Score")
axis(1, at = seq(1,105,1),labels = seq(as.POSIXct("2012-07-16"),as.POSIXct("2012-10-28"), "days"))
#*********************************************

baidu <- read.csv("date_baidu_index.csv",sep=';',header=T, quote = "\"",  encoding='UTF-8')
plot(baidu$count,type="b",main="Baidu Index")
