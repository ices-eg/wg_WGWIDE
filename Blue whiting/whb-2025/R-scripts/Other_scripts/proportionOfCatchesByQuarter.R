ProportionByQuarter<-function(years.in.average=2000:2001,infile='a.txt') {
  #years.in.average<-2015:2017
  
  a<-read.csv(file=infile)
  b<-reshape(a,direction="long",varying=list(4:7),timevar='Q',v.names='catch')
  
  
  b$catch<-b$catch/1000
  b<-droplevels(subset(b,age>0))
  
  library(lattice)
  check<-TRUE
  
  if (check) xtabs(~cat+age+year,data=b)
  b$age<-ifelse(b$age>10,10,b$age)
  b.an<-aggregate(catch~age+year,data=b,sum)
  if (check) xtabs(catch~age+year,data=b.an)
  
  if (check) barchart(catch~factor(year),groups=age,stack=TRUE,data=b.an,col=cm.colors(11))
  
  
  b.q1<-aggregate(catch~age+year,data=b,subset=(Q==1),sum)
  colnames(b.q1)<-c("age","year","q1")
  
  b.q12<-aggregate(catch~age+year,data=b,subset=(Q<=2),sum)
  colnames(b.q12)<-c("age","year","q12")
  
  bb<-merge(merge(b.q1,b.q12),b.an)
  head(subset(bb,age==5))
  
  bb$Pq1<-bb$q1/bb$catch*100
  bb$Pq12<-bb$q12/bb$catch*100
  
  tab1<-tapply(bb$Pq1,list(bb$age,bb$year),sum)
  round(tab1,1)
  
  tab12<-tapply(bb$Pq12,list(bb$age,bb$year),sum)
  round(tab12,1)
  
  tab12<-tab12[,as.character(years.in.average)]
  tab12<-cbind(tab12,  rowMeans(tab12))
  
  colnames(tab12)<-c(head(colnames(tab12),-1),"Average")

  
  return(tab12)
  
}

