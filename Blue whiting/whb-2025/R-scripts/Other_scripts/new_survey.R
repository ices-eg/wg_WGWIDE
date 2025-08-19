library(tidyverse)

new_sur<-file.path(year.root,"Update_survey")

read.csv.fish<-function (filen)  {
  # filen<-'Catch-numbers-at-age.csv'
  
  hn<-readLines(filen)[1]
  hn<-strsplit(hn,',')[[1]]
  hn<-suppressWarnings(as.numeric(hn))
  hnn<-!is.na(hn)
  hn<-hn[hnn]
  
  a<-read.csv(filen,check.names = FALSE)
  years<-a[,1]
  a<-a[,hnn]
  a<-as.matrix(a)
  rownames(a)<-years
  # colnames(a)<-hn
  a
}

x<-file.path(new_sur,'wgwide_2019.csv')
name<-tail(strsplit(x,'/')[[1]],1)
name<-strsplit(name,'.',fixed=TRUE)[[1]][1]

a<-read.csv(file.path(new_sur,'wgwide_2019.csv'))
colnames(a)<-c('Year',paste('age',1:10,sep='_'))


surveys<-c(file.path(new_sur,'wgwide_2019.csv'),
          file.path(new_sur,'updated_baseline.csv'),
          file.path(new_sur,'bootstrap_replicates.csv'))
    
a<-lapply(surveys,function(x){
  name<-tail(strsplit(x,'/')[[1]],1)
  name<-strsplit(name,'.',fixed=TRUE)[[1]][1]
  
  a<-read.csv(x)
  colnames(a)<-c('Year',paste('age',1:10,sep='_'))
  a$Survey<-name
  return(a)
})

a<-do.call(rbind,a)


b<-reshape(a,direction='long',idvar = c('Year','Survey'),varying=list(2:11),v.names='idx',timevar='Age')

b<-as_tibble(b[order(b$Age,b$Year,b$Survey),])


tst<- b  %>% mutate(Age=paste("Age",formatC(Age,flag="0",width=2))) %>%filter(Age<='Age 08')
X11(h=7,w=13)

ggplot(data=tst, aes(x=Year, y=idx, shape=Survey,color=Survey)) +
  #geom_bar(stat="identity", position=position_dodge()) +
  geom_point(size=2)+
  facet_wrap(~Age, scales="free_y", ncol=4) + xlab("") +ylab("Survey Index")

