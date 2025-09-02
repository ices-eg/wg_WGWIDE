library(reshape2)
library(tidyverse)
#library(gridExtra)
#library(data.table)

process_noise<-function(fit) {
  
  if (!extended) stop('ProcessZ must be done using the "extended dataset" to get N in the intermidiate year')
   
  n.ini <- melt(ntable(fit))
  colnames(n.ini) <-c("year","age","N.ini")
  n.end<-n.ini
  colnames(n.end)<-c("year","age","N.end")
  n.end$age<-n.end$age-1
  n.end$year<-n.end$year-1
  
  a<-merge(x=n.ini,y=n.end,all.x=TRUE)
  maxAge<-max(a$age)
  maxYear<-max(a$year)
 # a<-subset(a,(age %in% (1:(maxAge-2)) & (year !=maxYear)))  #delete plus group and preceeding group & last year
  
  a$trueZ=log(a$N.ini/a$N.end)
  
  ztab <- melt(faytable(fit)+fit$data$natMor)
  colnames(ztab)<-c("year","age","Z")
  a<-merge(x=a,y=ztab,all.x=TRUE)
  
  a$processZ<-a$trueZ-a$Z
  
  wtab<-melt(fit$data$catchMeanWeight)
  colnames(wtab)<-c("year","age","W")
  a<-merge(x=a,y=wtab,all.x=TRUE)
  
  a$process_N <-  a$N.ini*exp(-a$trueZ) -a$N.ini*exp(-a$Z) 
  a$process_bio<-a$W *a$process_N 
  
  a$n_ratio<-a$process_N/a$N.ini

  head(subset(a,year==2019),10)
  
  a<-subset(a,age<=8)

  
  pz<-tapply(a$processZ,list(a$year,a$age),sum,na.rm=TRUE)
  
  png(file=file.path('res','process_Z.png'),width=1000,height=1000,pointsize=25)
  X11(h=7,w=9)
      ggplot(a,aes(year,processZ)) +
      theme_bw() +
      geom_text(aes(label=age)) +
      stat_smooth(span=0.1) +
      labs(x="",y="",title="Process error expressed as deviations in mortality")
  ggsave(file.path('res','process_Z.png'))
  
  
  ggplot(a,aes(year,processZ)) +
    theme_bw() +
    geom_bar(stat="identity") +
    facet_wrap(~ age, scale="free_y",ncol=4) +
    labs(x="", y="",title="Process error expressed as deviations in mortality")
  ggsave(file.path('res','process_Z.png'))
  
      
  ggplot(a,aes(year,process_N)) +
      theme_bw() +
      geom_bar(stat="identity") +
      facet_wrap(~ age, scale="free_y",ncol=4) +
      labs(x="", y="",title="Process error expressed as deviations in number of fish")
  ggsave(file.path('res','process_N.png'))
  
      
  ggplot(a,aes(year,n_ratio*100)) +
      theme_bw() +
      geom_bar(stat="identity") +
      facet_wrap(~ age, scale="free_y",ncol=4) +
      labs(x="", y="",title="Process error expressed as percentage of initial number of fish ")
  ggsave(file.path('res','process_Npct.png'))
  
  b<-aggregate(process_bio~year,data=a,sum)
     ggplot(data=b, aes(x=year, y=process_bio/1000)) +
      geom_bar(stat="identity") +labs(x='Year',y='Biomass (thousands tonnes)')
     ggsave(file.path('res','process_cumBio.png'))
     
   
  return(list(pz,a))
}
  
