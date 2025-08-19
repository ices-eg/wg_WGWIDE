##### 1. Extract preliminary and final catch data for recent assessments ----
cat(stock.dir,'\n')
setwd(stock.dir)

# source('src/dataplot.R')
source('src/common.R')
setwd('data')
catch.no<-read.ices('cn.dat')
cn<-catch.no
land.mean.weight<-read.ices('lw.dat')
setwd('..');

wide.root<-file.path('m:','WGWIDE')

assess<-c(
          file.path(wide.root,'whb-2016','WHB_2016_prelim_catch_ADG'),
          file.path(wide.root,'whb-2017','bw_2017_preliminary_2017_catch_extended'),
          file.path(wide.root,'whb-2018','bw_2018_preliminary_2018_catch_extended'),
          file.path(wide.root,'whb-2019','bw_2019_preliminary_2019_catch_extended'),
          file.path(wide.root,'whb-2020','bw_2020_preliminary_2020_catch_extended'),
          file.path(wide.root,'whb-2021','bw_2021_preliminary_2021_catch_extended'),
          file.path(wide.root,'whb-2022','bw_2022_preliminary_2022_catch_extended'),
          file.path(wide.root,'whb-2023','bw_2023_preliminary_catch_extended'),
          file.path(wide.root,'whb-2024','bw_2024_preliminary_catch')
         )

a<-lapply(assess,function(x){
  print(x)
  setwd(x)
  cn<-read.ices(file.path('data','cn.dat'))
  year<-as.numeric(max(rownames(cn)))
  prelim<-data.frame(year=year,type='preliminary',tail(cn,1))
  final<-data.frame(year=year-1,type='final',head(tail(cn,2),1))
  rbind(prelim,final)
})

a<-do.call(rbind,a)
head(a)

##### 2. Plot comparison of prelim and final catch data ----
a<-subset(a,year>=2017 & year<=2023)
b<-reshape(a,direction='long',idvar = c('year','type'),varying=list(3:12),v.names='CN',timevar='Age')
b$CN<-b$CN/1000
b[order(b$Age,b$year,b$type),]
library(tidyverse)
b[b$type=="preliminary","type"]<-" preliminary"

tst<-as_tibble(b)  %>% mutate(Age=paste("Age",formatC(Age,flag="0",width=2)))

p<-ggplot(data=tst, aes(x=year, y=CN, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~Age, scales="free_y", ncol=5) + xlab("") +ylab("Catch numbers (millions)")
X11(h=7,w=13);print(p)

png(filename=file.path(stock.dir,'data','prelim_final_cn.png'),width=1200,height=700,pointsize = 20)
print(p)
cleanup()

##### 3. Create table with prelim and final catch data ----
aa<-lapply(assess,function(x){
  print(x)
  setwd(x)
  cn<-read.ices(file.path('data','cn.dat'))
  cw<-read.ices(file.path('data','cw.dat'))
  
  year<-as.numeric(max(rownames(cn)))
  prelim<-sum(tail(cn,1) * tail(cw,1))
  
  final<-sum(head(tail(cn,2),1) * head(tail(cw,2),1))
  
  rbind(data.frame(year=year,catch=prelim, type='prelim'),
        data.frame(year=year-1,catch=final, type='final'))
})

aa<-do.call(rbind,aa)
head(aa)
aaa<-xtabs(catch~year+type,data=aa)
aaa
aaa<-cbind(aaa,Change_PCT=(aaa[,1]-aaa[,2])/aaa[,2]*100)
aaa<-aaa[-1,] #delete the first year with just final data

stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))

xtab(aaa, caption=paste('Table 30. Preliminary and final catches. Change_PCT is (final-prelim)/prelim*100 ','.',sep=''), cornername='', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab30.html',sep='')), dec=c(0,0,1))
