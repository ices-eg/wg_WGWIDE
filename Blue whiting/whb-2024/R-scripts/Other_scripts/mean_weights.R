library(readxl)
library(tidyverse)
library(data.table)


a<-read_excel(file.path(year.root,"prelim_final.xlsx"))
head(as.data.frame(a),15)
unique(a$source)
a<-subset(a,source %in% c("preliminary age 10+","final","average","previous_year") & year>=2016)
a<-select(a,-age_0,-age_11,-age_12,-age_13,-age_14,-age_15)
a<-reshape(a,direction='long',idvar=c("source","year","type"),varying=list(4:13),v.names='value')

a<- a %>% filter(year<2019) %>% mutate(year=factor(year)) %>% rename(age=time)

prelim<- a %>% filter(source=='preliminary age 10+')  %>% rename(prelim=value)  %>% select(-source)
final<- a %>% filter(source=='final')  %>% rename(final=value) %>% select(-source)
average<-a %>% filter(source=='average')  %>% rename(average=value) %>% select(-source)
previous<-a %>% filter(source=='previous_year')  %>% rename(previous=value) %>% select(-source)

final[final$type=='weca','final']          <- final[final$type=='weca','final']*1000
average[average$type=='weca','average']    <- average[average$type=='weca','average'] * 1000
previous[previous$type=='weca','previous'] <- previous[previous$type=='weca','previous'] * 1000

b<-left_join(prelim,final)  %>% left_join(average)  %>% left_join(previous)

weca<-filter(b,type=='weca' )


require(gridExtra)

pl1<-ggplot(weca, aes(prelim, final, colour = year)) +  geom_point() +geom_abline(intercept = 0, slope = 1) +labs(y='Final weight',x='Three years average')
pl2<-ggplot(weca, aes(average, final, colour = year)) +  geom_point() +geom_abline(intercept = 0, slope = 1) +labs(y='Final weight',x='Preliminary')
pl3<-ggplot(weca, aes(previous, final, colour = year)) +  geom_point() +geom_abline(intercept = 0, slope = 1) +labs(y='Final weight',x='Previous year')

x11(width=10,height=6)
grid.arrange(pl1,pl2,pl3 ,ncol=3)

weca<- weca %>% mutate(pre_fin=prelim/final,avg_fin=average/final,previ_fin=previous/final,Age=factor(age))
pl1<-ggplot(weca, aes(Age, pre_fin, colour = year)) +  geom_point() +ylim(0.8,1.2)+geom_abline(intercept = 1, slope = 0) +labs(y='Preliminary / Final weight',x='Age')
pl1
pl2<-ggplot(weca, aes(Age, avg_fin, colour = year)) +  geom_point() +ylim(0.8,1.2)+geom_abline(intercept = 1, slope = 0) +labs(y='3 years average / Final weight',x='Age')
pl2
pl3<-ggplot(weca, aes(Age, previ_fin, colour = year)) +  geom_point() +ylim(0.8,1.2)+geom_abline(intercept = 1, slope = 0) +labs(y='Previous year / Final weight',x='Age')
pl3

x11(width=10,height=6)
grid.arrange(pl1,pl2,pl3, ncol=3)

x11(width=7,height=10)
grid.arrange(pl1,pl2,pl3, nrow=3)


library(reshape2)
w<-read.ices(file.path(stock.dir     ,'DATA','cw.dat'))

#ggplot needs a dataframe
w <- as.data.frame(w)
#id variable for position in matrix 
w$year <- row.names(w) 
#reshape to long format
w <- as_tibble(melt(w,id.var="year")) %>% rename(age=variable,weight=value)
head(w)

#plot
x11(width=7,height=6)
w %>% filter(year>2010) %>% ggplot(aes(x=year,y=weight,group=age,colour=age)) +
  labs(y='Weight (kg)',x='Year') +
  geom_point() +
  geom_line(aes(lty=age))

