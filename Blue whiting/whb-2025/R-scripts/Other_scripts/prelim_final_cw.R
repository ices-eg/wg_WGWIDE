##### 1. Extract preliminary and final weight data for recent assessments ----
cat(stock.dir,'\n')
setwd(stock.dir)

source('src/dataplot.R')
source('src/common.R')
# setwd('data')
# catch.no<-read.ices('cw.dat')
# cn<-catch.no
# land.mean.weight<-read.ices('lw.dat')
# setwd('..');

wide.root<-file.path('m:','WGWIDE')

assess<-c(
  # file.path(wide.root,'whb-2016','WHB_2016_prelim_catch_ADG'),
  # file.path(wide.root,'whb-2017','bw_2017_preliminary_2017_catch'),
  # file.path(wide.root,'whb-2018','bw_2018_preliminary_2018_catch'),
  # file.path(wide.root,'whb-2019','bw_2019_preliminary_2019_catch'),
  # file.path(wide.root,'whb-2020','bw_2020_preliminary_2020_catch'),
  # file.path(wide.root,'whb-2021','bw_2021_preliminary_2021_catch'),
  file.path(wide.root,'whb-2022','bw_2022_preliminary_2022_catch'),
  file.path(wide.root,'whb-2023','bw_2023_preliminary_catch'),
  file.path(wide.root,'whb-2024','bw_2024_preliminary_catch')
)

a<-lapply(assess,function(x){
  print(x)
  setwd(x)
  cw <-read.ices(file.path('data','cw.dat'))
  year<-as.numeric(max(rownames(cw)))
  prelim<-data.frame(year=year,type='preliminary',tail(cw,1))
  final<-data.frame(year=year-1,type='final',head(tail(cw,2),1))
  rbind(prelim,final)
})

a<-do.call(rbind,a)
head(a)


##### 2. Plot comparison of prelim and final catch data ----
a<-subset(a,year>=2022 & year<=2023)
b<-reshape(a,direction='long',idvar = c('year','type'),varying=list(3:12),v.names='CW',timevar='Age')
b[order(b$Age,b$year,b$type),]
library(tidyverse)

tst<-as_tibble(b)
tst$assmYear <- with(tst, ifelse(year == 2022, "WGWIDE 2022",
                                 ifelse(year == 2023, "WGWIDE 2023",NA)))

p <- ggplot(data=tst, aes(x=Age, y=CW, 
                     colour = factor(assmYear), 
                     linetype = type, 
                     shape = factor(assmYear), 
                     group = interaction(type, assmYear))) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(x="Age",y="Weight at age (kg)") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8,0.3),
        panel.grid.minor.x = element_blank())
X11()
print(p)

ggsave('prelim_final_cw.png', path = file.path(stock.dir,'data'), width = 5, height = 4)
