### Calculate the proportion of zeroes in each survey and plot
# Created by Alfonso Perez

rm(list=ls())

library(plyr)

datpath="C:/git/wg_WGWIDE/NSHM/data/"
surveypath="C:/git/wg_WGWIDE/NSHM/results/survey/"
resultpath="C:/git/wg_WGWIDE/NSHM/results/model/"
figPath <- "C:/git/wg_WGWIDE/NSHM/figures/model/"

# Load cleaned survey data
#dat <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/survey_data_1992-2018_clean.csv",stringsAsFactors = FALSE)
dat <- read.csv(paste0(surveypath,"survey_data_1992-2018_clean.csv",sep=""),stringsAsFactors = FALSE)

# Define folder for output
#resultpath="M:/My Documents/WGWIDE/Assessment 2019/NSHM/hurdle_model/output/"


dat=ddply(dat,.(Year,Survey,HaulNo,groupsize),summarize,Number=sum(Number))
head(dat)

dat$Count=ifelse(dat$Number==0 ,0,1)

res=ddply(dat,.(groupsize,Survey,Year),summarize,total=length(Count),presence=sum(Count))

res$propzero=1-(res$presence/res$total)

write.csv(res,paste(resultpath,"Proportion_Zeros_Surveys.csv",sep=""), row.names=F)

png(filename=paste(figPath,"Proportion_Zeros_Surveys.png",sep=""), heigh=3000, width=5000, units="px", pointsize=3, res=450)
xyplot(propzero~Year|groupsize,data=res,groups=Survey,type="b",pch=19,auto.key=T,
       scales=list(x = list(tick.number=24,at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27),labels=c("","" ,"",'1995',	'',	'','','',	'2000',	'',	'',	'',	'',	'2005',	'',	'','','',	'2010','','',	'','','2015','',''),cex=1,tck=c(1,0))),
       ylim=c(0,1))

dev.off()

