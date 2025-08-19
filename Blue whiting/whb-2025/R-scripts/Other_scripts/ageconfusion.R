#library(devtools)
# Sys.getenv("GITHUB_PAT") # "e7851098c95ad287080d27133a38be489b069e86"
# Sys.unsetenv("GITHUB_PAT")


# devtools::install_github("fishfollower/SAM/stockassessment",ref = "ageconfusion")

year.root<-file.path('m:','WGWIDE','whb-2021')

# the "extended version includes dummy survey data (NA's) for the year after the last catch year to get stock numbers and SSB at the start of the "intermediate" year (which in this case is the TAC year)
extended<-TRUE  # default FALSE, true used for forecast
#stock.dir<-file.path(year.root,"bw_2021_no_preliminary_2021_catch"); 
#stock.dir<-file.path(year.root,"bw_2021_preliminary_2021_catch")
stock.dir<-file.path(year.root,"bw_2021_ageconfusion") ; extended<-TRUE

cleanup<-function(){for(i in dev.list()) if (i[[1]]>1) dev.off()}  # just a useful function

library(ellipse)  # MV, used in plotting results 
library(stockassessment)

# read age confusion data
acFile<-file.path(year.root,"age_reading_error","bw_data.csv")
library(tidyverse)
a<-read_csv(file=acFile) %>% 
  filter(IsApproved==1 ) %>%
  filter(!(age==0 & length>220)   & !(age==0 & length<=150 )) %>%
  dplyr::select(sample,age,expertise,reader_number,AQ_Score) %>%
  mutate(age=if_else(age>10,10,age)) %>%
  filter(age>=1) %>%
  mutate(expertise=1,age=as.integer(age)) %>% # does not work in SAM with different values, so just set it to 1 (Anders )
  group_by(sample) %>% mutate(mode=raster::modal(age)) %>% ungroup()



a%>% mutate(PA=age==mode) %>% group_by(mode) %>% summarise(PA=sum(PA)/n(), CV=sd(age)/mean(age)) # proportion agreement (with model age)
xtabs(~age+mode,data=a)
a<- a %>% arrange(sample,age, expertise)
agesampledata <-as.data.frame(a %>% select(age,sample,data=expertise))
str(agesampledata)
summary(agesampledata)

#write_delim(agesampledata,file=acFile<-file.path(stock.dir,"data","age_samp.dat"),col_names=FALSE)
##


#PLEASE NOTE  ###################   # master for all R codes to be copied to the selected stock environment
# do not do that for age confusion
#code.dir<-file.path(year.root,'src')  
#file.copy(from=code.dir, to=stock.dir, overwrite = TRUE,  recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)

#delete all previous runs, forecast, residuals etc
cat(stock.dir,'\n')
setwd(stock.dir)
setwd("run")
for(f in dir(pattern="RData"))file.remove(f) 
setwd("..")



# check input data
setwd(stock.dir)
source(file.path(stock.dir,"src","dataplot.R"))
check.all(path=file.path(stock.dir,'data'))


# read data into dat object
setwd(stock.dir)
source(file.path(stock.dir,"src","datascript.R"))

# read configuration and run the model
setwd(stock.dir)
source(file.path(stock.dir,"src","model.R"))
# after this all results needed should be in the fit object ("fit")
# configuration in conf




#residual plots (must be done before the extra plots below !!!!)
RES<-residuals(fit)

# plot(RES) #observation noise
setwd(stock.dir)
save(RES, file="run/RES.RData")

RESP<-procres(fit)
#plot(RESP)  # Process noise
save(RESP, file="run/RESP.RData")


#retrospective
setwd(stock.dir)
source(file.path(stock.dir,"src","model.R")) # run again, to be absolutely sure you have a fresh copy
RETRO<-retro(fit,year=5)
setwd(stock.dir)
save(RETRO, file="run/RETRO.RData") 

# rectable(fit)

#fit$pl$logN[1,dim(fit$pl$logR)[[2]]]<-NA
# partable(fit)


# plot the result (default SAM script)
Extended<-extended # Extended used in the plotscript !
setwd(stock.dir)
# please note that the stampit function in common.R has been modified 
source(file.path(stock.dir,"src","plotscript.R"))

# some extra plotting
fit$data$surveys<-surveys  # MV addition
source(file.path(stock.dir,"src","MV_functions.R"))
CommonThings(fit)  # some initializations

source(file.path(year.root,'R-script-common','processZ.R'))
pz<-process_noise(fit) 
round(pz[[1]],3)



parplot(RETRO)

round(obsVar(fit),2)

round(exp(fit$pl$logSdLogN),2)  #process noise, N age 1 and ages 2-10

tp<-tableParameters(fit)
print(round(tp,2),na.print = "")
xtab(tp, caption=paste('Table 11. Parameter estimates','.',sep=''), cornername='Parameter \ Year', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab11a.html',sep='')), dec=rep(2,ncol(ftab)))


a<-cbind(
  tableParameters(RETRO[[4]],doCV=FALSE),
  tableParameters(RETRO[[3]],doCV=FALSE),
  tableParameters(RETRO[[2]],doCV=FALSE),
  tableParameters(RETRO[[1]],doCV=FALSE),
  tableParameters(fit,doCV=FALSE))

colnames(a)<-as.character(2017:2021)
xtab(a, caption=paste('Table 11. Parameter estimates, retrospective','.',sep=''), cornername='Parameter \ Year', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab11.html',sep='')), dec=rep(2,ncol(ftab)))


####
# lo <- leaveout(fit);  plot(lo)

if (FALSE) {
  v<-cov2cor(fit$sdrep$cov.fixed) 
  library(lattice)
  X11()
  levelplot(v,xlab='',ylab='')
}

} 

##### forecast


TACyear<-2022
#ave.years vector of years to average for weights, maturity, M and such
ave.years<-2019:2021   # for mean weigths etc. The last (copy of preliminary data year is not used, used since 2019)
#ave.years<-TACyear-1    # mean weight in the last year 

#rec.years<-1981:2020  # years to calculate GM recruitment (full time series minus 1)
rec.years<-1996:2020  # years to calculate GM recruitment (high recruitment regime, based on the Barents Sea survey

Fpa<-0.32
FMSY<-0.32
Flim<-0.88
Blim<-1500000
Bpa<- 2250000
MSYBtrigger<-2250000

last.advice<- 929292  # Published 30 September 2020
last.catch<-  tail(catchtable(fit,obs.show=TRUE),1)[1,4] 

Fsq<-fbartable(fit)[as.character(TACyear-1),1]

if (!extended) stop('Forecast must be done using the "extended dataset" to get the right (process corrected) initial stock numbers')

rec<-rectable(fit)  # recruitment
rec
rec.last<-tail(rec,1)[1,1]

GM<-exp(mean(log(rectable(fit)[as.character(rec.years),1])))  # all years in rec.year
GM

#reference year for recruitment
recRefYear<-TACyear-1
refRecruit<-rectable(fit)[as.character(recRefYear),1]

# use SAM estimate for age 1 in terminal year (year of the preliminary data)
my.nscale<- c(GM/refRecruit,1,GM/refRecruit)  # scaling of N: age 1 first year, age 2 first year and age 1 second+ year

# change the SAM estimate for age 1 in the terminal year (year of the preliminary data), in this case use GM for all years
#my.nscale<- c(GM/refRecruit,GM/refRecruit,GM/refRecruit)  # scaling of N: age 1 first year, age 2 first year and age 1 second+ year

my.nscale


source(file.path(stock.dir,"src","forecast_deterministic.R"))

#just testing
tst<-forecast_deterministic(fit, ave.years=ave.years,fscale=c(NA,NA,NA),fval=c(0.32,0.32,0.32), nscale=my.nscale,label=paste0("test"),rec.years=TACyear-1)

tab<-attr(tst, "tab")
tab
ssb.start.TAC.year<-tab[1,'ssb:median']


setwd(stock.dir)

load("run/model.RData")



#################  
SAM_optimize<-function(x,speci) {
  FF<-x
  my.fval<-c(FF,FF)
  set.seed(12345)
  a<-forecast_deterministic(fit, ave.years=ave.years, fscale=rep(NA,2),fval=my.fval, nscale=my.nscale,label=paste0("Status quo F in first year followed by F",FF),rec.years=TACyear-1)
  b<-attr(a, "tab")
  out<-b[speci$y,speci$target]
  target<-(speci$value-out)^2
  return(target)
}

if (FALSE) { #just testing
  speci<-list(target='ssb:median',value=Blim,y=2)
  op<-optimize(SAM_optimize,c(0,4),speci=speci)
  op
  FF<-op$minimum
  my.fval<-c(FF,FF) 
  forecast_deterministic(fit, ave.years=ave.years, fscale=rep(NA,2),fval=my.fval, nscale=my.nscale,label=paste0("Status quo F in first year followed by F",FF),rec.years=TACyear-1)
}

scen<- list(
  list(lab="Long-term management strategy (F=FMSY)", target='F',value=FMSY,y=0),
  list(lab="MSY approach: FMSY",                 target='F',value=FMSY,y=0),
  list(lab="F = 0",                              target='F',value=1E-6,y=0),
  list(lab="Fpa",                                target='F',value=Fpa,y=0),
  list(lab="Flim",                               target='F',value=Flim,y=0),
  list(lab=paste0("SSB (",TACyear+1,") = Blim"), target='ssb:median',value=Blim,y=2),
  list(lab=paste0("SSB (",TACyear+1," = Bpa"),   target='ssb:median',value=Bpa,y=2),
  list(lab=paste0("SSB (",TACyear+1,") = MSY Btrigger"), target='ssb:median',value=MSYBtrigger,y=2),
  list(lab=paste0("F = F (",TACyear-1,")"), target='F',value=Fsq,y=0),
  list(lab=paste0("SSB (",TACyear+1,") = SSB (",TACyear,")"), target='ssb:median',value=ssb.start.TAC.year,y=2),
  list(lab=paste0("Catch (",TACyear,") = Catch (",TACyear-1,")"), target='catch:median',value=last.catch,y=1),
  list(lab=paste0("Catch (",TACyear,") = Catch (",TACyear-1,") -20 %"),target='catch:median',value=last.catch*0.8,y=1),
  list(lab=paste0("Catch (",TACyear,") = Catch (",TACyear-1,")  +25%"),target='catch:median',value=last.catch*1.25,y=1),
  list(lab=paste0("Catch (",TACyear,") = Advice (",TACyear-1,") -20 %"),target='catch:median',value=last.advice*0.8,y=1),
  list(lab=paste0("Catch (",TACyear,") = Advice (",TACyear-1,") +25%"),target='catch:median',value=last.advice*1.25,y=1)
)


b<-lapply(scen,function(x){ if (x$target=='F') return(list(minimum=x$value,objective=0)) else optimize(SAM_optimize,c(0,4),speci=x)})

# should be small ( less than around 1000)
unlist(lapply(b,function(x) x$objective))

targetF<-unlist(lapply(b,function(x) x$minimum))
labels<-unlist(lapply(scen,function(x) x$lab))

names(targetF)<-labels
t(t(targetF))

# fsq, SSB=Bpa, SSB= Blim, SSB=last year's SSB catch(y+1)=catch(y)
add.targetF<-c(0.05,0.10,seq(from=0.15,to=0.35,by=.01),0.45,0.50)
names(add.targetF)<-paste('F =',add.targetF)
targetF<-c(targetF,add.targetF)
round(t(t(targetF)),3)

my.fscale<-c(NA,NA)   # F status quo the first year 

FC<-list()
set.seed(12345)





for (FF in targetF) {
  my.fval<-c(FF,FF)
  #print(my.fval)
  set.seed(12345)
  FC[[length(FC)+1]] <- forecast_deterministic(fit, ave.years=ave.years, fscale=rep(NA,2),fval=my.fval, nscale=my.nscale,label=paste0("Status quo F in first year followed by F",FF),rec.years=TACyear-1)
  #cat(FF,'\n')
}

setwd(stock.dir)
save(FC, file="run/forecast.RData")
#source(file.path(stock.dir,"src","plotscript.R"))

FC[[1]]

FC


FbarAges<-c(3,7)
test<-FC[[1]]
n1<-exp(test[[1]]$sim) # Stock at the beginning of the TAC year
n2<-exp(test[[2]]$sim) # Stock at the beginning at next year

n<-rbind(n1[1,],n2[1,])

N<-n[,1:10]
cat(paste0("Stock N at the beginning of the TAC year(",TACyear,") and the following year(",TACyear+1,")\n:"));round(N)
FF<-cbind(n[,11:19],n[,19])
round(FF,3)
expl<-FF[2,]
expl<-expl/mean(expl[FbarAges[1]:FbarAges[2]])

mw<-attr(test,"fit")[['data']]$stockMeanWeight
mw[as.character('2017'),]
pm<-attr(test,"fit")[['data']]$propMat
nm<-attr(test,"fit")[['data']]$natMor
# should be the same as
gem$sw  # gem is from the forecast_deterministic script

tab<-t(rbind(mw[as.character(TACyear-1),],gem$sw ,pm[as.character(TACyear-1),],nm[as.character(TACyear-1),],expl,N[1,]))
rownames(tab)<-paste('Age',attr(test,"fit")[['conf']]$minAge:attr(test,"fit")[['conf']]$maxAge)
colnames(tab)<-c(paste0('Mean weight in the stock and catch (kg) in ',as.character(TACyear-1)),paste0('Mean weight in the stock and catch (kg) in ', as.character(TACyear),'+'),'Proportion mature','Natural mortality','Exploitation pattern',paste0('Stock number(',TACyear,') (thousands)'))

xtab(tab, caption=paste('Table 20. Input to short term forecast','.',sep=''), cornername='Age', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab20.html',sep='')), dec=c(3,3,2,2,3,0))

xtab(tab, caption=paste('UNROUNDED Table 21. Input to short term forecast','.',sep=''), cornername='Age', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab21.html',sep='')), dec=c(5,5,5,5,5,2))





#intermediate year
ft<-attr(FC[[1]],"tab")
refSSB<-ft[as.character(TACyear),'ssb:median']
refCatch<-catchtable(fit,obs.show = TRUE)[as.character(TACyear-1),'sop.catch']

ft<-rbind(Fsq, 
          refSSB,
          rec.last,
          ft[as.character(TACyear),'rec:median'],
          ft[as.character(TACyear+1),'rec:median'],
          refCatch)

rownames(ft)<-c(paste0('F ages ',FbarAges[1],'-',FbarAges[2],' (',TACyear-1,')'),
                paste0('SSB (',TACyear,')'),
                paste0('R age 1 (',TACyear-1,')'),
                paste0('R age 1 (',TACyear,')'),     
                paste0('R age 1 (',TACyear+1,')'),
                paste0('Total catch (',TACyear-1,')'))  
ft<-cbind(ft,rep(NA,dim(ft)[[1]]))
colnames(ft)<-c('Value','Notes')


xtab(ft, caption=paste('Table 19. Intermediate year input to short term forecast','.',sep=''), cornername='Values', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab19.html',sep='')), dec=c(3,0))


SSByear<-TACyear+1
if(exists("FC")){  
  res<-lapply(FC, function(f){
    tf<-attr(f,"tab");
    c(tf[as.character(TACyear),'catch:median'],tf[as.character(TACyear),'fbar:median'],tf[as.character(SSByear),'ssb:median'])
  })
  
  res<-cbind(matrix(unlist(res),ncol=3,byrow=TRUE),0,0,0)
  res[,4]<-(res[,3]-refSSB)/refSSB*100   
  res[,5]<-(res[,1]-refCatch)/refCatch*100 
  res[,6]<-(res[,1]-last.advice)/last.advice*100 
  head(res)
}

rownames(res)<-names(targetF)
colnames(res)<-c(paste0('Catch(',TACyear,')'), paste0('F(',TACyear,')'),paste0('SSB(',SSByear,')'),'% SSB change*', '% Catch change**','% Advice change***')
head(res,14)
xtab(res, caption=paste('Table 22.  short term forecast','.',sep=''), cornername='Basis', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab22.html',sep='')), dec=c(0,3,0,1,1,1))
#res


