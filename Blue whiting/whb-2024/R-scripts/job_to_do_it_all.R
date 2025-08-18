####################################################################################################################-
# Run BW assessment model and deterministic forecast
#
# WGWIDE 2024
#
# Original author: Morten Vinther, adjusted by Esther D. Beukhof
####################################################################################################################-
rm(list=ls())

# Set year of assessment
assessmentYear  <- 2024

# Libraries
if (FALSE) {  # to install packages
  #is library stockassessment  installed
  library(stockassessment)
  # to install a newer version
  # remove.packages('stockassessment') # remember also to delete the directory stockassessment in the win-library (if not the old SAM version is kept, in some cases) 

  Sys.unsetenv("GITHUB_PAT")
  devtools::install_github("fishfollower/SAM/stockassessment")
}

library(ellipse)  #  used in plotting results 
library(stockassessment)

# Set paths

## Main folder for this year
year.root      <- file.path('m:','WGWIDE',paste0('whb-',assessmentYear))

## R scripts
R_script       <- file.path(year.root,'R-scripts')
other_R        <- file.path(R_script,'Other_scripts')

## Data
#stock.dir      <-file.path(year.root,paste0("bw_",assessmentYear,"_preliminary_catch")); extended<-FALSE
 stock.dir     <-file.path(year.root,paste0("bw_",assessmentYear,"_preliminary_catch_extended")) ; extended<-TRUE  # used for forecast

# The "extended version includes dummy survey data (NA's) for the year after the last catch year to get stock numbers and SSB at the
# start of the "intermediate" year (which in this case is the TAC year).
# Data for the extended are copied from the "normal" run, and an extra year (and data) is added for mo.dat, nm.dat, pf.dat. pm.dat, 
# sw.dat and survey.dat.
# For sw.dat, we duplicate weights in the last year (e.g. unchanged mean weight for the last two years in sw.dat)
# For survey.dat the new data line should be missing observations (- 1).

# Dependencies
cleanup         <-function(){for(i in dev.list()) if (i[[1]]>1) dev.off()}  # just a useful function



############ PLEASE NOTE ##################- 
# master for all R codes used by SAM should be copied once when a new stock environment is established, to ensure that the newest code is applied
if (FALSE) {
  code.dir        <- file.path(year.root,'SAM_src_source') 
  cpFiles         <- list.files(path=code.dir) 
  lapply(cpFiles, function(x) file.copy(from=file.path(code.dir,x), to=file.path(stock.dir,'src',x), overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE))
}
###########################################- 



####################################################################################################################-
### Read interCatch output and format data for SAM ----

# Update catch numbers and mean weights for the "final year" (the year with reported catches for the full year)
if (FALSE) { 
  
  source(file.path(R_script, 'Read_new_data.R'))
  
  finalDir        <- "whb.27.1-91214_all_ 2024-8-26 13_52_20"  # directory name for the un-zipped interCatch data
  final           <- read_Intercatch_data(root=file.path(year.root,finalDir)) 
  final   # PLEASE NOTE: you have to update the catch number ("cn.dat") and mean weights ("cw.dat") files in the data directory manually
  sum(final$canum*final$weca)  #total catch weight (without 0-groups), should be close to caton.txt
 }

# Raise preliminary catches to total (best guess on) catches for the preliminary year
if (FALSE) {  
  
  source(file.path(R_script,'Read_new_data.R'))
  
  prelimDir       <- "whb.27.1-91214_all_ 2024-8-28 15_39_28_2024_1S"
  prelim          <- read_Intercatch_data(root=file.path(year.root,prelimDir)) # 2024 preliminary data (again) 
  prelim  
  
  sum(prelim$canum*prelim$weca)  #total catch weight (without 0-groups)
  
  newC            <-prelim[['canum']] 
  newW            <-prelim[['weca']]
  SOP             <-sum(newC*newW)   #total catch weight
  SOP #sum of product  
  
  # Preliminary year catches, the best guesses on total catch in the current (full) year (the catch of O-groups should be subtracted, but not done)
 
  totalyield      <- 1881072  ## best guess for 2024, from the preliminary catch table by country and quarter
 
  factor          <- totalyield/SOP # factor between the expected yield for the current year (estimated at the meeting) and the preliminary yield from InterCatch 
  SOP;totalyield;factor
 
  # REMEMBER TO UPDATE THE INPUT FILES ("cn.dat" and "cw.dat" for current year as preliminary) WITH THE VALUES BELOW
  newCscaled      <- newC*factor   # Canum scaled to expected catches
  round(newCscaled,1) #used to update cn.dat for the preliminary data

  # and mean weights
  round(newW,5) #used to update cw.dat for the preliminary data
  
  sum(newCscaled*newW) # just checking
} 


################## We are now ready to make the SAM run ##################-



####################################################################################################################-
### Run SAM ----

# Delete all previous runs, forecast, residuals etc
cat(stock.dir,'\n')
setwd(stock.dir)
setwd("run")
for(f in dir(pattern="RData"))file.remove(f) 
setwd("..")


# Check input data
setwd(stock.dir)
source(file.path(stock.dir,"src","dataplot.R"))
check.all(path=file.path(stock.dir,'data'))


# Read data into "dat" object
setwd(stock.dir)
source(file.path(stock.dir,"src","datascript.R"))


# Read configuration and run the model
setwd(stock.dir)
source(file.path(stock.dir,"src","model.R"))
# after this all results should be in the fit object ("fit")
# and configuration in object "conf"
fit

# Residuals
RES              <-residuals(fit)
# plot(RES) #observation noise
setwd(stock.dir)
save(RES, file="run/RES.RData")

# Process noise
RESP             <-procres(fit)

#plot(RESP) 
save(RESP, file="run/RESP.RData")


#retrospective runs
setwd(stock.dir)
# source(file.path(stock.dir,"src","model.R")) # run again, to be absolutely sure you have a fresh copy of "fit"
RETRO             <-retro(fit,year=5)
setwd(stock.dir)
save(RETRO, file="run/RETRO.RData") 

# plot the result, using (modified)  SAM script
Extended          <- extended
  # Extended used in the plotscript (do not change) !
setwd(stock.dir)
# please note that the stampit function in common.R has been modified 
source(file.path(stock.dir,"src","plotscript.R"))

# some extra plotting
fit$data$surveys  <-surveys  # MV addition
source(file.path(stock.dir,"src","MV_functions.R"))
CommonThings(fit)  # some initializations

if (Extended & FALSE) {  #work only with "extended" configuration
  source(file.path(R_script,'processZ.R'))
  pz              <-process_noise(fit) 
  round(pz[[1]],3)
}
#just checking 
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

colnames(a)<-as.character((assessmentYear-4):assessmentYear)
xtab(a, caption=paste('Table 11. Parameter estimates, retrospective','.',sep=''), cornername='Parameter \ Year', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab11.html',sep='')), dec=rep(2,ncol(ftab)))


# Leave out IBWSS
if (FALSE) { # it does not work with my SAM version, MV August 2024
  lo <- leaveout(fit,ncores=1)  # MV added ncores=1, 28 August 2024
  png(file.path(stock.dir,'res','leave_one_out.png'),width=900,height=1200,pointsize=25)
  plot(lo)
  dev.off()
  #ssbplot(lo);recplot(lo);fbarplot(lo)
}

if (FALSE) { # not used !
  v<-cov2cor(fit$sdrep$cov.fixed) 
  library(lattice)
  X11()
  levelplot(v,xlab='',ylab='')
}

# other scripts used for presentation etc.
library(tidyverse) #MV insert
if (TRUE) {
  source(file.path(other_R,'prelim_final_cn.R'))  # plot of preliminary and final catch at age, and table of preliminary and final catches
  source(file.path(other_R,'prelim_final_cw.R')) #plot of preliminary final weight at age
  source(file.path(other_R,'catch_curves_plot.R')) # catch curve plot and proportion catch plot
}


####################################################################################################################-
### Run forecast ----

# Done using the extended version (that is with estimated stock numbers for the end  of the present year)

# TAC year and years to average across
TACyear             <- 2025
#ave.years vector of years to average for weights, maturity, M and such
ave.years           <- c((assessmentYear-2):assessmentYear)   # for mean weights etc. (copy of preliminary data year is not used, used since 2019)

# Set years for recruitment to average across - depending on taking full time series or high recruitment regime only
# rec.years           <-1981:2020  # years to calculate GM recruitment (full time series minus 1)
rec.years           <-1996:2023  # years to calculate GM recruitment (high recruitment regime) 

# Set reference points
Fpa                 <-0.32
FMSY                <-0.32
Flim                <-0.88
Blim                <-1500000
Bpa                 <-2250000
MSYBtrigger         <-2250000

# Set latest catch advice and latest catch
last.advice         <-1529754  # Published 29 September 2023
last.catch          <-tail(catchtable(fit,obs.show=TRUE),1)[1,4] 

# Extract latest F  
Fsq                 <-fbartable(fit)[as.character(TACyear-1),1]  #F status quo = average F from last year - used for one of the catch options

if (!extended) stop('Forecast must be done using the "extended dataset" to get the right (process corrected) initial stock numbers')

# Get recruitment table and extract latest recruitment value
rec                 <-rectable(fit)  # recruitment
rec
rec.last            <-tail(rec,1)[1,1]

# Estimate the geometric mean recruitment based on the selected years
GM                  <-exp(mean(log(rectable(fit)[as.character(rec.years),1])))  # all years in rec.year
GM

# Set reference year for recruitment
recRefYear          <-TACyear-1
refRecruit          <-rectable(fit)[as.character(recRefYear),1]

# use SAM estimate for age 1 in terminal year (year of the preliminary data)
my.nscale           <- c(GM/refRecruit,1,GM/refRecruit)  # scaling of N: age 1 first year, age 2 first year and age 1 second+ year

# change the SAM estimate for age 1 in the terminal year (year of the preliminary data), in this case use GM for all years
#my.nscale<- c(GM/refRecruit,GM/refRecruit,GM/refRecruit)  # scaling of N: age 1 first year, age 2 first year and age 1 second+ year

my.nscale


source(file.path(stock.dir,"src","forecast_deterministic.R"))

#just testing
tst                 <-forecast_deterministic(fit, 
                                             ave.years =ave.years,
                                             fscale    =c(NA,NA,NA),
                                             fval      =c(0.32,0.32,0.32), 
                                             nscale    =my.nscale,
                                             label     =paste0("test"),
                                             rec.years =TACyear-1)

tab                 <-attr(tst, "tab")
tab
ssb.start.TAC.year  <-tab[1,'ssb:median']
 

setwd(stock.dir)

load("run/model.RData")



# Set up optimizing function 
SAM_optimize<-function(x,speci) {
  FF        <-x
  my.fval   <-c(FF,FF)
  set.seed(12345)
  a         <-forecast_deterministic(fit, 
                                     ave.years  =ave.years, 
                                     fscale     =rep(NA,2),
                                     fval       =my.fval, 
                                     nscale     =my.nscale,
                                     label      =paste0("Status quo F in first year followed by F",FF),
                                     rec.years  =TACyear-1)
  b         <-attr(a, "tab")
  out       <-b[speci$y,speci$target]
  target    <-(speci$value-out)^2
  return(target)
}

if (FALSE) { #just testing
  speci     <-list(target='ssb:median',value=Blim,y=2) #SSB target for 2024 to median and Blim for 2025
  op        <-optimize(SAM_optimize,c(0,7),speci=speci)
  op
  FF        <-op$minimum  
  my.fval   <-c(FF,FF) #take F-values for 2024 and 2025
  forecast_deterministic(fit, 
                         ave.years   =ave.years, 
                         fscale      =rep(NA,2),
                         fval        =my.fval, 
                         nscale      =my.nscale,
                         label       =paste0("Status quo F in first year followed by F",FF),
                         rec.years   =TACyear-1)
}

# Set-up scenario table
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

# Get catch options for all the scenarios from the optimizing function
b <-lapply(scen,function(x){ if (x$target=='F') #EB: what is done here?
  return(list(minimum=x$value,objective=0)) 
  else optimize(SAM_optimize,c(0,7),speci=x)})

# should be small ( less than around 1000)
unlist(lapply(b,function(x) x$objective)) #EB: what are we checking here?

targetF             <-unlist(lapply(b,function(x) x$minimum))
labels              <-unlist(lapply(scen,function(x) x$lab))

names(targetF)      <-labels
t(t(targetF))

# fsq, SSB=Bpa, SSB= Blim, SSB=last year's SSB catch(y+1)=catch(y)
add.targetF         <-c(0.05,0.10,seq(from=0.15,to=0.35,by=.01),0.45,0.50)
names(add.targetF)  <-paste('F =',add.targetF)
targetF             <-c(targetF,add.targetF)
round(t(t(targetF)),3)

my.fscale           <-c(NA,NA)   # F status quo the first year 

FC                  <-list()
set.seed(12345)

for (FF in targetF) { # EB: what is done here?
  my.fval            <-c(FF,FF)
  #print(my.fval)
  set.seed(12345)
  FC[[length(FC)+1]] <- forecast_deterministic(fit, 
                                               ave.years    =ave.years, 
                                               fscale       =rep(NA,2),
                                               fval         =my.fval, 
                                               nscale       =my.nscale,
                                               label        =paste0("Status quo F in first year followed by F",FF),
                                               rec.years    =TACyear-1)
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

n<-rbind(n1[1,],n2[1,]) #what are the different columns

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

# Check the catch proportions by age for the next year for the first scenario (long-term management strategy)
catchAge <-  FC[[1]][[1]][["catchByAge"]][,1]
catchProp <- catchAge / sum(catchAge)
catchProp
round(catchProp * 100,1) #percentages

