
rm(list=ls())


year.root<-file.path('c:','mv','WGWIDE','whb-2020')
year.root<-file.path('h:','WGWIDE','whb-2020')

# the "extended version includes dummy survey data (NA's) for the year after the last catch year to get stock numbers and SSB at the start of the "intermidiate" year (which in this case is the TAC year)
extended<-TRUE  # default 

stock.dirs<-c(file.path(year.root,"r_2018"), file.path(year.root,"r_2018_prelim"),file.path(year.root,"r_2018_final"))
#stock.dirs<-c(file.path(year.root,"r_2017"), file.path(year.root,"r_2017_prelim"),file.path(year.root,"r_2017_final"))

stock.dir<-stock.dirs[3]

#extended<-TRUE  #used for forecast

cleanup<-function(){for(i in dev.list()) if (i[[1]]>1) dev.off()}  # just a usefull function

library(ellipse)  # MV, used in plotting results 
library(stockassessment)


#PLEASE NOTe  ###################   # master for all R codes to be copied to the selected stock environment
code.dir<-file.path(year.root,'src')  
file.copy(from=code.dir, to=stock.dir, overwrite = TRUE,  recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)




#delete all  previous runs, forecast, residuals etc
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
# after this all results needed should be in the fit object
# configuration in conf

if (TRUE) {
  #residualplots (must be done before the extra plots below !!!!)
  RES<-residuals(fit)
  
  #plot(RES) #observation noise
  setwd(stock.dir)
  save(RES, file="run/RES.RData")
  
  RESP<-procres(fit)
  #plot(RESP)  # Process noise
  save(RESP, file="run/RESP.RData")
}


# plot the result (default SAM script)
Extended<-extended # Extended use in the plotscript !
setwd(stock.dir)
cat(stock.dir)
# please note that the stampit function in common.R has been modified 
source(file.path(stock.dir,"src","plotscript.R"))

# some extra plotting
fit$data$surveys<-surveys  # MV addition
source(file.path(stock.dir,"src","MV_functions.R"))
CommonThings(fit)  # some initializations

source(file.path(year.root,'R-script-common','processZ.R'))
pz<-process_noise(fit) 
round(pz[[1]],3)


round(obsVar(fit),2)

round(exp(fit$pl$logSdLogN),2)  #process noise, N age 1 and ages 2-10

tp<-tableParameters(fit)
print(round(tp,2),na.print = "")
xtab(tp, caption=paste('Table 11. Parameter estimates','.',sep=''), cornername='Parameter \ Year', 
     file=file.path(stock.dir,'res',paste(stamp,'_tab11a.html',sep='')), dec=rep(2,ncol(ftab)))



##########################################################
# Here, you should have made runs for all the configurations to compare 

proot<- file.path(year.root,'Compare')  # output directory


stock.dirs
stock.dir<-stock.dirs[3]


my.roots<-stock.dirs
my.labels<-c('no preliminary catch','Preliminary catch','Final catch')
fileName<-'sensi'


resul<-NULL
resul2<-NULL
resul3<-NULL
resul4<-NULL

col.pref<-cbind(c(0,0,0),c(255,204,0),c(102,0,153),c(102,204,0),
                c(255,0,153),c(255,0,0),c(153,0,102),c(51,204,255),
                c(153,153,153))

palette(apply(rgb2hsv(col.pref),2,function(x)hsv(x[1],x[2],x[3])))
palette("default")

fy<-2009  # years in plot
ly<-2020


plots<-function(){
  getFit<-function(rx) {
    setwd(rx)
    cat(getwd(),'\n')
    load(file.path(rx,'run','model.RData'))  # load fit
    return(fit)
  }
  
  
  su<-lapply(my.roots,getFit)
  attr(su, "fit") <- fit
  class(su) <- "samset"
  
  names(su)<-my.labels
  par(cex.lab=1, cex.axis=1, mar=c(5,5,1,1))
  par(mfrow=c(2,2))
  
  ssbplot(su, las=0, drop=0,ci=F,xlim=c(fy,ly))

  fbarplot(su,las=0, ci=FALSE,drop=1,xlim=c(fy,ly))

  recplot(su, ci=FALSE, las=0,drop=0,xlim=c(fy,ly))
  stampit()
  
}


setwd(proot)
file.remove(dir(pattern='png$'))
stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))

png(filename = paste("big_",stamp,"_%03d.png", sep=''), width = 1200, height = 1200, 
    units = "px", pointsize = 20, bg = "white")
plots()    
dev.off()

# next try



getFit<-function(rx) {
  setwd(rx)
  cat(getwd(),'\n')
  load(file.path(rx,'run','model.RData'))  # load fit
  return(fit)
}

su<-lapply(my.roots,getFit)
names(su)<-my.labels

setwd(proot)
file.remove(dir(pattern='png$'))
stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))

png(filename = paste("big_",stamp,"_%03d.png", sep=''), width = 1200, height = 800, 
    units = "px", pointsize = 20, bg = "white")

par(cex.lab=1, cex.axis=1, mar=c(5,5,1,1))
par(mfrow=c(2,2))


ssbplot(su[[1]], las=1, drop=1,ci=FALSE,xlim=c(fy,ly),col='black')
legend("topright",legend=my.labels,fill=c('black','blue','red'))
ssbplot(su[[2]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='blue',add=TRUE)
ssbplot(su[[3]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='red',add=TRUE)


fbarplot(su[[1]], las=1, drop=1,ci=FALSE,xlim=c(fy,ly),col='black')
fbarplot(su[[2]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='blue',add=TRUE)
fbarplot(su[[3]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='red',add=TRUE)


recplot(su[[1]], las=1, drop=1,ci=FALSE,xlim=c(fy,ly),col='black')
recplot(su[[2]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='blue',add=TRUE)
recplot(su[[3]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='red',add=TRUE)

stampit()

dev.off()


su[[3]]
tp<-lapply(su,tableParameters)
a<-cbind(tp[[1]][,1],tp[[2]][,1],tp[[3]][,1])
colnames(a)<-my.labels
a

print(round(a,3),na.print = "")
xtab(a, caption=paste('Table 11. Parameter estimates','.',sep=''), cornername='Parameter \ Year', 
     file=file.path(proot,paste(stamp,'_tab11a.html',sep='')), dec=rep(3,ncol(ftab)))

a<-lapply(su, logLik)
a
