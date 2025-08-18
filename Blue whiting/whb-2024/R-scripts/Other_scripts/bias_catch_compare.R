
rm(list=ls())


year.root<-file.path('c:','mv','WGWIDE','whb-2020')
year.root<-file.path('h:','WGWIDE','whb-2020')

# the "extended version includes dummy survey data (NA's) for the year after the last catch year to get stock numbers and SSB at the start of the "intermidiate" year (which in this case is the TAC year)
extended<-TRUE  # default 

stock.dirs<-c(file.path(year.root,"bw_2020_preliminary_2020_catch_extended"),
              file.path(year.root,"bw_2020_preliminary_2020_catch_extended_updated_baseline"),
              file.path(year.root,"bw_2020_preliminary_2020_catch_extended_bootstrap_replicates"))
my.labels<-c('Old','Updated','Bootstrap')
stock.dir<-stock.dirs[3]


stock.dirs<-c(file.path(year.root,"bw_2020_preliminary_2020_catch_extended"),
              file.path(year.root,"test_age_correct_catch"))
my.labels<-c('Old','BIASED CORRECTED CATCH')
stock.dir<-stock.dirs[1]

my.roots<-stock.dirs



proot<- file.path(year.root,'Compare')  # output directory

fileName<-'update'





getFit<-function(rx) {
  setwd(rx)
  cat(getwd(),'\n')
  load(file.path(rx,'run','model.RData'))  # load fit
  return(fit)
}

su<-lapply(my.roots,getFit)
names(su)<-my.labels


# please note that the stampit function in common.R has been modified 
# just to load plotscript 
setwd(stock.dir)
Extended<-TRUE

# read data into dat object
setwd(stock.dir)
source(file.path(stock.dir,"src","datascript.R"))

# please note that the stampit function in common.R has been modified 
source(file.path(stock.dir,"src","plotscript.R"))

# some extra plotting
fit$data$surveys<-surveys  # MV addition
source(file.path(stock.dir,"src","MV_functions.R"))
CommonThings(fit)  # some initializations



setwd(proot)
file.remove(dir(pattern='png$'))
stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))

png(filename = paste("big_",stamp,"_%03d.png", sep=''), width = 1200, height = 800, 
    units = "px", pointsize = 20, bg = "white")

par(cex.lab=1, cex.axis=1, mar=c(5,5,1,1))
par(mfrow=c(2,2))

fy<-2009  # years in plot
ly<-2019


ssbplot(su[[1]], las=1, drop=1,ci=FALSE,xlim=c(fy,ly),col='black')
legend("topright",legend=my.labels,fill=c('black','blue','red'))
ssbplot(su[[2]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='blue',add=TRUE)
#ssbplot(su[[3]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='red',add=TRUE)


fbarplot(su[[1]], las=1, drop=1,ci=FALSE,xlim=c(fy,ly),col='black')
fbarplot(su[[2]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='blue',add=TRUE)
#fbarplot(su[[3]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='red',add=TRUE)


recplot(su[[1]], las=1, drop=1,ci=FALSE,xlim=c(fy,ly),col='black')
recplot(su[[2]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='blue',add=TRUE)
#recplot(su[[3]], las=0, drop=0,ci=FALSE,xlim=c(fy,ly),col='red',add=TRUE)

stampit()

dev.off()


su[[2]]
tp<-lapply(su,tableParameters)
a<-cbind(tp[[1]][,1],tp[[2]][,1])
colnames(a)<-my.labels
a

print(round(a,3),na.print = "")
xtab(a, caption=paste('Table 11. Parameter estimates','.',sep=''), cornername='Parameter \ Year', 
     file=file.path(proot,paste(stamp,'_tab11a.html',sep='')), dec=rep(3,ncol(ftab)))

a<-lapply(su, logLik)
a
