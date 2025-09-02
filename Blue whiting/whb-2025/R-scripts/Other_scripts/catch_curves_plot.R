data_source<-c('catch','survey')[1]
survey_n<-1  #survey number to be plotted  

##  No used yet
# get the assessment result (stock N) to be used in the graphs
load(file.path(stock.dir,"run",'model.Rdata'),verbose=TRUE)
a<-ntable(fit)
N<-data.frame(v=as.vector(a),year=as.numeric(dimnames(a)[[1]]),age=rep(as.numeric(dimnames(a)[[2]]),each=dim(a)[[1]]))
N$cohort<-N$year-N$age  

cat(stock.dir,'\n')
setwd(stock.dir)

source('src/dataplot.R')
setwd('data')

if (data_source=='catch') catch.no<-read.ices('cn.dat') else  {
  catch.no<-read.ices('survey.dat') 
  catch.no<-catch.no[[1]]
}
  
land.mean.weight<-read.ices('lw.dat')
setwd('..');
plotit('cn.dat')
plotit('cw.dat')

# Catch weight
if (data_source=='catch')  {
  cw<-apply(catch.no *land.mean.weight,1,sum)
  cw<-data.frame(Year=names(cw),Catch=cw/1000)
  cw<-subset(cw,Year>="2000")
  X11()
  ggplot(data=cw, aes(x=Year, y=Catch)) +
   geom_bar(stat="identity",fill='blue')
}

# Catch proportion at age
a<-catch.no/rowSums(catch.no)
a<-data.frame(v=as.vector(a),year=as.numeric(dimnames(a)[[1]]),age=rep(as.numeric(dimnames(a)[[2]]),each=dim(a)[[1]]))
op<-par(cex.lab=1.5, cex.axis=1.5, mar=c(4,5,1,1),mfcol=c(1,1))

bpLog<-function(x,y,v, scale=3, ...){
  plot(x,y,cex=sqrt(abs(v))*scale, col=ifelse(v<0,'tomato2','blue'), pch=ifelse(v<0,16,1), ...)
  points(x[v>0],y[v>0],cex=log10(v[v>0])*scale, col='blue', pch=1, ...)
}

png(filename=file.path('data',paste0(data_source,'_proportion.png')),width=1000,height=600)
bpLog(x=a$year,y=a$age,v=a$v,scale=12,xlab='Year',ylab='Age')
#par(op)
#stampit();setcap("Catch proportion", "Catch proportion at age")
dev.off()


# catch curves
a<-catch.no
a<-data.frame(v=as.vector(a),year=as.numeric(dimnames(a)[[1]]),age=rep(as.numeric(dimnames(a)[[2]]),each=dim(a)[[1]]))
#subset(a,year==2015)
a$cohort<-a$year-a$age  



if (FALSE) { # first step towards a ggplot version of catch curve
  N$source<-'stock N'
  if (data_source=='catch') a$source<-'catch' else a$source<-'survey'
  
  # a<-rbind(a,N)
  
  ggplot( aes(age,log(v)),data=a) + geom_point()+
    geom_abline(intercept = seq(6,20,2),slope=b)
}

library(lattice) 
trellis.device(device='png',filename=file.path('data',paste0(data_source,'_curves.png')),width=1000,height=600)


ttl <- list(label="", cex=1.5)
yttl <- list(label="log(catch numbers)", cex=1.0)
xttl <- list(cex=1.0)
stripttl <- list(cex=1.0)
ax <- list(cex=0.7)
b<--0.6    # slope

if (data_source=='catch') {
  print(xyplot(log(v)~age| factor(cohort), data=a,  subset=(cohort>1985 & cohort<2021),  #          subset=(cohort>1981 & cohort<2017),
       main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, layout = c(7, 5),
       panel = function(x, y) {
         panel.xyplot(x, y,col="blue", type="l",lwd=2)
         panel.abline(a =10, b=b, lwd=0.7, lty=3, col='grey')  # der må være en nemmere måde
         panel.abline(a =12, b=b, lwd=0.7, lty=3, col='grey')
         panel.abline(a =14, b=b, lwd=0.7, lty=3, col='grey')
         panel.abline(a =16, b=b, lwd=0.7, lty=3, col='grey')
         panel.abline(a =18, b=b, lwd=0.7, lty=3, col='grey')
         panel.abline(a =20, b=b, lwd=0.7, lty=3, col='grey')
         panel.abline(a =22, b=b, lwd=0.7, lty=3, col='grey')
         panel.abline(a =24, b=b, lwd=0.7, lty=3, col='grey')
       }
  )) 
} else {
  print(xyplot(log(v)~age| factor(cohort), data=a, subset=(cohort>2003 & cohort<2019),
               main=ttl, ylab=yttl, xlab=xttl, scales=ax, par.strip.text=stripttl, layout = c(4, 4),
               panel = function(x, y) {
                 panel.xyplot(x, y,col="blue", type="l",lwd=2)
                 panel.abline(a =10, b=b, lwd=0.7, lty=3, col='grey')  # der må være en nemmere måde
                 panel.abline(a =12, b=b, lwd=0.7, lty=3, col='grey')
                 panel.abline(a =14, b=b, lwd=0.7, lty=3, col='grey')
                 panel.abline(a =16, b=b, lwd=0.7, lty=3, col='grey')
                 panel.abline(a =18, b=b, lwd=0.7, lty=3, col='grey')
                 panel.abline(a =20, b=b, lwd=0.7, lty=3, col='grey')
                 panel.abline(a =22, b=b, lwd=0.7, lty=3, col='grey')
                 panel.abline(a =24, b=b, lwd=0.7, lty=3, col='grey')
               }
  )) 
  
  
  
  
}
dev.off()


# Return to original working directory
setwd(orig_wd)