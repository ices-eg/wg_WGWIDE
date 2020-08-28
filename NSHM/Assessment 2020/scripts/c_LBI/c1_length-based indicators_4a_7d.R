############################################################################################################-
# Length-based indicators (LBI) based on the commercial length frequency data from both 4a and 7d
#
# Only 2018 and 2019 data from both areas.
#
# Created by Alfonso Perez Rodriguez, adapted by Benoit Berges and Esther Beukhof
############################################################################################################-

rm(list=ls())

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Set paths
dataHistPath <- "NSHM/Assessment 2020/data/length/"
dataPath     <- "NSHM/Assessment 2020/results/length/"
resPath      <- "NSHM/Assessment 2020/results/length/"
figPath      <- "NSHM/Assessment 2020/figures/length/"
figAdvice    <- "NSHM/Assessment 2020/figures/advice/" 

# Settings
years      <-c(2018:2019)
title_name <- 'NSHM_7d_4a'

# Create empty array where to store results
adviceOut <- array(0,dim=c(2,0))

# Loop through all years
for (i in years){
  print(i)
  
  # Load data from year of interest
  if(i %in% years[c(1:length(years)-1)]){
    data <- read.table(paste0(dataHistPath,'InterCatch/historic/NSHM_LF_area_',as.character(i),".csv"),sep=',',header=TRUE,check.names=FALSE)
  } else{
    data <- read.table(paste0(dataPath,'/NSHM_LF_area_',as.character(i),".csv"),sep=',',header=TRUE,check.names=FALSE)
  }
  
  # Create empty data frame
  dat      <- array(NA, dim=c(dim(data)[1],3))
  colnames(dat) <- c('length','prop','order')
  dat <- as.data.frame(dat)
  
  # Combine proportions of 7.d and 4.a
  dat$prop          <- rowMeans(cbind(data[,match('27.7.d',colnames(data))],data[,match('27.4.a',colnames(data))]))
  dat$length        <- data[,match('length_class',colnames(data))]
  dat$order         <- c(1:dim(dat)[1])
  
  
  ###############################################################################################
  ###########   Now estimate all the size based indicators and reference points       ###########
  ###############################################################################################
  
  # First the size based indicators
  #Lc = length at first catch = 50% of the mode (i.e. most occurring length)
  Lmaxabu     <- dat[dat$prop==max(dat$prop),]
  halfmaxabu  <- (Lmaxabu$prop)/2
  maxabuord   <- Lmaxabu$order
  lenlowlim   <- max(dat[dat$prop<halfmaxabu & dat$order<maxabuord,"length"])
  abulowlim   <- max(dat[dat$prop<halfmaxabu & dat$order<maxabuord,"prop"])
  ordlowlim   <- max(dat[dat$prop<halfmaxabu & dat$order<maxabuord,"order"])
  lenhighlim  <- dat[dat$order==(ordlowlim+1),"length"]
  abuhighlim  <- dat[dat$order==(ordlowlim+1),"prop"]
  Lc          <- lenlowlim + ((lenhighlim-lenlowlim)/(abuhighlim-abulowlim)*(halfmaxabu-abulowlim))
  # Lmean
  dat$xn      <- dat$length*dat$prop
  Lmean       <- sum(dat[dat$length>Lc,"xn"])/sum(dat[dat$length>Lc,"prop"])
  # L95
  dat$cumnum  <- cumsum(dat$prop)
  dat$cumprop <- dat$cumnum/sum(dat$prop)
  lenlowlim   <- max(dat[dat$cumprop<0.95,"length"])
  proplowlim  <- max(dat[dat$cumprop<0.95,"cumprop"])
  ordlowlim   <- max(dat[dat$cumprop<0.95,"order"])
  lenhighlim  <- dat[dat$order==(ordlowlim+1),"length"]
  prophighlim <- dat[dat$order==(ordlowlim+1),"cumprop"]
  L95         <- lenlowlim + (((lenhighlim-lenlowlim)/(prophighlim-proplowlim))*(0.95-proplowlim))
  
  # Lsq
  Lsq         <- sum(dat[,"xn"])/sum(dat[,"prop"])
  
  # Now the size based reference points
  # Tomo los parámetro sde crecimeinto del benchmark del western horse mackerel, del reporte del WKWIDE 2017, tabla 5.2.4. El Linf lo cojo directamente. El L50 lo estimo a partir de la edad A50 que es 3.5 años y los parámetros de la ecuación de crecimiento.
  Linf <- 40
  L50  <-20.5
  
  # Lfm
  Lfm <- (3*Lc+Linf)/4
  # Lopt
  Lopt <- 2/3*Linf
  
  
  # Plott
  year=i
  
  png(filename=paste(figPath,title_name,'_',year,".png",sep=""),height=1500, width=2000, units = "px", pointsize = 7, bg = "white", res = 450, family = "", restoreConsole = TRUE)
  plot(dat$length,dat$prop,type="b", pch=19, lwd=1.2, cex=0.7, xlab="Length (cm)", ylab="Proportion", cex.lab=1.5, 
       main=paste(title_name,year,sep=" "), cex.main=1.8)
  # points(Lc,halfmaxabu,col="red",pch=19)
  # points(Lmean,halfmaxabu,col="blue",pch=19)
  # points(L95,halfmaxabu,col="green",pch=19)
  dev.off()
  
  # Calculate F/Fmsy ratio
  ratio_F_Fmsy <- Lfm/Lmean
  
  # Combine results 
  result <- data.frame(year,Lc,Lmean,Lsq,L95,Lfm,Lopt,Linf,L50,ratio_F_Fmsy)
  Lopt/Lmean
  
  # Save
  write.csv(result,paste(resPath,title_name,"_size_Based_Indicators_&_Reference_Points_",year,".csv",sep=""),row.names=F)
  
  ### PDF
  pdf(file = paste(figPath,title_name,"_size_Based_Indicators_&_Reference_Points_",year,".pdf", sep=""))
  grid.table(result, rows=NULL, cols=colnames(table), theme=ttheme_default(base_size = 5))
  dev.off()
  
  # More plotting
  maxx=max(dat$length,Lc,Lmean,Lsq,L95,Lfm,Lopt,Linf,L50)
  minx=min(dat$length,Lc,Lmean,Lsq,L95,Lfm,Lopt,Linf,L50)
  
  png(filename=paste(figPath,title_name,"_size_Based_Indicators_",year,".png",sep=""),width = 1500, height = 1200, units = "px", pointsize = 4,bg = "white", res = 450)
  
  plot(dat$length,dat$prop,type="b", pch=19, lwd=1.2, cex=0.8, xlab="Length (cm)", ylab="Proportion", cex.lab=1.5, 
       main=paste(title_name,year,sep=" "), cex.main=1.5,xlim=c(minx,maxx),ylim=c(0,max(dat$prop)+0.010),las=1,mgp=c(2.5,0.5,0))
  abline(v=Lopt,col="red3",lty=2); text(Lopt+0.5,max(dat$prop)+0.007,label="Lopt",col="red3",cex=0.8)
  abline(v=Linf,col="blue",lty=2); text(Linf+0.5,max(dat$prop)+0.003,label="Linf",col="blue",cex=0.8)
  abline(v=L50,col="blue",lty=2); text(L50+0.5,max(dat$prop)+0.005,label="Lmat",col="blue",cex=0.8)
  abline(v=Lc,col="blue",lty=2); text(Lc-0.5,max(dat$prop)+0.01,label="Lc",col="blue",cex=0.8)
  abline(v=Lmean,col="red3",lty=2); text(Lmean+0.5,max(dat$prop)+0.001,label="Lmean",col="red3",cex=0.8)
  abline(v=Lfm,col="red3",lty=2); text(Lfm+0.5,max(dat$prop)+0.005,label="Lfm",col="red3",cex=0.8)
  
  dev.off()
  
  
  png(filename=paste(figPath,title_name,"_Lmean_Lfm_",year,".png",sep=""),width = 1500, height = 1200, units = "px", pointsize = 5,bg = "white", res = 450)
  
  plot(dat$length,dat$prop,type="b", pch=19, lwd=1.2, cex=0.8, xlab="Length (cm)", ylab="Proportion", cex.lab=1.5, 
       main=paste(title_name,year,sep=" "), cex.main=1.5,xlim=c(minx,maxx),ylim=c(0,max(dat$prop)+0.010),las=1,mgp=c(2.5,0.5,0))
  abline(v=Lc,col="olivedrab",lty=2); text(Lc-0.9,max(dat$prop)-0.02,label="Lc",col="olivedrab",cex=0.8)
  abline(v=Lmean,col="red3",lty=2); text(Lmean-1.9,max(dat$prop)+0.009,label="Lmean",col="red3",cex=0.8)
  abline(v=Lfm,col="blue",lty=2); text(Lfm+1.4,max(dat$prop)+0.005,label="Lfm",col="blue",cex=0.8)
  text(15,0.1,label=paste("ratio F/Fmsy = ",round(ratio_F_Fmsy,digits=3),sep=""),col="blue",cex=1.4)
  
  dev.off()
  
  
  png(filename=paste(figAdvice,title_name,"_Lmean_Lfm_xa_advice_sheet_",year,".png",sep=""),width = 1500, height = 1200, units = "px", pointsize = 5,bg = "white", res = 450)
  
  plot(result$year,1/result$ratio_F_Fmsy,type="p", pch=19, lwd=1.2, cex=1.2, xlab="Year", ylab="Indicator ratio", 
       cex.lab=1.5, cex.main=1.5,xlim=c(year-1,year+1),ylim=c(0,result$ratio_F_Fmsy+0.8),las=1,mgp=c(2.5,0.5,0),
       col="blue", lab=c(2,5,1),bty="l",main=paste(title_name,year,sep=" "))
  abline(1,0,cex=0.8,lty=2)
  text(year+0.5,0.5,label="Lmean/LF=M",col="blue",cex=1.4)
  
  dev.off()
  
  adviceOut <- cbind(adviceOut,c(result$year,1/result$ratio_F_Fmsy))
  
}


# figure for advice
png(filename=paste(figAdvice,title_name,"_advice",".png",sep=""),width = 1500, height = 1200, units = "px", pointsize = 5,bg = "white", res = 450)

minYear <- 2016
plot(adviceOut[1,],adviceOut[2,],type="b", pch=19, lwd=1.2, cex=1.2, xlab="Year", ylab="Indicator ratio", 
     cex.lab=1.5, cex.main=1.5,xlim=c(min(adviceOut[1,])-0.5,max(adviceOut[1,])+0.5),ylim=c(0,max(adviceOut[2,])+0.8),
     las=1,mgp=c(2.5,0.5,0),col="blue", lab=c(3,1,1),bty="l")
abline(1,0,cex=0.8,lty=2)
text(minYear+1,0.5,label="Lmean/LF=M",col="blue",cex=1.4)
dev.off()