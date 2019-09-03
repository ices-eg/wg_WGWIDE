rm(list=ls())

resultPath  <- "C:/git/wg_WGWIDE/NSHM/results/"
dataPath    <- "C:/git/wg_WGWIDE/NSHM/data/"
figPath     <- "C:/git/wg_WGWIDE/NSHM/figures/advice/"

figtype   <- "png"

# settings for 2019 advice
assessmentYear  <- 2018
adviceYear      <- assessmentYear + 1
yearIndexA      <- seq(assessmentYear-1,assessmentYear)
yearIndexB      <- seq(assessmentYear-4,assessmentYear-2)

# settings for 2017 advice
#assessmentYear  <- 2016
#catch           <- 18247
#yearIndexA      <- c(2015,2016)
#yearIndexB      <- c(2012,2013,2014)

lengthInd <- read.table(paste0(resultPath,'NSHM_size_Based_Indicators_&_Reference_Points_',as.character(assessmentYear),'.csv'),header = TRUE,sep=',')

indexSurveys <- read.table(paste0(resultPath,'model_fit_large_nshm.csv'),header = TRUE,sep=',')
#indexSurveys <- read.table(paste0(modelPath,'model_fit_large_nshm_alfonso.csv'),header = TRUE,sep=',')

catchData <- read.table(paste0(dataPath,'caton_area_cat.csv'),header = TRUE,sep=',')

historic_TAC  <- read.table(paste0(dataPath,'hist_TAC.csv'),header = TRUE,sep=',')

catch         <- historic_TAC[historic_TAC$country == 'all' & historic_TAC$year == adviceYear,]$advice_TAC

historic_TAC_subset   <- historic_TAC[historic_TAC$country == 'all',]
historic_TAC_subset   <- historic_TAC_subset[historic_TAC_subset$cap != 'no advice',]
historic_TAC_subset   <- historic_TAC_subset[adviceYear-3 < historic_TAC_subset$year & historic_TAC_subset$year < adviceYear,]
flagBufferHistoric    <- !(1 %in% historic_TAC_subset$buffer)

# catch advice calculation
indexA <- mean(indexSurveys$all[match(yearIndexA,indexSurveys$year)])
indexB <- mean(indexSurveys$all[match(yearIndexB,indexSurveys$year)])

ratioAB <- indexA/indexB

if(ratioAB > 1.2){
  cap <- 1.2
}else if(ratioAB < 0.8){
  cap <- 0.8
}else{
  cap <- ratioAB
}

if(lengthInd$ratio_F_Fmsy > 1 & flagBufferHistoric){
  buffer <- 0.8
}else{
  buffer <- 1
}

catchAdvice <- catch*cap*buffer

# discard rate
#discards <- subset(catchData,Year == yearIndexA[1] | Year == yearIndexA[2])
discards <- subset(catchData,CatchCategory == 'Discards')

uniqueYears <- unique(discards$Year)

outArray      <- array(NA, dim=c(length(uniqueYears),2))
outArray[,1]  <- uniqueYears

for(i in uniqueYears){
  outArray[outArray[,1] == i,2] <- sum(discards$caton[discards$Year == i])/sum(catchData$caton[catchData$Year == i])*100
}




discardRate1 <- sum(discards$caton[discards$Year == yearIndexA[1]])/sum(catchData$caton[catchData$Year == yearIndexA[1]])
discardRate2 <- sum(discards$caton[discards$Year == yearIndexA[2]])/sum(catchData$caton[catchData$Year == yearIndexA[2]])

discardRate <- mean(c(discardRate1,discardRate2))

discardAdvice   <- catchAdvice*discardRate
landingsAdvice  <- catchAdvice - catchAdvice*discardRate

outArray <- array(NA,dim=c(1,10))
colnames(outArray) <- c('catchAdvice',
                        'discardAdvice',
                        'landingsAdvice',
                        'averageDiscardRate',
                        paste0('discardRate_',as.numeric(yearIndexA[1])),
                        paste0('discardRate_',as.numeric(yearIndexA[2])),
                        'cap',
                        'buffer',
                        'ratio_F_Fmsy',
                        'flagBufferHistoric')
outArray[1]     <- catchAdvice
outArray[2]     <- discardAdvice
outArray[3]     <- landingsAdvice
outArray[4]     <- discardRate
outArray[5]     <- discardRate1
outArray[6]     <- discardRate2
outArray[7]     <- cap
outArray[8]     <- buffer
outArray[9]     <- lengthInd$ratio_F_Fmsy
outArray[10]    <- flagBufferHistoric

write.table(outArray,file = paste0(resultPath,'advice.csv'),sep=',',col.names=TRUE, row.names = FALSE)


######################################### plot discard time  series  #########################################

windows()

plot(outArray[,1],outArray[,2],type="b",pch=19,col="blue",cex=1,lwd=1.5,xlab="Year",ylab="discard rate",cex.axis=1.5, cex.lab=1.7,main=list("historical discard records",cex=2),bty="l",xlim = c(2008,assessmentYear))

savePlot(paste0(figPath,"discard_rate"),type=figtype)


######################################### plot 2 over 3 rule  #########################################

plot(indexSurveys$year,indexSurveys$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

lines(indexSurveys$year[match(yearIndexA,indexSurveys$year)],
      indexSurveys$all[match(yearIndexA,indexSurveys$year)],
      type="b",pch=19,col="blue",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

lines(indexSurveys$year[match(yearIndexB,indexSurveys$year)],
      indexSurveys$all[match(yearIndexB,indexSurveys$year)],
      type="b",pch=19,col="red",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

savePlot(paste0(figPath,"ratio_calculation"),type=figtype)

