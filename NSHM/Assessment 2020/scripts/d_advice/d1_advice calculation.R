############################################################################################################-
# NO ADVICE THIS YEAR! So not necessary to run this code
#
# Created by Alfonso Perez Rodriguez, adapted by Benoit Berges and Esther Beukhof
############################################################################################################-

rm(list=ls())

# Set paths
dataLengthPath <- "NSHM/Assessment 2020/results/length/"
dataModelPath  <- "NSHM/Assessment 2020/results/model/"
dataCatchPath  <- "NSHM/Assessment 2020/data/commercial/"
dataTACPath    <- "NSHM/Assessment 2020/data/commercial/TAC/"
resPath        <- "NSHM/Assessment 2020/results/advice/"
figPath        <- "NSHM/Assessment 2020/figures/advice/"

# Settings for advice
assessmentYear  <- 2019
adviceYear      <- assessmentYear + 1
yearIndexA      <- seq(assessmentYear-1,assessmentYear)
yearIndexB      <- seq(assessmentYear-4,assessmentYear-2)

# Load length-based indicators
lengthInd <- read.table(paste0(dataLengthPath,'NSHM_size_Based_Indicators_&_Reference_Points_',as.character(assessmentYear),'.csv'),header = TRUE,sep=',')

# Load abundance index from surveys
indexSurveys <- read.table(paste0(dataModelPath,'model_fit_adults_nshm.csv'),header = TRUE,sep=',')

# Load catch data
catchData <- read.table(paste0(dataCatchPath,'caton_area_cat.csv'),header = TRUE,sep=',')

# Load historic TAC
historic_TAC  <- read.table(paste0(dataTACPath,'hist_TAC.csv'),header = TRUE,sep=',')

# Get previous advice TAC
catch         <- historic_TAC[historic_TAC$country == 'all' & historic_TAC$year == adviceYear,]$advice_TAC



######################################### Calculate advice  #########################################

# Check if precautionary buffer has been used in previous 3 years; if yes, then do not apply buffer this time
historic_TAC_subset   <- historic_TAC[historic_TAC$country == 'all',]
historic_TAC_subset   <- historic_TAC_subset[historic_TAC_subset$cap != 'no advice',]
historic_TAC_subset   <- historic_TAC_subset[adviceYear-3 < historic_TAC_subset$year & historic_TAC_subset$year < adviceYear,]
flagBufferHistoric    <- !(1 %in% historic_TAC_subset$buffer)

# Get '2-over-3' or 'AB' ratio of the survey index
indexA <- mean(indexSurveys$all[match(yearIndexA,indexSurveys$year)])
indexB <- mean(indexSurveys$all[match(yearIndexB,indexSurveys$year)])
ratioAB <- indexA/indexB

# Check if uncertainty cap should be applied: 
# - if index decreased more than 20%, than cap = 0.8
# - if index increased more than 20%, than cap = 1.2
if(ratioAB > 1.2){
  cap <- 1.2
}else if(ratioAB < 0.8){
  cap <- 0.8
}else{
  cap <- ratioAB
}

# Should buffer be applied?
if(lengthInd$ratio_F_Fmsy > 1 & flagBufferHistoric){
  buffer <- 0.8
}else{
  buffer <- 1
}

# Calculate catch advice
catchAdvice <- catch*cap*buffer


# Calculate discard rate and unwanted catch advice ('discard advice')
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

colnames(outArray) <- c("Year","discardRate")
write.csv(outArray,"NSHM/Assessment 2020/results/commercial/discardRate.csv", row.names = FALSE)

discardAdvice   <- catchAdvice*discardRate
landingsAdvice  <- catchAdvice - catchAdvice*discardRate

# Summarize the advice output
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

write.table(outArray,file = paste0(resPath,'advice.csv'),sep=',',col.names=TRUE, row.names = FALSE)



######################################### Plot 2 over 3 rule  #########################################

windows()

plot(indexSurveys$year,indexSurveys$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",
     cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

lines(indexSurveys$year[match(yearIndexA,indexSurveys$year)],
      indexSurveys$all[match(yearIndexA,indexSurveys$year)],
      type="b",pch=19,col="blue",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

lines(indexSurveys$year[match(yearIndexB,indexSurveys$year)],
      indexSurveys$all[match(yearIndexB,indexSurveys$year)],
      type="b",pch=19,col="red",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

savePlot(paste0(figPath,"ratio_calculation"),type = "png")



######################################### Plot discard time  series  #########################################

# windows()
# 
# plot(outArray[,1],outArray[,2],type="b",pch=19,col="blue",cex=1,lwd=1.5,xlab="Year",ylab="discard rate",cex.axis=1.5, cex.lab=1.7,
#      main=list("historical discard records",cex=2),bty="l",xlim = c(2008,assessmentYear))
# 
# savePlot(paste0(figPath,"discard_rate.png"))


