############################################################################################################-
# Calculate catch advice and prepare input data for advice sheet
#
# Created by Alfonso Perez Rodriguez, adapted by Benoit Berges and Esther Beukhof
############################################################################################################-

rm(list=ls())

# Set paths
dataLengthPath <- "NSHM/Assessment 2021/results/length/"
dataModelPath  <- "NSHM/Assessment 2021/results/model/"
dataCatchPath  <- "NSHM/Assessment 2021/data/commercial/"
dataTACPath    <- "NSHM/Assessment 2021/data/commercial/TAC/"
resPath        <- "NSHM/Assessment 2021/results/advice/"
figPath        <- "NSHM/Assessment 2021/figures/advice/"

# Settings for advice
yearAssessed    <- 2020 #the most recent year for which we have data
assessmentYear  <- yearAssessed + 1 #year in which we do the assessment
yearIndexA      <- seq(yearAssessed-1,yearAssessed)
yearIndexB      <- seq(yearAssessed-4,yearAssessed-2)

# Load length-based indicators
lengthInd <- read.table(paste0(dataLengthPath,'NSHOM_size_Based_Indicators_&_Reference_Points_',as.character(yearAssessed),'.csv'),header = TRUE,sep=',')

# Load abundance index from surveys
indexSurveys <- read.table(paste0(dataModelPath,'model_fit_adults_nshm.csv'),header = TRUE,sep=',')

# Load catch data
catchData <- read.table(paste0(dataCatchPath,'caton_area_cat.csv'),header = TRUE,sep=';')

# Load historic TAC
historic_TAC  <- read.table(paste0(dataTACPath,'hist_TAC.csv'),header = TRUE,sep=';')

# Get previous catch advice
catch         <- historic_TAC[historic_TAC$country == 'all' & historic_TAC$year == yearAssessed,]$advice_TAC



######################################### Calculate advice  #########################################

# Check if precautionary buffer has been used in previous 3 years; if no, then do apply PA buffer this time
historic_TAC_subset   <- historic_TAC[historic_TAC$country == 'all',]
historic_TAC_subset   <- historic_TAC_subset[historic_TAC_subset$cap != 'no advice',]
historic_TAC_subset   <- historic_TAC_subset[assessmentYear-3 < historic_TAC_subset$year & historic_TAC_subset$year < assessmentYear,]
flagBufferHistoric    <- !(1 %in% historic_TAC_subset$buffer)

# Get '2-over-3' or 'AB' ratio of the survey index
indexA  <- mean(indexSurveys$all[match(yearIndexA,indexSurveys$year)], na.rm=TRUE)
indexB  <- mean(indexSurveys$all[match(yearIndexB,indexSurveys$year)], na.rm=TRUE)
ratioAB <- indexA/indexB

# Check if uncertainty cap should be applied: 
# - if index decreased more than 20%, than cap = 0.8
# - if index increased more than 20%, than cap = 1.2
if(ratioAB > 1.2){
  cap <- 1.2
}else if(ratioAB < 0.8){
  cap <- 0.8
}else{
  cap <- ratioAB #note sure if this is right!
}

if(!cap == 1){
  capApplied = TRUE
}else{
  capApplied = FALSE
}

# Should buffer be applied?
if(lengthInd$ratio_F_Fmsy < 1 & flagBufferHistoric){
  buffer <- 0.8
}else{
  buffer <- 1
}

if(!buffer == 1){
  bufferApplied = TRUE
}else{
  bufferApplied = FALSE
}

# Calculate catch advice
catchAdvice <- round(catch*cap*buffer,0)


# Calculate annual discard rates and three-year rolling average
discards <- subset(catchData,CatchCategory == 'Discards')

uniqueYears <- unique(discards$Year)

outArray      <- array(NA, dim=c(length(uniqueYears),2))
outArray[,1]  <- uniqueYears

for(i in uniqueYears){
  outArray[outArray[,1] == i,2] <- sum(discards$caton[discards$Year == i])/sum(catchData$caton[catchData$Year == i])
}
discardRate  <- mean(outArray[c((nrow(outArray)-2):nrow(outArray)),2]) #three-year rolling average

colnames(outArray) <- c("Year","discardRate")
write.csv(outArray,"NSHM/Assessment 2021/results/commercial/discardRate.csv", row.names = FALSE)

# Use three-year rolling average of discard rate to calculate projected landings corresponding to advice
projectedLandings <- round(catchAdvice*(1-discardRate),0)

# Advice change
adviceChange <- round(catchAdvice/catch*100-100,1)

# Summarize the advice output
outArray <- array(NA,dim=c(1,13))
colnames(outArray) <- c('indexA',
                        'indexB',
                        'indexRatioAB',
                        'capApplied',
                        'cap',
                        'advicedCatch',
                        'discardRate',
                        'ratio_F_FMSY',
                        'bufferApplied',
                        'buffer',
                        'catchAdvice',
                        'projectedLandings',
                        'adviceChange')
outArray[1]     <- indexA
outArray[2]     <- indexB
outArray[3]     <- ratioAB
outArray[4]     <- capApplied
outArray[5]     <- cap
outArray[6]     <- catch
outArray[7]     <- discardRate
outArray[8]     <- lengthInd$ratio_F_Fmsy
outArray[9]     <- bufferApplied
outArray[10]    <- buffer
outArray[11]    <- catchAdvice
outArray[12]    <- projectedLandings
outArray[13]    <- adviceChange

write.table(outArray,file = paste0(resPath,'advice.csv'),sep=',',col.names=TRUE, row.names = FALSE)



######################################### Plot 2 over 3 rule  #########################################

png(paste0(figPath,"ratio_calculation.png"),width = 1600, height = 1200, units = "px", pointsize = 5,bg = "white", res = 450)
par(mar=c(5,5,3,1))

plot(indexSurveys$year,indexSurveys$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",
     cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

lines(indexSurveys$year[match(yearIndexA,indexSurveys$year)],
      indexSurveys$all[match(yearIndexA,indexSurveys$year)],
      type="b",pch=19,col="blue",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

lines(indexSurveys$year[match(yearIndexB,indexSurveys$year)],
      indexSurveys$all[match(yearIndexB,indexSurveys$year)],
      type="b",pch=19,col="red",cex=1,lwd=1.5,xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")

dev.off()


######################################### Plot discard time  series  #########################################

# windows()
# 
# plot(outArray[,1],outArray[,2],type="b",pch=19,col="blue",cex=1,lwd=1.5,xlab="Year",ylab="discard rate",cex.axis=1.5, cex.lab=1.7,
#      main=list("historical discard records",cex=2),bty="l",xlim = c(2008,yearAssessed))
# 
# savePlot(paste0(figPath,"discard_rate.png"))


