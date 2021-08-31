rm(list=ls())

library(icesSAG)  # devtools::install_github("ices-tools-prod/icesSAG")
library(tidyverse)

# Set paths
resultModelPath   <- "NSHM/Assessment 2021/results/model/"
resultLengthPath  <- "NSHM/Assessment 2021/results/length/"
resultsAdvicePath <- "NSHM/Assessment 2021/results/advice/"
dataPath          <- "NSHM/Assessment 2021/data/commercial/"

# Set assessed year
yearAssessed <- 2020

# Load data
indexSurveys_exploitable  <- read.table(paste0(resultModelPath,'adult_surv_idx.csv'),header = TRUE,sep=',')
indexSurveys_juvenile     <- read.table(paste0(resultModelPath,'juv_surv_idx.csv'),header = TRUE,sep=',')

catchData <- read.table(paste0(dataPath,'caton_area_cat.csv'),header = TRUE,sep=';')

lengthInd <- read.table(paste0(resultLengthPath,'NSHOM_size_Based_Indicators_&_Reference_Points_2016-',as.character(yearAssessed),'.csv'),header = TRUE,sep=',')


options(icesSAG.use_token = TRUE)

# Meta information
stockkeylabel  <- "hom.27.3a4bc7d"
assessmentyear <- 2021
contactperson  <- "esther.beukhof@wur.nl"

# Create the input data for uploading 
info     <- stockInfo(
  StockCode      = stockkeylabel, 
  AssessmentYear = assessmentyear, 
  ContactPerson  = contactperson)


info$StockCategory              <- "3.2"
info$CatchesLandingsUnits       <- 't'
info$StockSizeDescription       <- 'Abundance Index'
info$StockSizeUnits             <- 'NE3'
info$FishingPressureDescription <- "F/Fmsy"
info$FishingPressureUnits       <- "ratio"
info$CustomRefPointName         <- "F/Fmsy ratio"
info$CustomRefPointValue        <- "≥1"
info$CustomRefPointNotes        <- "Obtained with Length Based Methods"
info$CustomName1                <- "Mean"
info$CustomName2                <- "CI_low"
info$CustomName3                <- "CI_high"
info$ConfidenceIntervalDefinition <- '95%'

# Create the fish data

uniqueYear <- unique(catchData$Year)

outArray            <- array(NA,dim=c(length(uniqueYear),3))
rownames(outArray)  <- uniqueYear
colnames(outArray)  <- c('catches','landings','discards')

for(i in uniqueYear){
  outArray[rownames(outArray) == as.character(i),'catches']   <- sum(catchData$caton[   catchData$Year == i])
  outArray[rownames(outArray) == as.character(i),'landings']  <- sum(catchData$caton[   catchData$Year == i & 
                                                                                          (catchData$CatchCategory %in% 'Landings' |
                                                                                          catchData$CatchCategory %in% 'BMS landing')])
  outArray[rownames(outArray) == as.character(i),'discards']  <- sum(catchData$caton[   catchData$Year == i & 
                                                                                          catchData$CatchCategory == 'Discards'])
}

fishdata            <- stockFishdata(min(uniqueYear):max(uniqueYear))

# commercial data
fishdata$Catches    <- outArray[,'catches']
fishdata$Landings   <- outArray[,'landings']
fishdata$Discards   <- outArray[,'discards']

# time series exploitable stock
fishdata$Low_StockSize[match(indexSurveys_exploitable$year,fishdata$Year)]    <- indexSurveys_exploitable$all_low/1000
fishdata$StockSize[match(indexSurveys_exploitable$year,fishdata$Year)]        <- indexSurveys_exploitable$all/1000
fishdata$High_StockSize[match(indexSurveys_exploitable$year,fishdata$Year)]   <- indexSurveys_exploitable$all_high/1000

# time series juveniles
fishdata$CustomSeries1[match(indexSurveys_juvenile$year,fishdata$Year)]    <- indexSurveys_juvenile$all_low/1000
fishdata$CustomSeries2[match(indexSurveys_juvenile$year,fishdata$Year)]    <- indexSurveys_juvenile$all/1000
fishdata$CustomSeries3[match(indexSurveys_juvenile$year,fishdata$Year)]    <- indexSurveys_juvenile$all_high/1000

# F/Fmsy proxy
fishdata$FishingPressure[match(lengthInd$year,fishdata$Year)] <- lengthInd$ratio_F_Fmsy

key <- icesSAG::uploadStock(info, fishdata)

# Save fishdata
write.csv(fishdata, paste0(resultsAdvicePath,"summary_table_assessment_",assessmentyear,".csv"), row.names = FALSE)
