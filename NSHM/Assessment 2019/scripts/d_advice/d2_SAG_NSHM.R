rm(list=ls())

library(icesSAG)  # devtools::install_github("ices-tools-prod/icesSAG")
library(tidyverse)

# Set paths
resultPath  <- "NSHM/Assessment 2019/results/model/"
dataPath    <- "NSHM/Assessment 2019/data/"

indexSurveys_exploitable  <- read.table(paste0(resultPath,'model_fit_large_nshm.csv'),header = TRUE,sep=',')
indexSurveys_juvenile     <- read.table(paste0(resultPath,'model_fit_small_nshm.csv'),header = TRUE,sep=',') # needs updating

catchData <- read.table(paste0(dataPath,'caton_area_cat.csv'),header = TRUE,sep=',')

options(icesSAG.use_token = TRUE)

# Meta information
stockkeylabel  <- "hom.27.3a4bc7d"
assessmentyear <- 2019
contactperson  <- "esther.beukhof@wur.nl"

# Create the input data for uploading 
info     <- stockInfo(
  StockCode      = stockkeylabel, 
  AssessmentYear = assessmentyear, 
  ContactPerson  = contactperson)


info$StockCategory            <- "3"
info$CatchesLandingsUnits     <- 't'
info$StockSizeDescription     <- 'Abundance Index'
info$StockSizeUnits           <- 't'
info$CustomRefPointName       <- "F/Fmsy ratio"
info$CustomRefPointValue      <- "model catch"
info$CustomRefPointNotes      <- "Obtained with Length Based Methods"
info$CustomName1              <- "Mean"
info$CustomName2              <- "CI_low"
info$CustomName3              <- "CI_high"
info$ConfidenceIntervalDefinition <- 'test'

# Create the fish data

uniqueYear <- unique(catchData$Year)

outArray            <- array(NA,dim=c(length(uniqueYear),3))
rownames(outArray)  <- uniqueYear
colnames(outArray)  <- c('catches','landings','discards')

for(i in uniqueYear){
  outArray[rownames(outArray) == as.character(i),'catches']   <- sum(catchData$caton[   catchData$Year == i])
  outArray[rownames(outArray) == as.character(i),'landings']  <- sum(catchData$caton[   catchData$Year == i & 
                                                                                        catchData$CatchCategory == 'Landings' &
                                                                                        catchData$CatchCategory == 'BMS landing'])
  outArray[rownames(outArray) == as.character(i),'discards']  <- sum(catchData$caton[   catchData$Year == i & 
                                                                                        catchData$CatchCategory == 'Discards'])
}

fishdata            <- stockFishdata(min(uniqueYear):max(uniqueYear))

# commercial data
fishdata$Catches    <- outArray[,'catches']
fishdata$Landings   <- outArray[,'landings']
fishdata$Discards   <- outArray[,'discards']

# time series exploitable stock
fishdata$Low_StockSize[match(indexSurveys_exploitable$year,fishdata$Year)]    <- indexSurveys_exploitable$all/1000
fishdata$StockSize[match(indexSurveys_exploitable$year,fishdata$Year)]        <- indexSurveys_exploitable$all/1000
fishdata$High_StockSize[match(indexSurveys_exploitable$year,fishdata$Year)]   <- indexSurveys_exploitable$all/1000

# time series juveniles
fishdata$CustomSeries1[match(indexSurveys_juvenile$year,fishdata$Year)]    <- indexSurveys_juvenile$all/1000
fishdata$CustomSeries2[match(indexSurveys_juvenile$year,fishdata$Year)]    <- indexSurveys_juvenile$all/1000
fishdata$CustomSeries3[match(indexSurveys_juvenile$year,fishdata$Year)]    <- indexSurveys_juvenile$all/1000

key <- icesSAG::uploadStock(info, fishdata)
