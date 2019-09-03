rm(list=ls())

library(tidyverse)

inpath="C:/Users/berge057/OneDrive - WageningenUR/projects/WG/2019_WGWIDE/2019 assessment/NSHM/data/commercial_data/data_xa_analysis/length_frequency_data/LF_2018_WGWIDE_2019/"

LF_tab        <- read.table(paste0(inpath,'LF_2018_PFA.csv'),sep=',',header=TRUE)

areaUnique    <- as.character(unique(LF_tab$area))
orderUnique   <- as.numeric(unique(LF_tab$order))

length_vec    <- subset(LF_tab,order == orderUnique[1])$length

LF_Array      <- array(NA, dim=c(length(length_vec),length(areaUnique)+1))
colnames(LF_Array) <- c('length_class',areaUnique)

LF_Array[,1] <- length_vec

for(iArea in areaUnique){
  LF_current        <- subset(LF_tab,area == iArea)
  
  orderUnique <- as.numeric(unique(LF_current$order))
  
  tempArray <- array(NA, dim=c(length(length_vec),0))
  
  for(iOrder in orderUnique){
    tempVec     <- subset(LF_current,order == iOrder)$value/max(subset(LF_current,order == iOrder)$value)
    tempArray   <- cbind(tempArray,tempVec)
  }
  
  LF_Array[,iArea] <- apply(tempArray, 1, FUN = mean)
  LF_Array[,iArea] <- LF_Array[,iArea]/sum(LF_Array[,iArea])
}

write.table(LF_Array,file = paste0(inpath,'NSHM_LF_area_2018_PFA.csv'),sep = ',',row.names = FALSE)

