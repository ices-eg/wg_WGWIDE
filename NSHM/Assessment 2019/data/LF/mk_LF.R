rm(list=ls())

library(tidyverse)

inpath="C:/Users/berge057/OneDrive - WageningenUR/projects/WG/2019_WGWIDE/2019 assessment/NSHM/data/commercial_data/data_xa_analysis/length_frequency_data/"

LF_tab        <- read.table(paste0(inpath,'LF_all.csv'),sep=',',header=TRUE)
length_vec    <- 1:60 # length vector

for(iYear in unique(LF_tab$year)){
  LF_tab_sub <- subset(LF_tab,year == iYear)
  
  LF_Array      <- array(NA, dim=c(length(length_vec),length(unique(LF_tab_sub$area))+1))
  colnames(LF_Array) <- c('length_class',as.character(unique(LF_tab_sub$area)))
  LF_Array[,1] <- length_vec
  
  for(iArea in unique(LF_tab_sub$area)){
    # initialize array to store all distributions
    tempArray <- array(NA, dim=c(length(length_vec),0))
    
    uniqueQuarter <- unique(LF_tab_sub[LF_tab_sub$area == iArea,]$quarter)
    for(iQuarter in uniqueQuarter){
      
      uniqueCountry <- unique(LF_tab_sub[LF_tab_sub$area == iArea & LF_tab_sub$quarter == iQuarter,]$country)
      for(iCountry in uniqueCountry){
        tempTab       <- subset(LF_tab_sub,area == iArea & quarter == iQuarter & country == iCountry)
        tempTab       <- tempTab[order(tempTab$length_class),]
        
        tempTabFull   <- array(0, dim=c(length(length_vec),1))
        tempTabFull[match(tempTab$length_class,length_vec)] <- tempTab$value
        tempTabFull <- tempTabFull/max(tempTabFull)
        
        tempArray   <- cbind(tempArray,tempTabFull)
      }
    }
    
    # taking the mean here as we don't know numbers for all cases
    LF_Array[,iArea] <- apply(tempArray, 1, FUN = mean)
    LF_Array[,iArea] <- LF_Array[,iArea]/sum(LF_Array[,iArea])
    
    write.table(LF_Array,file = paste0(inpath,'NSHM_LF_area_',as.character(iYear),'.csv'),sep = ',',row.names = FALSE)
  }
}