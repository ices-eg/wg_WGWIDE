# Combine length frequency  (LF) data from multiple countries and quarters by area
# First prepare French LF data
#
# This is an update of the 2019 assessment. French data are re-calculated and kept in numbers per length.
# Length frequency data from multiple countries in the same area are combined in a way previously done (in 2018 and 2017).

library(tidyverse)

# Load French data
FRA_LF <- read.csv("NSHM/Assessment 2019/data/LF/FRA_length_composition_InterCatch.csv", sep=";", stringsAsFactors = FALSE)

# Aggregate numbers by length class and store
FRA_LF_2020         <- aggregate(NumberCaught ~ Length + Season + FishingArea, FRA_LF, sum)
FRA_LF_2020$Year    <- 2020
FRA_LF_2020$Country <- "France"
write.csv(FRA_LF_2020, file="NSHM/Assessment 2019/data/LF/FRA_length_composition_aggregated.csv", row.names = FALSE)

## These data are replacing the old French data in the LF_all.csv file. This has been done manually.

# Load the LF data of all countries combined (including new version of French data)
LF_tab        <- read.table('NSHM/Assessment 2019/data/LF/LF_all.csv',sep=';',header=TRUE)
length_vec    <- 1:60 # length vector

# Sum numbers per length across countries per area and make relative
for(iYear in unique(LF_tab$year)){
  LF_tab_sub         <- subset(LF_tab,year == iYear)
  
  LF_Array           <- array(NA, dim=c(length(length_vec),length(unique(LF_tab_sub$area))+1))
  colnames(LF_Array) <- c('length_class',as.character(unique(LF_tab_sub$area)))
  LF_Array[,1]       <- length_vec
  
  for(iArea in unique(LF_tab_sub$area)){
    
    # Subset to area
    tempTab   <- subset(LF_tab_sub,area == iArea)
    tempTab   <- tempTab[order(tempTab$length_class),]
    
    # Sum numbers per length classes from multiple countries and quarters
    tempTab   <- aggregate(value ~ length_class + area + quarter + year, tempTab, FUN=sum)
    
    # Make numbers relative
    tempTabFull   <- array(0, dim=c(length(length_vec),1))
    tempTabFull[match(tempTab$length_class,length_vec)] <- tempTab$value
    tempTabFull   <- tempTabFull/sum(tempTabFull)
    
    #Add back to main data 
    LF_Array[,iArea] <- tempTabFull[,1]
  }
  
  write.table(LF_Array,file = paste0('NSHM/Assessment 2019/data/LF/NSHM_LF_area_',as.character(iYear),'.csv'),sep = ',',row.names = FALSE)
} 
