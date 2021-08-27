############################################################################################################-
# Prepare lenght frequency data of InterCatch and the PFA
#
# Length distributions per country, quarter and area will be summed by area, and proportional length
# distributions per area will be calculated.
# Input values are numbers in thousands. Data were gathered from the InterCatch files provided by each
# country and the PFA.
#
# Created by Benoit Berges, adapted by Esther Beukhof
############################################################################################################-

dataPath <- "NSHM/Assessment 2021/data/length/"
resPath  <- "NSHM/Assessment 2021/results/length/"

########################## InterCatch length frequencies ########################## 

LF_tab        <- read.table(paste0(dataPath,'LF_all_2020.csv'),sep=';',header=TRUE)
length_vec    <- 1:60 # length vector

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

  write.table(LF_Array,file = paste0(resPath,'NSHM_LF_area_',as.character(iYear),'.csv'),sep = ',',row.names = FALSE)
} 


########################## PFA length frequencies ########################## 

LF_tab        <- read.table(paste0(dataPath,'PFA/LF_all_PFA.csv'),sep=',',header=TRUE)
LF_tab        <- subset(LF_tab, year == 2019)
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
        
        tempArray   <- cbind(tempArray,tempTabFull)
      }
    }
    
        # summing across quarter and country as here we know numbers
    LF_Array[,iArea] <- rowSums(tempArray)
    LF_Array[,iArea] <- LF_Array[,iArea]/sum(LF_Array[,iArea])
    
    write.table(LF_Array,file = paste0(resPath,'NSHM_LF_area_PFA_',as.character(iYear),'.csv'),sep = ',',row.names = FALSE)
  }
}
