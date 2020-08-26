############################################################################################################-
# Create FLStock object
#
# Created by Benoit Berges, adapted by Esther Beukhof
############################################################################################################-

rm(list=ls())

library(FLCore)

##### Set paths
dataPath  <- "NSHM/Assessment 2020/data/commercial/"
outPath   <- "NSHM/Assessment 2020/results/commercial/"

##### Set stock and assessment info
stockkeylabel   <- "hom.27.3a4bc7d"
stockName       <- 'NSHM'
ageVec          <- 0:15
yearVec         <- 1982:2019
uniqueAreas     <- c('27.3.a','27.4.a','27.4.b','27.4.c','27.7.d')


##### Read data files
canum       <- read.table(paste0(dataPath,"canum.csv"),sep=',', 
                          header = TRUE,
                          row.names = 1,
                          check.names=FALSE)

canum_area  <- read.table(paste0(dataPath,"canum_area.csv"),sep=',', 
                          header = TRUE,
                          check.names=FALSE)

caton       <- read.table(paste0(dataPath,"caton.csv"),sep=',', 
                          header = TRUE,
                          check.names=FALSE)

weca        <- read.table(paste0(dataPath,"weca.csv"),sep=',', 
                          header = TRUE,
                          check.names=FALSE,
                          row.names = 1)

lena        <- read.table(paste0(dataPath,"lena.csv"),sep=',', 
                          header = TRUE,
                          check.names=FALSE,
                          row.names = 1)

caton_area_cat        <- read.table(paste0(dataPath,"caton_area_cat.csv"),sep=',', 
                                    header = TRUE,
                                    check.names=FALSE)

##### Create stock object

# Get a list of the dimension names of age, year, unit, season and area
dmns <- list(age = as.character(ageVec),
             year = as.character(yearVec),
             unit = 'unique',
             season = 'all',
             area = c(uniqueAreas,
                      'all'))
# Create empty FLQuant
stk <- FLQuant(array( NA,
                      dim=c(length(dmns$age),    # ages
                            length(dmns$year),   # years
                            1,
                            1,
                            6,
                            1)), # number of iterations
               dimnames=dmns)

# Get it into stock object format
stk <- FLStock(stock.n = stk,
               name = "stockName",
               desc = stockkeylabel)

# fill in total catch numbers
stk@catch.n[rownames(canum),colnames(canum),,,'all'] <- as.matrix(canum)*1e6

# fill in catch numbers per area
for(iArea in uniqueAreas){
  arrayTemp <- array(NA,dim=c(length(unique(canum_area$year)),length(unique(canum_area$age))))
  rownames(arrayTemp) <- unique(canum_area$year)
  colnames(arrayTemp) <- unique(canum_area$age)
  
  
  
  for(iYear in unique(canum_area$year)){
    arrayTemp[match(iYear,as.numeric(rownames(arrayTemp))),
              match(canum_area[canum_area$year == iYear,'age'],as.numeric(colnames(arrayTemp)))] <- canum_area[canum_area$year == iYear,colnames(canum_area) == iArea]
  }
  
  stk@catch.n[colnames(arrayTemp),rownames(arrayTemp),,,iArea] <- as.matrix(arrayTemp)
}

# fill in catch weight at age
stk@catch.wt[rownames(weca),colnames(weca),,,'all'] <- as.matrix(weca)

# fill in catch length at age - hack here, filling m for it
stk@m[rownames(lena),colnames(lena),,,'all'] <- as.matrix(lena)

# fill in catch tonnage per area
for (iArea in uniqueAreas){
  
  currentYears     <- unique(caton_area_cat[caton_area_cat$Area == iArea[1],]$Year)
  
  for(iYear in currentYears){
    stk@landings[,as.character(iYear),,,iArea] <-  sum(caton_area_cat[  caton_area_cat$Year == iYear &
                                                                          caton_area_cat$Area == iArea[1] &
                                                                          (caton_area_cat$CatchCategory == 'Landings' |
                                                                             caton_area_cat$CatchCategory == 'BMS landing'),]$caton)
    
    if(length(caton_area_cat[   caton_area_cat$Year == iYear &
                                caton_area_cat$Area == iArea[1] &
                                caton_area_cat$CatchCategory == 'Discards',]$caton) != 0){
      stk@discards[,as.character(iYear),,,iArea]   <- sum(caton_area_cat[   caton_area_cat$Year == iYear &
                                                                              caton_area_cat$Area == iArea[1] &
                                                                              caton_area_cat$CatchCategory == 'Discards',]$caton)
      
    }
    
    stk@catch[,as.character(iYear),,,iArea]   <- sum(caton_area_cat[  caton_area_cat$Year == iYear &
                                                                        caton_area_cat$Area == iArea[1],]$caton)
    
  }
}

stk@discards[,,,,'all'] <- areaSums(stk@discards)
stk@landings[,,,,'all'] <- areaSums(stk@landings)
stk@catch[,,,,'all']    <- areaSums(stk@catch)

# Save
save(stk,file=paste0(outPath,stockName,'_FLStock.RData'))
