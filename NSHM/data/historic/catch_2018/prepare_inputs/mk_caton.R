rm(list=ls())

library(tidyverse)

inpath="C:/Users/berge057/OneDrive - WageningenUR/projects/WG/2019_WGWIDE/2019 assessment/NSHM/data/commercial_data/2019/hom.27.3a4bc7d_all_ 2019-8-13 13_47_53/sub_tables/"

catch_raw_tab <- read.table(paste0(inpath,'NSHM_catch_table.csv'),sep=',',header=TRUE)
catch_raw_tab <- subset(catch_raw_tab,AgeOrLength == 0)
catch_raw_tab <- catch_raw_tab[,c(1:16,23:27)]

areaUnique <- as.character(unique(catch_raw_tab$Area))
catchCategoryUniqueAll <- as.character(unique(catch_raw_tab$CatchCategory))

outArray <- array(NA, dim=c(length(areaUnique),length(catchCategoryUniqueAll)))
colnames(outArray) <- catchCategoryUniqueAll
rownames(outArray) <- areaUnique

for(area in areaUnique){
  idxArea <- match(area,areaUnique)
  sub_catch_raw_tab <- subset(catch_raw_tab,Area == area)
  catchCategoryUnique <- as.character(unique(sub_catch_raw_tab$CatchCategory))
  for(catchCategory in catchCategoryUnique){
    idxCatchCategory <- match(catchCategory,catchCategoryUniqueAll)
    outTemp <- subset(sub_catch_raw_tab,CatchCategory == catchCategory)
    outTemp <- subset(outTemp,CatchCategory == catchCategory)
    outArray[idxArea,idxCatchCategory] <- sum(outTemp$CATON)
  }
}

write.table(catch_raw_tab,file = paste0(inpath,'NSHM_2018_todo_catch.csv'),sep = ',',row.names = FALSE)


catch_raw_tab <- read.table(paste0(inpath,'NSHM_2017_todo_catch.csv'),sep=';',header=TRUE)

areaUnique <- as.character(unique(catch_raw_tab$Area))
catchCategoryUniqueAll <- as.character(unique(catch_raw_tab$CatchCategory))

outArrayOld <- array(NA, dim=c(length(areaUnique),length(catchCategoryUniqueAll)))
colnames(outArrayOld) <- catchCategoryUniqueAll
rownames(outArrayOld) <- areaUnique

for(area in areaUnique){
  idxArea <- match(area,areaUnique)
  sub_catch_raw_tab <- subset(catch_raw_tab,Area == area)
  catchCategoryUnique <- as.character(unique(sub_catch_raw_tab$CatchCategory))
  for(catchCategory in catchCategoryUnique){
    idxCatchCategory <- match(catchCategory,catchCategoryUniqueAll)
    outTemp2 <- subset(sub_catch_raw_tab,CatchCategory == catchCategory)
    outArrayOld[idxArea,idxCatchCategory] <- sum(outTemp$CATON)
  }
}

