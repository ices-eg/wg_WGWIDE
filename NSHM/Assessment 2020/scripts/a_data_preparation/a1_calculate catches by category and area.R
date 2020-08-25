############################################################################################################-
# Calculate total catches and catches by category and area
#
# The output is stored and should then be pasted to caton.csv and caton_cat_area.csv files that contain
# the catch data from all years.
#
# Created by Benoit Berges, adapted by Esther Beukhof
############################################################################################################-

library(tidyverse)

# Setpaths
inpath <- "NSHM/Assessment 2020/data/commercial/InterCatch/Ages from InterCatch/"
outpath <- "NSHM/Assessment 2020/data/commercial/"

# Load catch data from InterCatch output
catch_raw_tab <- read.table(paste0(inpath,'NSHM_2019_catch_table.csv'),sep=',',header=TRUE)

# Get all areas and catch categories
areaUnique             <- as.character(unique(catch_raw_tab$Area))
catchCategoryUniqueAll <- as.character(unique(catch_raw_tab$CatchCategory))

# Create array to store the data in
outArray           <- array(NA, dim=c(length(areaUnique),length(catchCategoryUniqueAll)))
colnames(outArray) <- catchCategoryUniqueAll
rownames(outArray) <- areaUnique

# Loop through the areas to calculate the total catch per category and per area
for(area in areaUnique){
  idxArea             <- match(area,areaUnique)
  sub_catch_raw_tab   <- subset(catch_raw_tab,Area == area)
  catchCategoryUnique <- as.character(unique(sub_catch_raw_tab$CatchCategory))
  for(catchCategory in catchCategoryUnique){
    idxCatchCategory <- match(catchCategory,catchCategoryUniqueAll)
    outTemp          <- subset(sub_catch_raw_tab,CatchCategory == catchCategory)
    outTemp          <- subset(outTemp,CatchCategory == catchCategory)
    outArray[idxArea,idxCatchCategory] <- sum(outTemp$CATON)
  }
}
caton_area_cat_2019 <- outArray/1000 #convert from kg to tonnes
caton_2019          <- sum(outArray, na.rm=TRUE)/1000 #calculate the total catch in 2019 (and convert to tonnes)

# Save
write.csv(caton_area_cat_2019, file=paste(outpath,"caton_area_cat_",year,".csv",sep="")) 
write.csv(caton_2019, file=paste(outpath,"caton_",year,".csv",sep="")) 
