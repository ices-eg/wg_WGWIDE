
library(FLCore)
library(stockassessment)

# Set assessment year
assessmentYear           <- 2024

# Should extended dataset be used?
extended                 <- TRUE

# Set path and working directory for SAM data input 
root                     <- file.path('m:','WGWIDE',paste0('whb-',assessmentYear),paste0("bw_",assessmentYear,"_preliminary_catch"),"data")
setwd(root)
getwd()

# Set root for extended dataset
root.ext                 <- file.path('m:','WGWIDE',paste0('whb-',assessmentYear),paste0("bw_",assessmentYear,"_preliminary_catch_extended"),"data")

# Create index.txt file as input for creation FLStock object
cat(
    "BW", 
    "1",
    " ",          # no yield file in SAM
    "cn.dat",  #2 catch C
    "lw.dat",  #3 catch w
    "sw.dat",  #4
    "nm.dat",  #5
    "mo.dat",  #6
    "pf.dat",  #7
    "pm.dat",  #8
    " ", # F   #9
    " ", # N  #10
    " ", # discard ton #11
    " ", # discard N  #12
    " ", # dicard W   #13
    " ",   #14
    " ",   #15
    sep = "\n",file=file.path(root,"index.txt"))

# Create FLStock object
stock             <- readFLStock(file.path(root,"index.txt"))

# Check stock object
slotNames(stock)
summary(stock)
stock@m
stock@mat
stock@harvest.spwn
stock@m.spwn

# Add catch numbers and weight at age
stock@catch.n     <-readVPAFile(file.path(root, "cn.dat"))
stock@catch.wt    <-readVPAFile(file.path(root, "lw.dat"))
stock@landings.n  <-readVPAFile(file.path(root, "cn.dat"))
stock@landings.wt <-readVPAFile(file.path(root, "lw.dat"))
catch(stock)      <- computeCatch(stock, na.rm=TRUE)
landings(stock)   <- computeLandings(stock)
summary(stock)

# Load SAM output
if(extended == FALSE){
  
  source('../src/common.R')
  load(file.path(root,'..',"run","model.RData"),verbose=TRUE)  # to get the current fit 
}
if(extended == TRUE){
  
  source('../src/common.R')
  load(file.path(root.ext,'..',"run","model.RData"),verbose=TRUE)  # to get the current fit 
  
  # Create extended FLStock object
  stock.not.extended    <- stock
  stock                 <- window(stock.not.extended, end=2025)
}
minAge           <-fit$data$minAgePerFleet[1]
maxAge           <-fit$data$maxAgePerFleet[1] 
noN              <-maxAge-minAge+1

tab2b            <-ntable(fit)  #stock number

# Add estimated stock size in numbers
stock@stock.n[,,1,1,1,1]   <-t(tab2b)
if(extended == FALSE){
  stock@stock.wt             <-readVPAFile(file.path(root, "sw.dat"))
}
if(extended == TRUE){
  stock@stock.wt             <-readVPAFile(file.path(root.ext, "sw.dat"))
  stock@stock.wt[,as.character(assessmentYear+1)]       <- yearMeans(stock@stock.wt[,as.character(2022:2024)])
}

# Add estimated fishing mortality
stock@harvest.spwn
tab3                       <-faytable(fit)
harvest(stock)[,,1,1,1,1]  <-t(tab3)

# Check
ssb(stock)
harvest(stock)
catch(stock)

# Add discards (which are all zero)
stock@discards.n[]        <-0
stock@discards.wt[]       <-0
stock@discards[]          <-0
discards(stock)           <- computeDiscards(stock)

# Add additional values for extended dataset
if(extended == TRUE){
  stock@m[,as.character(assessmentYear+1)]              <- stock@m[,as.character(assessmentYear)]
  stock@mat[,as.character(assessmentYear+1)]            <- stock@mat[,as.character(assessmentYear)]
  stock@harvest.spwn[,as.character(assessmentYear+1)]   <- stock@harvest.spwn[,as.character(assessmentYear)]
  stock@m.spwn[,as.character(assessmentYear+1)]         <- stock@m.spwn[,as.character(assessmentYear)]
  stock@catch.wt[,as.character(assessmentYear+1)]       <- yearMeans(stock@catch.wt[,as.character(2022:2024)])
  stock@landings.wt[,as.character(assessmentYear+1)]    <- yearMeans(stock@landings.wt[,as.character(2022:2024)])
  stock@stock.n[1,as.character(assessmentYear+1)]       <- exp(mean(log(rectable(fit)[as.character(1996:(assessmentYear-1)),1])))
}

# Add fbar range and units
stock@range["minfbar"]    <- 3
stock@range["maxfbar"]    <- 7
stock@range["plusgroup"]  <- stock@range["max"] 
units(harvest(stock))     <- "f"
units(catch.n(stock))     <- "thousands"
units(catch(stock))       <- "tonnes"
units(catch.wt(stock))    <- "kg"
units(stock.wt(stock))    <- "kg"
units(stock(stock))       <- "tonnes"
units(stock.n(stock))     <- "thousands"
units(landings.n(stock))  <- "thousands"
units(landings(stock))    <- "tonnes"
summary(stock)

# Plot to check
plot(stock)

# Save
if(extended == FALSE){
  save(stock,file=file.path(root,paste0('FLStock_BW_',assessmentYear,'.RData')))
}
if(extended == TRUE){
  save(stock,file=file.path(root.ext,paste0('FLStock_BW_',assessmentYear,'_extended.RData')))
}
