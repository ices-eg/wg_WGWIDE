############################################################################################################-
# Extract survey data from DATRAS
#
# Created by Alfonso Pérez Rodríguez, adapted by Esther Beukhof
############################################################################################################-

library(icesDatras)
library(icesVocab)

# Set paths
datpath <- "NSHM/Assessment 2021/data/survey/"
outpath <- "NSHM/Assessment 2021/results/survey/"


############################################################################################################-
##### Extract data from DATRAS -----

# Check available surveys
getSurveyList()
# getDatrasDataOverview(surveys = c("NS-IBTS","FR-CGFS"))

# Set parameters for extracting
years            <- c(1991:2020)
datafile_HH_ibts <- "survey-hh-ibts.RData"
datafile_HL_ibts <- "survey-hl-ibts.RData"
datafile_HH_cgfs <- "survey-hh-cgfs.RData"
datafile_HL_cgfs <- "survey-hl-cgfs.RData"

# Extract HH data (or load if already extracted once)
## NS-ITBS
if(!file.exists(paste(datpath, datafile_HH_ibts,sep=""))){
  survey_hh_ibts <- getDATRAS(record = "HH", survey = "NS-IBTS", year = years, quarters = 3)
  save(survey_hh_ibts, file=paste(datpath, datafile_HH_ibts,sep="")) #save raw data
} else {
  load(paste(datpath, datafile_HH_ibts,sep=""))
}
## FR-CGFS
if(!file.exists(paste(datpath, datafile_HH_cgfs,sep=""))){
  survey_hh_cgfs <- getDATRAS(record = "HH", survey = "FR-CGFS", year = years, quarters = 4)
  save(survey_hh_cgfs, file=paste(datpath, datafile_HH_cgfs,sep="")) #save raw data
} else {
  load(paste(datpath, datafile_HH_cgfs,sep=""))
}

# Extract HL data (or load if already extracted once)
## NS-ITBS
if(!file.exists(paste(datpath, datafile_HL_ibts,sep=""))){
  survey_hl_ibts <- getDATRAS(record = "HL", survey = "NS-IBTS", year = years, quarters = 3)
  save(survey_hl_ibts, file=paste(datpath, datafile_HL_ibts,sep="")) #save raw data
} else {
  load(paste(datpath, datafile_HL_ibts,sep=""))
}
## FR-CGFS
if(!file.exists(paste(datpath, datafile_HL_cgfs,sep=""))){
  survey_hl_cgfs <- getDATRAS(record = "HL", survey = "FR-CGFS", year = years, quarters = 4)
  save(survey_hl_cgfs, file=paste(datpath, datafile_HL_cgfs,sep="")) #save raw data
} else {
  load(paste(datpath, datafile_HL_cgfs,sep=""))
}


############################################################################################################-
##### Select columns of interest -----

### HL data

# Combine HL data
survey_hl <- rbind(survey_hl_cgfs,survey_hl_ibts)

# Select species: both AphiaID and TSN code, as the latter was used until 1997
survey_hl <- subset(survey_hl, SpecCode %in% c(126822,168588))

# Create unique haulID
survey_hl$HaulID <- paste(survey_hl$Survey,survey_hl$Year,survey_hl$Ship,survey_hl$HaulNo,sep="-")

# Check length measurement and convert from mm to cm where necessary
table(survey_hl$LngtCode) # '.' in mm, '1' in cm, '-9' is invalid
survey_hl$LngtClass <- with(survey_hl,ifelse(LngtCode %in% "1", LngtClass,LngtClass/10))
summary(survey_hl$LngtClass)

# Check SubFactor
summary(survey_hl$SubFactor)
survey_hl$SubFactor <- with(survey_hl, ifelse(SubFactor == -9, NA, SubFactor))
summary(survey_hl$SubFactor)

# Check HLNoAtLngt
summary(survey_hl$HLNoAtLngt)
survey_hl$HLNoAtLngt <- with(survey_hl, ifelse(HLNoAtLngt == -9, NA, HLNoAtLngt))
summary(survey_hl$HLNoAtLngt)

# Remove records with NA for HLNoAtLngt
survey_hl <- subset(survey_hl, !is.na(HLNoAtLngt))

# Select columns of interest
select           <- c("Survey", "Year", "HaulID", "LngtClass", "HLNoAtLngt","SubFactor")
survey_hl        <- survey_hl[,names(survey_hl) %in% select]
names(survey_hl) <- c("Survey", "Year", "SubFactor", "LngtClass", "Number","HaulID")

# Check the data
head(survey_hl)
dim(survey_hl)
summary(survey_hl)


### HH data

# Combine HH data
survey_hh <- rbind(survey_hh_cgfs,survey_hh_ibts)

# Create unique haulID
survey_hh$HaulID <- paste(survey_hh$Survey,survey_hh$Year,survey_hh$Ship,survey_hh$HaulNo,sep="-")

# Select only valid hauls
survey_hh <- subset(survey_hh, HaulVal %in% "V")

# Check data type
table(survey_hh$DataType, survey_hh$Survey, useNA = "always")
table(survey_hh$Year, survey_hh$DataType, useNA = "always")

# Select columns of interest
select    <- c("HaulID","Survey","Year","HaulDur", "ShootLat", "ShootLong", "Depth", "StatRec","DataType")
survey_hh <- survey_hh[,names(survey_hh) %in% select]

# Check the data
dim(survey_hh)
head(survey_hh)
summary(survey_hh)


############################################################################################################-
##### Save ----

save(survey_hh, file=paste(outpath,"survey_hh_1991-2020.RData",sep=""))
save(survey_hl, file=paste(outpath,"survey_hl_1991-2020.RData",sep=""))
