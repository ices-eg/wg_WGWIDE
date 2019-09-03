# CPUE calculation for North Sea horse mackerel based on NS-IBTS and CGFS

# Load survey data
dat <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/survey_data_1992-2018_clean.csv")

# How Alfonso calculated CPUE:
dat$CPUE <- data$Number/(data$HaulDur/60)

# How Esther calculates CPUE:
dat$CPUE <- NA 
# If datatype=C (catch per unit of effort (1 h trawling)), then number * subfactor
dat[dat$DataType=="C",]$CPUE <- (dat[dat$DataType=="C",]$Number)*(dat[dat$DataType=="C",]$SubFactor)
#If datatype=R (raw data), then number * subfactor * haul duration/60
dat[dat$DataType=="R",]$CPUE <- (dat[dat$DataType=="R",]$Number)*(dat[dat$DataType=="R",]$SubFactor)*60/(dat[dat$DataType=="R",]$HaulDur)
