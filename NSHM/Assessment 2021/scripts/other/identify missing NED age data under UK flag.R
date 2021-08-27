############################################################################################################-
# Check differences between number of Dutch age samples and readings collected under UK flag in WMR database 
# and UK age samples in InterCatch (Part 1 and 2)
#
# Check differences between number of Dutch age samples and readings in the WMR database and in InterCatch 
# (Part 3 and 4)
############################################################################################################-

library(ggplot2)

dataPath <- "NSHM/Assessment 2021/data/commercial/"
resPath  <- "NSHM/Assessment 2021/results/commercial/Missing age data/"

# Load overview of age data in InterCatch (NSHOM and WHOM separately)
nshom_IC_raw <- read.csv(paste0(dataPath,"InterCatch/historic/NSHOM_catch.data2000.20.csv"), sep=";", stringsAsFactors = FALSE)
whom_IC_raw  <- read.csv(paste0(dataPath,"InterCatch/historic/whom.catch_data_HOM_2000_20.csv"), sep=";", stringsAsFactors = FALSE)

# Load Dutch database data at WMR
hom_WMR <- read.csv(paste0(dataPath,"other/HOM_2000-2019_SampleOverview_NED.csv"), sep=";", stringsAsFactors = FALSE)



############################################################################################################-
##### Prepare InterCatch data for further analysis ----

# Only select  landings or catch data
nshom_IC <- subset(nshom_IC_raw, CatchCategory %in% c("Landings", "BMS landings", "Catch"))
whom_IC  <- subset(whom_IC_raw, CatchCategory %in% c("Landings", "BMS landings", "Catch"))

# Only select UK and NED
nshom_IC <- subset(nshom_IC, Country %in% c("UK (England)","Netherlands"))
whom_IC  <- subset(whom_IC, Country %in% c("UK (England)","Netherlands"))

# Exclude 2019 as we have no WMR data yet
nshom_IC <- subset(nshom_IC, !Year %in% 2020)
whom_IC  <- subset(whom_IC, !Year %in% 2020)


# Change column names
names(nshom_IC)[c(20,21)] <- c("No.Age.Samples","No.Age.Readings")
names(whom_IC)[c(20,21)]  <- c("No.Age.Samples","No.Age.Readings")

# Sum data across year, area, quarter and country
nshom_IC_age                  <- aggregate(No.Age.Samples ~ Country + Area + Year + Season , nshom_IC, FUN=sum)
nshom_IC_age$No.Age.Readings <- aggregate(No.Age.Readings ~ Country + Area + Year + Season, nshom_IC, FUN=sum)[,5]

whom_IC_age                   <- aggregate(No.Age.Samples ~ Country + Area + Year + Season, whom_IC, FUN=sum)
whom_IC_age$No.Age.Readings  <- aggregate(No.Age.Readings ~ Country + Area + Year + Season, whom_IC, FUN=sum)[,5]

# Add source column
nshom_IC_age$Source <- "InterCatch"
whom_IC_age$Source  <- "InterCatch"

# Rename area 4.a
nshom_IC_age$Area[nshom_IC_age$Area %in% "27.4.a.nshm"] <- "27.4.a"
nshom_IC_age$Area[nshom_IC_age$Area %in% "27.3.a.nshm"] <- "27.3.a"



############################################################################################################-
##### Prepare WMR data for further analysis ----

# Make sure column names match
names(hom_WMR) <- c("Division","Season","Year","Country","No.Age.Readings","No.Length.Measured","No.Age.Samples")

# Rename countries
hom_WMR$Country <- with(hom_WMR, ifelse(Country %in% "eng","UK (England)",
                                 ifelse(Country %in% "ned", "Netherlands",
                                 ifelse(Country %in% "fra", "France","unknown"))))

# Drop data from Dutch database that do not have info on area
hom_WMR <- subset(hom_WMR, !is.na(Division))

# Create column with ICES area
## Define first some functions used for splitting
removefirst  <- function(dat){paste(dat[-1], collapse = " ")}
removesecond <- function(dat){paste(dat[-2], collapse = " ")}
## Split Division column into numbers and letters
div.split    <- strsplit(hom_WMR$Division, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)
div.number   <- unlist(lapply(div.split, removesecond))
div.letter   <- unlist(lapply(div.split, removefirst))
## Create new area column
hom_WMR$Area <- paste0(rep("27.", length(div.number)), div.number, ".", div.letter)

# Add source column
hom_WMR$Source  <- "WMR"

# Drop length data columns and match order of columns with IC data
hom_WMR$Division <- hom_WMR$No.Length.Measured <- NULL
hom_WMR <- hom_WMR[,names(nshom_IC_age)]

# Split by stock
nshom_WMR <- subset(hom_WMR, Area %in% "27.4.a" & Season %in% c(1,2) | Area %in% c("27.4.b","27.4.c","27.7.d"))
whom_WMR  <- subset(hom_WMR, !(Area %in% "27.4.a" & Season %in% c(1,2)))
whom_WMR  <- subset(whom_WMR, !Area %in% c("27.4.b","27.4.c","27.7.d"))



############################################################################################################-
##### 1. NSHOM: compare WMR with InterCatch UK ----

# Combine the data sets
compare_WMR_IC <- rbind(nshom_WMR[nshom_WMR$Country %in% "UK (England)",], nshom_IC_age[nshom_IC_age$Country %in% "UK (England)",])
compare_WMR_IC <- reshape(compare_WMR_IC, idvar = c("Country","Year","Season","Area"), timevar = "Source", direction="wide")

# If NA, then zero
compare_WMR_IC[is.na(compare_WMR_IC)] <- 0

# Calculate difference in no age samples and measurements
compare_WMR_IC$Diff.Age.Samples <- with(compare_WMR_IC, No.Age.Samples.WMR - No.Age.Samples.InterCatch)
compare_WMR_IC$Diff.Age.Readings <- with(compare_WMR_IC, No.Age.Readings.WMR - No.Age.Readings.InterCatch)

# Sort data
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Area),]
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Season),]
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Year),]

# Remove rows where neither UK IC nor WMR has data
compare_WMR_IC <- subset(compare_WMR_IC, !(No.Age.Samples.WMR==0 & No.Age.Samples.InterCatch==0 & No.Age.Readings.WMR==0 & No.Age.Readings.InterCatch==0))

# Save
NSHOM_compare_WMR_IC <- compare_WMR_IC
save(NSHOM_compare_WMR_IC, file=paste0(resPath,"NSHOM_compare_WMR_UK_InterCatch.RData"))
write.csv(NSHOM_compare_WMR_IC, file=paste0(resPath, "NSHOM_compare_WMR_UK_InterCatch.csv"), row.names = FALSE)

# Sum differences per year and save
NSHOM_compare_WMR_IC_year                   <- aggregate(Diff.Age.Samples ~ Country + Year, NSHOM_compare_WMR_IC, FUN=sum)
NSHOM_compare_WMR_IC_year$Diff.Age.Readings <- aggregate(Diff.Age.Readings ~ Country + Year, NSHOM_compare_WMR_IC, FUN=sum)[,3]

save(NSHOM_compare_WMR_IC_year, file=paste0(resPath,"NSHOM_compare_WMR_UK_InterCatch_yearSum.RData"))
write.csv(NSHOM_compare_WMR_IC_year, file=paste0(resPath, "NSHOM_compare_WMR_UK_InterCatch_yearSum.csv"), row.names = FALSE)



############################################################################################################-
##### 2. WHOM: compare WMR with InterCatch UK ----

# Combine the data sets
compare_WMR_IC <- rbind(whom_WMR[whom_WMR$Country %in% "UK (England)",], whom_IC_age[whom_IC_age$Country %in% "UK (England)",])
compare_WMR_IC <- reshape(compare_WMR_IC, idvar = c("Country","Year","Season","Area"), timevar = "Source", direction="wide")

# If NA, then zero
compare_WMR_IC[is.na(compare_WMR_IC)] <- 0

# Calculate difference in no age samples and measurements
compare_WMR_IC$Diff.Age.Samples <- with(compare_WMR_IC, No.Age.Samples.WMR - No.Age.Samples.InterCatch)
compare_WMR_IC$Diff.Age.Readings <- with(compare_WMR_IC, No.Age.Readings.WMR - No.Age.Readings.InterCatch)

# Sort data
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Area),]
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Season),]
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Year),]

# Remove rows where neither UK IC nor WMR has data
compare_WMR_IC <- subset(compare_WMR_IC, !(No.Age.Samples.WMR==0 & No.Age.Samples.InterCatch==0 & No.Age.Readings.WMR==0 & No.Age.Readings.InterCatch==0))

# Remove rows with negative differences
compare_WMR_IC <- subset(compare_WMR_IC, Diff.Age.Readings > 0 | Diff.Age.Samples > 0)

# Save
WHOM_compare_WMR_IC <- compare_WMR_IC
save(WHOM_compare_WMR_IC, file=paste0(resPath,"WHOM_compare_WMR_UK_InterCatch.RData"))
write.csv(WHOM_compare_WMR_IC, file=paste0(resPath, "WHOM_compare_WMR_UK_InterCatch.csv"), row.names = FALSE)

# Sum differences per year
WHOM_compare_WMR_IC_year <- aggregate(Diff.Age.Samples ~ Country + Year, WHOM_compare_WMR_IC, FUN=sum)
WHOM_compare_WMR_IC_year$Diff.Age.Readings <- aggregate(Diff.Age.Readings ~ Country + Year, WHOM_compare_WMR_IC, FUN=sum)[,3]

save(WHOM_compare_WMR_IC_year, file=paste0(resPath,"WHOM_compare_WMR_UK_InterCatch_yearSum.RData"))
write.csv(WHOM_compare_WMR_IC_year, file=paste0(resPath, "WHOM_compare_WMR_UK_InterCatch_yearSum.csv"), row.names = FALSE)



############################################################################################################-
##### 3. NSHOM: missing Dutch data in InterCatch ----

# Combine the data sets
compare_WMR_IC <- rbind(nshom_WMR[nshom_WMR$Country %in% "Netherlands",], nshom_IC_age[nshom_IC_age$Country %in% "Netherlands",])
compare_WMR_IC <- reshape(compare_WMR_IC, idvar = c("Country","Year","Season","Area"), timevar = "Source", direction="wide")

# If NA, then zero
compare_WMR_IC[is.na(compare_WMR_IC)] <- 0

# Calculate difference in no age samples and measurements
compare_WMR_IC$Diff.Age.Samples <- with(compare_WMR_IC, No.Age.Samples.WMR - No.Age.Samples.InterCatch)
compare_WMR_IC$Diff.Age.Readings <- with(compare_WMR_IC, No.Age.Readings.WMR - No.Age.Readings.InterCatch)

# Sort data
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Area),]
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Season),]
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Year),]

# Remove rows where neither UK IC nor WMR has data
compare_WMR_IC <- subset(compare_WMR_IC, !(No.Age.Samples.WMR==0 & No.Age.Samples.InterCatch==0 & No.Age.Readings.WMR==0 & No.Age.Readings.InterCatch==0))

# Save
NSHOM_compare_WMR_IC <- compare_WMR_IC
save(NSHOM_compare_WMR_IC, file=paste0(resPath,"NSHOM_compare_WMR_NED_InterCatch.RData"))
write.csv(NSHOM_compare_WMR_IC, file=paste0(resPath, "NSHOM_compare_WMR_NED_InterCatch.csv"), row.names = FALSE)

# Sum differences per year and save
NSHOM_compare_WMR_IC_year                   <- aggregate(Diff.Age.Samples ~ Country + Year, NSHOM_compare_WMR_IC, FUN=sum)
NSHOM_compare_WMR_IC_year$Diff.Age.Readings <- aggregate(Diff.Age.Readings ~ Country + Year, NSHOM_compare_WMR_IC, FUN=sum)[,3]

save(NSHOM_compare_WMR_IC_year, file=paste0(resPath,"NSHOM_compare_WMR_NED_InterCatch_yearSum.RData"))
write.csv(NSHOM_compare_WMR_IC_year, file=paste0(resPath, "NSHOM_compare_WMR_NED_InterCatch_yearSum.csv"), row.names = FALSE)



############################################################################################################-
##### 4. WHOM: missing Dutch data in InterCatch ----

# Combine the data sets
compare_WMR_IC <- rbind(whom_WMR[whom_WMR$Country %in% "Netherlands",], whom_IC_age[whom_IC_age$Country %in% "Netherlands",])
compare_WMR_IC <- reshape(compare_WMR_IC, idvar = c("Country","Year","Season","Area"), timevar = "Source", direction="wide")

# If NA, then zero
compare_WMR_IC[is.na(compare_WMR_IC)] <- 0

# Calculate difference in no age samples and measurements
compare_WMR_IC$Diff.Age.Samples <- with(compare_WMR_IC, No.Age.Samples.WMR - No.Age.Samples.InterCatch)
compare_WMR_IC$Diff.Age.Readings <- with(compare_WMR_IC, No.Age.Readings.WMR - No.Age.Readings.InterCatch)

# Sort data
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Area),]
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Season),]
compare_WMR_IC <- compare_WMR_IC[order(compare_WMR_IC$Year),]

# Remove rows where neither UK IC nor WMR has data
compare_WMR_IC <- subset(compare_WMR_IC, !(No.Age.Samples.WMR==0 & No.Age.Samples.InterCatch==0 & No.Age.Readings.WMR==0 & No.Age.Readings.InterCatch==0))

# Save
WHOM_compare_WMR_IC <- compare_WMR_IC
save(WHOM_compare_WMR_IC, file=paste0(resPath,"WHOM_compare_WMR_NED_InterCatch.RData"))
write.csv(WHOM_compare_WMR_IC, file=paste0(resPath, "WHOM_compare_WMR_NED_InterCatch.csv"), row.names = FALSE)

# Sum differences per year and save
WHOM_compare_WMR_IC_year                   <- aggregate(Diff.Age.Samples ~ Country + Year, WHOM_compare_WMR_IC, FUN=sum)
WHOM_compare_WMR_IC_year$Diff.Age.Readings <- aggregate(Diff.Age.Readings ~ Country + Year, WHOM_compare_WMR_IC, FUN=sum)[,3]

save(WHOM_compare_WMR_IC_year, file=paste0(resPath,"WHOM_compare_WMR_NED_InterCatch_yearSum.RData"))
write.csv(WHOM_compare_WMR_IC_year, file=paste0(resPath, "WHOM_compare_WMR_NED_InterCatch_yearSum.csv"), row.names = FALSE)
