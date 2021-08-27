############################################################################################################-
# Prepare survey data for further analysis and make exploration plots
#
# Created by Alfonso Pérez Rodríguez, adapted by Esther Beukhof
############################################################################################################-

library(ggplot2)

# Set paths
outpath <- "NSHM/Assessment 2021/results/survey/"

# Load extracted survey data
load("NSHM/Assessment 2021/results/survey/survey_hh_1991-2020.RData")
load("NSHM/Assessment 2021/results/survey/survey_hl_1991-2020.RData")


############################################################################################################-
##### Prepare survey data -----

# Create a database in which all hauls with all possible sizes for NSHM will be
temp <- expand.grid(HaulID = sort(unique(survey_hh$HaulID)), LngtClass=c(1:60)) #comment EB: I changed minimum from 0 to 1 cm
temp <- temp[with(temp, order(HaulID,LngtClass)),]
head(temp)

# Merge with the survey data (to get expanded HH data)
survey_hh_exp <- merge(survey_hh,temp, by.x = "HaulID", by.y = "HaulID", all.y=TRUE)
head(survey_hh_exp)

# Create unique code for each combination of haul ID and length
survey_hh_exp$code <- paste(survey_hh_exp$HaulID, survey_hh_exp$LngtClas, sep="-")
head(survey_hh_exp)

# Prepare HL data for merging with the expanded HH survey data
survey_hl$code <- paste(survey_hl$HaulID, survey_hl$LngtClas, sep="-")
head(survey_hl)

# Merge the survey HL data with the expanded HH data
survey_exp <- merge(survey_hh_exp, survey_hl[,c("code","Number","SubFactor")], by="code", all.x = TRUE)
head(survey_exp)

# Convert NAs for number of fish caught into 0
summary(survey_exp)
survey_exp$Number[is.na(survey_exp$Number)] <- 0
summary(survey_exp)

# Next, prepare the data base for the analysisdim(data). 
# data=data[data$Depth>0,] # Comment EB: as depth is not used in the analysis further on, no need to delete these data
# dim(data)
# data=na.omit(data)
# dim(data)
# data=data[data$LngtClas>=0,] #Comment EB: I set minimum length to 1 cm (this is also minimum in survey data), so no need to delete these data
# dim(data)

# Exclude ICES rectangles 
# Statistical rectangles from areas 3a and 4a are excluded since the surveys are conducted in Q3 and Q4, when the Western stock is 
# assumed to be the one inhabiting this areas. The statistical rectangles from the CGFS survey area are all included in this list.
rectangles <- c("27E8", "27E9", "27F0", "28E8", "28E9", "28F0", "28F1", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0", "30F1", "31F1", 
                "31F2", "39F1", "39F2", "40F1", "40F2", "40F3", "40F4", "41F2", "41F3", "41F4", "42F3", "42F4", "43F4", "36F0", "37F0", 
                "37F1", "38E9", "38F0", "38F1", "39F0", "32F1", "33F2", "34F2", "35F1", "35F2", "36F1", "36F2", "32F2", "32F3", "33F3", 
                "33F4", "34F3", "34F4", "35F3", "35F4", "36F3", "36F4", "36F5", "36F6", "37F2", "37F3", "37F4", "37F5", "37F6", "37F7", 
                "38F2", "38F3", "38F4", "38F5", "38F6", "38F7", "39F3", "39F4", "39F5", "39F6", "39F7", "40F5", "40F6", "40F7", "41F7", 
                "42F5", "42F6")
table(survey_exp$Survey)
survey_exp <- survey_exp[survey_exp$StatRec %in% rectangles,]
table(survey_exp$Survey)

# Remove those hauls conducted in statistical rectangles 33F1 and 33F2 due to change in research vessel CGFS after 2014
dim(survey_exp)
survey_exp <- subset(survey_exp, !StatRec %in% c("33F1","33F2"))
dim(survey_exp)

# Remove hauls that have been conducted in the mouth of the Seine river (as new research vessel cannot fish there)
dim(survey_exp)
survey_exp <- survey_exp[survey_exp$ShootLat>49.4735 | survey_exp$ShootLong<(-0.09442),]
dim(survey_exp)

# Define the years that are going to be used in the analysis
intYears   <- c(1992:2020)
table(survey_exp$Survey)
survey_exp <- survey_exp[survey_exp$Year %in% intYears,]
table(survey_exp$Survey)


############################################################################################################-
##### Calculate CPUE ----

# Select haul duration between 15 and 45 minutes
table(survey_exp$HaulDur)
survey_exp <- survey_exp[!(survey_exp$HaulDur<15 | survey_exp$HaulDur>45),] 

# Calculate CPUE
survey_exp$CPUE <- NA
## If datatype=C (catch per unit of effort (1 h trawling)), then number at length per haul = number at length * subfactor
survey_exp[survey_exp$DataType=="C",]$CPUE <- round((survey_exp[survey_exp$DataType=="C",]$Number)*(survey_exp[survey_exp$DataType=="C",]$SubFactor),0)
## If dataatype=R (raw data), then number at length per haul = number at length * subfactor * 60/haul duration (60 from 60 minutes -> we want to convert from x minutes haul duration to one hour)
survey_exp[survey_exp$DataType=="R",]$CPUE <- round((survey_exp[survey_exp$DataType=="R",]$Number)*(survey_exp[survey_exp$DataType=="R",]$SubFactor)*60/(survey_exp[survey_exp$DataType=="R",]$HaulDur),0)

# Check CPUE data and convert NAs to 0
summary(survey_exp)
survey_exp$CPUE[is.na(survey_exp$CPUE)] <- 0
summary(survey_exp)

# Convert abundance of CGFS by GWD to match with new vessel THA
survey_exp$CPUE <- with(survey_exp,ifelse(Survey %in% "FR-CGFS" & Year<2015, CPUE*10.363,CPUE))
summary(survey_exp)

# Quick plot to check CPUE data
datplot <- survey_exp[survey_exp$CPUE > 0,]
ggplot(datplot,aes(x=Year,y=log(CPUE),group=Year)) +
  geom_boxplot() +
  facet_wrap(~Survey) + ggtitle("")


############################################################################################################-
##### Final data prep and save  -----

# Separate the data in juveniles and exploitable stock based in the 20cm size limit decided during the 2017 benchmark 
survey_exp$groupsize <- ifelse(survey_exp$LngtClas<20,"juveniles","exploitable")

# Next, hauls for which there is no catch for any of the two groupsizes and for which the length is zero, 
# hauls are going to be assigned to both juvenile and exploitable stocks
# sel        <- survey_exp[survey_exp$LngtClass==0,]
# sel$groupsize=rep("exploitable",length(sel$Year))
# survey_exp <- rbind(survey_exp,sel)
# Comment EB: I dropped this as I removed 0 length (so start at 1 cm, 0 cm doesn't make sense and is not in the survey data either)

# Reset the levels of factors for year and HaulID
survey_exp$Year   <- factor(survey_exp$Year)
survey_exp$HaulID <- factor(survey_exp$HaulID)

# Save
head(survey_exp)
summary(survey_exp)
write.csv(survey_exp, paste(outpath,"survey_data_1992-2020_clean.csv",sep = ""),row.names = FALSE)
save(survey_exp, file=paste(outpath,"survey_data_1992-2020_clean.RData",sep = ""))

