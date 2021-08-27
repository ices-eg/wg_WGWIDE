############################################################################################################-
# Assess impact of only CGFS data in French EEZ in 2020 on abundance index
#
# - Assign hauls to either French or English EEZ
# - Redo the model up to 2019 with only French data for all years and for final year, and see if resulting 
#   index is different from when English hauls are included.
# - Redo the model without CGFS data for final year
# - Calculate model index for 2020 assessment for the different option
############################################################################################################-

library(sp)
library(pscl)
library(lme4)
library(plyr)
library(ggplot2)

# Set figure path
figpath <- "NSHM/Assessment 2021/figures/model/"

# Load survey data (note: form last year, as we want to compare to last years index up to 2019)
survey <- read.csv("NSHM/Assessment 2020/results/survey/survey_data_1992-2019_clean.csv",stringsAsFactors = FALSE)

# Load spatial data on EEZ
load("NSHM/Assessment 2021/data/survey/EEZ.RData")

# Select unique hauls
hauls <- survey[!duplicated(survey$HaulID),]

# Match hauls with corresponding EEZ
coords                <- coordinates(hauls[,c("ShootLong","ShootLat")])
# coords                <- coords[sample(c(1:nrow(coords)),10),]
spPoints              <- SpatialPoints(coords)
proj4string(spPoints) <- proj4string(EEZ)
EEZ_haul              <- over(spPoints, EEZ) #check to which EEZ each haul belongs

# Match EEZ with haul and survey data
hauls$EEZ <- EEZ_haul$Country
survey    <- merge(survey, hauls[,c("HaulID","EEZ")], all.x=TRUE)

# Weights parameters to multiply the fitted values. These weighting factors consider both the survey area 
# covered and the wing spread.
weightCGFS=0.24 #CGFS covers 14% of total NSHOM area, and up to 2014 gear was 52.6% of the IBTS wingspread
weightIBTS=0.76 #IBTS covers 86% of total NSHOM area, and had larger wingspread than CGFS up to 2014


############################################################################################################-
##### Exclude UK stations from all years ----

# Drop English CGFS data from the data, so with French CGFS data only
survey_FRA <- subset(survey, !(Survey %in% "FR-CGFS" & EEZ %in% "United Kingdom"))

# Run hurdle model for exploitable stock with French CGFS data only
datos <- survey_FRA[survey_FRA$groupsize=="exploitable",] #select adults                
dat   <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos) # Sum CPUE across length classes
dim(dat);str(dat)
table(dat$CPUE>0) #check how many zeroes
dat$CPUE <- round(dat$CPUE,0) #round off CPUE values to make sure it's count data
dat$Year <- as.factor(dat$Year)
str(dat)
mod.expl.FRA <- hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial") #run hurdle model 
summary(mod.expl.FRA)
## Save model and data
save(mod.expl.FRA, file="NSHM/Assessment 2021/results/model/model_adults_only_FRA_CGFS.RData")
dat_FRA <- dat
save(survey_FRA, dat_FRA, survey_FRA, EEZ_haul, file="NSHM/Assessment 2021/results/model/data_model_adults_only_FRA_CGFS.RData")

# Load model and data from 2020 assessment
load("NSHM/Assessment 2020/results/model/model_adults.RData")
load("NSHM/Assessment 2020/results/model/data_model_adults.RData")

# Multiply the model prediction by the weighting factors
dat$weight     <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit        <- predict(mod.expl)*dat$weight
dat_FRA$weight <- ifelse(dat_FRA$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat_FRA$fit    <- predict(mod.expl.FRA)*dat_FRA$weight

# Estimate the index for the CGFS survey
dat_CGFS               <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS          <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year     <- as.integer(as.character(survpred_CGFS$Year))
dat_CGFS_FRA           <- dat_FRA[dat_FRA$Survey=="FR-CGFS",]
survpred_CGFS_FRA      <- ddply(dat_CGFS_FRA,.(Year),summarize,idx=mean(fit))
survpred_CGFS_FRA$Year <- as.integer(as.character(survpred_CGFS_FRA$Year))

# Estimate the index for the IBTS survey
dat_IBTS               <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS          <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year     <- as.integer(as.character(survpred_IBTS$Year))
dat_IBTS_FRA           <- dat[dat_FRA$Survey=="NS-IBTS",]
survpred_IBTS_FRA      <- ddply(dat_IBTS_FRA,.(Year),summarize,idx=mean(fit))
survpred_IBTS_FRA$Year <- as.integer(as.character(survpred_IBTS_FRA$Year))

# Combine output
index.expl     <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)
index.expl_FRA <- data.frame(year=survpred_CGFS_FRA$Year, cgfs=survpred_CGFS_FRA$idx, ibts=survpred_IBTS_FRA$idx)

# Estimate the combined index as the average of the two survey indexes
index.expl$all <- rowMeans(index.expl[,!names(index.expl)=="year"])
index.expl_FRA$all <- rowMeans(index.expl_FRA[,!names(index.expl_FRA)=="year"])

# Combine
index.expl$surveyEEZ <- "FRA & UK"
index.expl_FRA$surveyEEZ <- "FRA"
index.expl.combine <- rbind(index.expl, index.expl_FRA)

# Plot to compare the time series (all, CGFS index only, IBTS index only)
p <- ggplot(index.expl.combine, aes(year,all,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("Combined IBTS-CGFS index exploitable stock") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_combined.png", p, path=figpath, width=8, height = 6)

p <- ggplot(index.expl.combine, aes(year,cgfs,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("CGFS index exploitable stock") +
    theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_CGFS.png", p, path=figpath, width=8, height = 6)

p <- ggplot(index.expl.combine, aes(year,ibts,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("IBTS index exploitable stock") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_IBTS.png", p, path=figpath, width=8, height = 6)

# Extract year effects
year_eff     <- data.frame(parameter = names(coefficients(mod.expl)), 
                       model=c(rep("count",66),rep("zero",length(coefficients(mod.expl))-66)),
                       coefficient_FRA_UK = coefficients(mod.expl), 
                       coefficient_FRA = coefficients(mod.expl.FRA))

# Plot year effects to compare
p <- ggplot(year_eff, aes(coefficient_FRA_UK, coefficient_FRA, colour=model)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()
print(p)

p <- ggplot(year_eff, aes(coefficient_FRA_UK, coefficient_FRA, colour=model)) +
  geom_point() +
  geom_text(aes(label=parameter),hjust=0, vjust=0) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()
print(p)


############################################################################################################-
##### Exclude UK stations from 2019 ----

# Load survey data (note: form last year, as we want to compare to last years index up to 2019)
survey <- read.csv("NSHM/Assessment 2020/results/survey/survey_data_1992-2019_clean.csv",stringsAsFactors = FALSE)
survey <- merge(survey, hauls[,c("HaulID","EEZ")], all.x=TRUE)

# Remove UK stations from CGFS from last year, 2019
survey_2019 <- subset(survey, !(Survey %in% "FR-CGFS" & EEZ %in% "United Kingdom" & Year %in% 2019))

# Run hurdle model for exploitable stock with French CGFS data only
datos <- survey_2019[survey_2019$groupsize=="exploitable",] #select adults                
dat   <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos) # Sum CPUE across length classes
dim(dat);str(dat)
table(dat$CPUE>0) #check how many zeroes
dat$CPUE <- round(dat$CPUE,0) #round off CPUE values to make sure it's count data
dat$Year <- as.factor(dat$Year)
str(dat)
mod.expl.2019 <- hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial") #run hurdle model 
summary(mod.expl.2019)
## Save model and data
save(mod.expl.2019, file="NSHM/Assessment 2021/results/model/model_adults_only_FRA_CGFS_2019.RData")
dat_2019 <- dat
save(survey_2019, dat_2019, survey_2019, EEZ_haul, file="NSHM/Assessment 2021/results/model/data_model_adults_only_FRA_CGFS.RData")

# Load model and data from 2020 assessment
load("NSHM/Assessment 2020/results/model/model_adults.RData")
load("NSHM/Assessment 2020/results/model/data_model_adults.RData")

# Multiply the model prediction by the weighting factors
dat$weight      <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit         <- predict(mod.expl)*dat$weight
dat_2019$weight <- ifelse(dat_2019$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat_2019$fit    <- predict(mod.expl.2019)*dat_2019$weight

# Estimate the index for the CGFS survey
dat_CGFS                <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS           <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year      <- as.integer(as.character(survpred_CGFS$Year))
dat_CGFS_2019           <- dat_2019[dat_2019$Survey=="FR-CGFS",]
survpred_CGFS_2019      <- ddply(dat_CGFS_2019,.(Year),summarize,idx=mean(fit))
survpred_CGFS_2019$Year <- as.integer(as.character(survpred_CGFS_2019$Year))

# Estimate the index for the IBTS survey
dat_IBTS                <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS           <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year      <- as.integer(as.character(survpred_IBTS$Year))
dat_IBTS_2019           <- dat_2019[dat_2019$Survey=="NS-IBTS",]
survpred_IBTS_2019      <- ddply(dat_IBTS_2019,.(Year),summarize,idx=mean(fit))
survpred_IBTS_2019$Year <- as.integer(as.character(survpred_IBTS_2019$Year))

# Combine output
index.expl     <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)
index.expl_2019 <- data.frame(year=survpred_CGFS_2019$Year, cgfs=survpred_CGFS_2019$idx, ibts=survpred_IBTS_2019$idx)

# Estimate the combined index as the average of the two survey indexes
index.expl$all <- rowMeans(index.expl[,!names(index.expl)=="year"])
index.expl_2019$all <- rowMeans(index.expl_2019[,!names(index.expl_2019)=="year"])

# Combine
index.expl$surveyEEZ <- "FRA & UK 2019"
index.expl_2019$surveyEEZ <- "FRA 2019"
index.expl.combine <- rbind(index.expl, index.expl_2019)

# Plot to compare the time series (all, CGFS index only, IBTS index only)
p <- ggplot(index.expl.combine, aes(year,all,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("Combined IBTS-CGFS index exploitable stock") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_combined_2019_without_UK.png", p, path=figpath, width=8, height = 6)

p <- ggplot(index.expl.combine, aes(year,cgfs,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("CGFS index exploitable stock") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_CGFS_2019_without_UK.png", p, path=figpath, width=8, height = 6)

p <- ggplot(index.expl.combine, aes(year,ibts,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("IBTS index exploitable stock") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_IBTS_2019_without_UK.png", p, path=figpath, width=8, height = 6)

# Extract year effects
year_eff     <- data.frame(parameter = names(coefficients(mod.expl)), 
                           model=c(rep("count",66),rep("zero",length(coefficients(mod.expl))-66)),
                           coefficient_FRA_UK = coefficients(mod.expl), 
                           coefficient_2019 = coefficients(mod.expl.2019))

# Plot year effects to compare
p <- ggplot(year_eff, aes(coefficient_FRA_UK, coefficient_2019, colour=model)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(x="Coefficients model 2019 FRA & UK", y="Coefficients model 2019 FRA only") +
  theme_bw()
print(p)
ggsave("coefficients_CGFS_change_2019_without_UK.png", p, path=figpath, width=8, height = 6)

p <- ggplot(year_eff, aes(coefficient_FRA_UK, coefficient_2019, colour=model)) +
  geom_point() +
  geom_text(aes(label=parameter),hjust=0, vjust=0, size=2) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x="Coefficients model 2019 FRA & UK", y="Coefficients model 2019 FRA only") +
  theme_bw()
print(p)
ggsave("coefficients_CGFS_change_2019_without_UK_text.png", p, path=figpath, width=8, height = 6)


############################################################################################################-
##### Exclude CGFS 2019 ----

# Load survey data (note: form last year, as we want to compare to last years index up to 2019)
survey <- read.csv("NSHM/Assessment 2020/results/survey/survey_data_1992-2019_clean.csv",stringsAsFactors = FALSE)

# Remove UK stations from CGFS from last year, 2019
survey_2019_cgfs <- subset(survey, !(Survey %in% "FR-CGFS" & Year %in% 2016))

# Run hurdle model for exploitable stock with French CGFS data only
datos <- survey_2019_cgfs[survey_2019_cgfs$groupsize=="exploitable",] #select adults                
dat   <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos) # Sum CPUE across length classes
dim(dat);str(dat)
table(dat$CPUE>0) #check how many zeroes
dat$CPUE <- round(dat$CPUE,0) #round off CPUE values to make sure it's count data
# dat$CPUE <- as.integer(dat$CPUE)
dat$Year <- as.factor(dat$Year)
str(dat)
mod.expl.2019.cgfs <- hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial") #run hurdle model 
summary(mod.expl.2019.cgfs)
## Model doesn't run! Neither when another year is left out.
## Error message: Error in optim(fn = countDist, gr = countGrad, par = c(start$count, 
##                  if (dist ==  : non-finite value supplied by optim
## https://stats.stackexchange.com/questions/225849/zero-inflated-model-non-finite-value-supplied-by-optim
## 

## What about a zero-inflated model?
mod.expl.2019.cgfs.zinfl <-  zeroinfl(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin") #run hurdle model 
## Does run, but with strange warning message:
## In value[[3L]](cond) : system is computationally singular: reciprocal condition number = 6.10205e-19FALSE
## https://stackoverflow.com/questions/44134711/zeroinfl-system-is-computationally-singular-whereas-no-correlation-in-predicto 
## https://stats.stackexchange.com/questions/76488/error-system-is-computationally-singular-when-running-a-glm 
summary(mod.expl.2019.cgfs.zinfl)
dat_2019_cgfs_zinfl <- dat

# Set up zero-inflated model from last year
load("NSHM/Assessment 2020/results/model/data_model_adults.RData")
mod.expl.zinfl <-  zeroinfl(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin") 
summary(mod.expl.zinfl)

# ## Save model and data
# save(mod.expl.2019, file="NSHM/Assessment 2021/results/model/model_adults_only_FRA_CGFS_2019.RData")
# dat_2019 <- dat
# save(survey_2019, dat_2019, survey_2019, EEZ_haul, file="NSHM/Assessment 2021/results/model/data_model_adults_only_FRA_CGFS.RData")
# 
# # Load model and data from 2020 assessment
# load("NSHM/Assessment 2020/results/model/model_adults.RData")
# load("NSHM/Assessment 2020/results/model/data_model_adults.RData")

# Multiply the model prediction by the weighting factors
dat$weight                 <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit                    <- predict(mod.expl.zinfl)*dat$weight
dat_2019_cgfs_zinfl$weight <- ifelse(dat_2019_cgfs_zinfl$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat_2019_cgfs_zinfl$fit    <- predict(mod.expl.2019.cgfs.zinfl)*dat_2019_cgfs_zinfl$weight

# Estimate the index for the CGFS survey
dat_CGFS                <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS           <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year      <- as.integer(as.character(survpred_CGFS$Year))
dat_CGFS_2019           <- dat_2019_cgfs_zinfl[dat_2019_cgfs_zinfl$Survey=="FR-CGFS",]
survpred_CGFS_2019      <- ddply(dat_CGFS_2019,.(Year),summarize,idx=mean(fit))
survpred_CGFS_2019$Year <- as.integer(as.character(survpred_CGFS_2019$Year))
survpred_CGFS_2019      <- rbind(survpred_CGFS_2019, data.frame(Year=2016,idx=NA))
survpred_CGFS_2019      <- survpred_CGFS_2019[order(survpred_CGFS_2019$Year),]

# Estimate the index for the IBTS survey
dat_IBTS                <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS           <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year      <- as.integer(as.character(survpred_IBTS$Year))
dat_IBTS_2019           <- dat_2019_cgfs_zinfl[dat_2019_cgfs_zinfl$Survey=="NS-IBTS",]
survpred_IBTS_2019      <- ddply(dat_IBTS_2019,.(Year),summarize,idx=mean(fit))
survpred_IBTS_2019$Year <- as.integer(as.character(survpred_IBTS_2019$Year))

# Combine output
index.expl      <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)
index.expl_2019 <- data.frame(year=survpred_CGFS_2019$Year, cgfs=survpred_CGFS_2019$idx, ibts=survpred_IBTS_2019$idx)

# Estimate the combined index as the average of the two survey indexes
index.expl$all <- rowMeans(index.expl[,!names(index.expl)=="year"])
index.expl_2019$all <- rowMeans(index.expl_2019[,!names(index.expl_2019)=="year"], na.rm = TRUE)

# Combine
index.expl$surveyEEZ <- "IBTS & CGFS 2016"
index.expl_2019$surveyEEZ <- "IBTS 2016"
index.expl.combine <- rbind(index.expl, index.expl_2019)

# Plot to compare the time series (all, CGFS index only, IBTS index only)
p <- ggplot(index.expl.combine, aes(year,all,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("Combined IBTS-CGFS index exploitable stock - zero-inflated") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_combined_no_CGFS_2016_zero_inflated.png", p, path=figpath, width=8, height = 6)

p <- ggplot(index.expl.combine, aes(year,cgfs,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("CGFS index exploitable stock - zero-inflated") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_CGFS_no_CGFS_2018_zero_inflated.png", p, path=figpath, width=8, height = 6)

p <- ggplot(index.expl.combine, aes(year,ibts,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("IBTS index exploitable stock - zero - inflated") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_IBTS_no_CGFS_2018_zero_inflated.png", p, path=figpath, width=8, height = 6)

# Extract year effects
year_eff     <- data.frame(parameter = names(coefficients(mod.expl.zinfl)), 
                           model=c(rep("count",66),rep("zero",length(coefficients(mod.expl.zinfl))-66)),
                           coefficient_FRA_UK = coefficients(mod.expl.zinfl), 
                           coefficient_2019 = coefficients(mod.expl.2019.cgfs.zinfl))

# Plot year effects to compare
p <- ggplot(year_eff, aes(coefficient_FRA_UK, coefficient_2019, colour=model)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(x="Coefficients zero-infl. model 2018 IBTS & CGFS", y="Coefficients zero-infl. model 2018 IBTS only") +
  theme_bw()
print(p)
ggsave("coefficients_CGFS_change_no_CGFS_2018_zero_inflated.png", p, path=figpath, width=8, height = 6)

p <- ggplot(year_eff, aes(coefficient_FRA_UK, coefficient_2019, colour=model)) +
  geom_point() +
  geom_text(aes(label=parameter),hjust=0, vjust=0, size=2) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x="Coefficients model 2018 FRA & UK", y="Coefficients model 2018 FRA only") +
  theme_bw()
print(p)
ggsave("coefficients_CGFS_change_no_CGFS_2018_zero_inflated_text.png", p, path=figpath, width=8, height = 6)


############################################################################################################-
##### Exclude 2018 entirely ----

# Load survey data (note: form last year, as we want to compare to last years index up to 2019)
survey <- read.csv("NSHM/Assessment 2020/results/survey/survey_data_1992-2019_clean.csv",stringsAsFactors = FALSE)

# Remove UK stations from CGFS from last year, 2019
survey_2019_cgfs <- subset(survey, !Year %in% 2017)

# Run hurdle model for exploitable stock with French CGFS data only
datos <- survey_2019_cgfs[survey_2019_cgfs$groupsize=="exploitable",] #select adults                
dat   <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos) # Sum CPUE across length classes
dim(dat);str(dat)
table(dat$CPUE>0) #check how many zeroes
dat$CPUE <- round(dat$CPUE,0) #round off CPUE values to make sure it's count data
# dat$CPUE <- as.integer(dat$CPUE)
dat$Year <- as.factor(dat$Year)
str(dat)
mod.excl.year <- hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial") #run hurdle model 
summary(mod.excl.year)
dat_2019 <- dat
## Model is running, so it can handle a missing year. 

# Load model and data from 2020 assessment
load("NSHM/Assessment 2020/results/model/model_adults.RData")
load("NSHM/Assessment 2020/results/model/data_model_adults.RData")

# Multiply the model prediction by the weighting factors
dat$weight      <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit         <- predict(mod.expl)*dat$weight
dat_2019$weight <- ifelse(dat_2019$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat_2019$fit    <- predict(mod.excl.year)*dat_2019$weight

# Estimate the index for the CGFS survey
dat_CGFS                <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS           <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year      <- as.integer(as.character(survpred_CGFS$Year))
dat_CGFS_2019           <- dat_2019[dat_2019$Survey=="FR-CGFS",]
survpred_CGFS_2019      <- ddply(dat_CGFS_2019,.(Year),summarize,idx=mean(fit))
survpred_CGFS_2019$Year <- as.integer(as.character(survpred_CGFS_2019$Year))

# Estimate the index for the IBTS survey
dat_IBTS                <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS           <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year      <- as.integer(as.character(survpred_IBTS$Year))
dat_IBTS_2019           <- dat_2019[dat_2019$Survey=="NS-IBTS",]
survpred_IBTS_2019      <- ddply(dat_IBTS_2019,.(Year),summarize,idx=mean(fit))
survpred_IBTS_2019$Year <- as.integer(as.character(survpred_IBTS_2019$Year))

# Combine output
index.expl     <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)
index.expl_2019 <- data.frame(year=survpred_CGFS_2019$Year, cgfs=survpred_CGFS_2019$idx, ibts=survpred_IBTS_2019$idx)

# Estimate the combined index as the average of the two survey indexes
index.expl$all <- rowMeans(index.expl[,!names(index.expl)=="year"])
index.expl_2019$all <- rowMeans(index.expl_2019[,!names(index.expl_2019)=="year"])

# Combine
index.expl$surveyEEZ <- "FRA & UK 2019"
index.expl_2019$surveyEEZ <- "FRA 2019"
index.expl.combine <- rbind(index.expl, index.expl_2019)

# Plot to compare the time series (all, CGFS index only, IBTS index only)
p <- ggplot(index.expl.combine, aes(year,all,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("Combined IBTS-CGFS index exploitable stock") +
  theme_bw()
print(p)
ggsave("adults_index_CGFS_change_2020_combined_2019_without_UK.png", p, path=figpath, width=8, height = 6)

p <- ggplot(index.expl.combine, aes(year,cgfs,colour=surveyEEZ)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 16) +
  labs(x="Year",y="Index exploitable stock") +
  ggtitle("CGFS index exploitable stock") +
  theme_bw()
print(p)

############################################################################################################-
##### Calculate model index for 2020 ----
# Option A: keep in FR-CGFS in 2020
# Option B: leave out FR-CGFS in 2020 and run zero-inflated model
# Option C: exclude 2020 index value

# Load survey data, incl. 2020 
survey <- read.csv("NSHM/Assessment 2021/results/survey/survey_data_1992-2020_clean.csv",stringsAsFactors = FALSE)


#### OPTION A ####

# Prepare data and run model
datos    <- survey[survey$groupsize=="exploitable",] #select adults                
dat      <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos) # Sum CPUE across length classes
dat$CPUE <- round(dat$CPUE,0) #round off CPUE values to make sure it's count data
dat$Year <- as.factor(dat$Year)
str(dat)
mod.expl.A <- hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial") #run hurdle model 
summary(mod.expl.A)

# Save
save(mod.expl.A, dat, file="NSHM/Assessment 2021/results/model/model_adults_option_A.RData")

# Multiply the model prediction by the weighting factors
dat$weight <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit    <- predict(mod.expl.A)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS           <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS      <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year <- as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS           <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS      <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year <- as.integer(as.character(survpred_IBTS$Year))

# Combine output
index.expl_A <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
index.expl_A$all <- rowMeans(index.expl_A[,!names(index.expl_A)=="year"])

# Save
write.csv(index.expl_A,"NSHM/Assessment 2021/results/model/model_fit_adults_nshm_option_A.csv",row.names=F)

# Plot average and two survey indices
png(filename="NSHM/Assessment 2021/figures/model/hurdle_model_adults_option_A.png", heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(index.expl$year,index.expl$all,type="b",pch=19,col="blue",cex=1,lwd=1.5,
     ylim=c(0,max(index.expl[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list("Option A: keep FR-CGFS 2020",cex=2),bty="l")
lines(index.expl$year,index.expl$cgfs,type="b",pch=19,col="grey40",cex=1,lwd=1.5)
lines(index.expl$year,index.expl$ibts,type="b",pch=19,col="red",cex=1,lwd=1.5)
lines(index.expl$year,index.expl$all,type="b",pch=19,col="blue",cex=1,lwd=1.5)
legend("topright",legend=c("Average", "FR-CGFS", "NS-IBTS"),col=c("blue","grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()


#### OPTION B ####

# Prepare data and run zer-inflated model
datos    <- survey[survey$groupsize=="exploitable",] #select adults
datos    <- subset(datos, !(Year %in% 2020 & Survey %in% 'FR-CGFS')) #DROP FR_CGFS IN 2020
dat      <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos) # Sum CPUE across length classes
dat$CPUE <- round(dat$CPUE,0) #round off CPUE values to make sure it's count data
dat$Year <- as.factor(dat$Year)
str(dat)
mod.expl.B <- zeroinfl(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin")
summary(mod.expl.B)

# Save
save(mod.expl.B, dat, file="NSHM/Assessment 2021/results/model/model_adults_option_B.RData")

# Multiply the model prediction by the weighting factors
dat$weight <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit    <- predict(mod.expl.B)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS           <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS      <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year <- as.integer(as.character(survpred_CGFS$Year))
survpred_CGFS      <- rbind(survpred_CGFS, data.frame(Year=2020,idx=NA))

# Estimate the index for the IBTS survey
dat_IBTS           <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS      <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year <- as.integer(as.character(survpred_IBTS$Year))

# Combine output
index.expl_B <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
index.expl_B$all <- rowMeans(index.expl[,!names(index.expl)=="year"], na.rm = TRUE)

# Save
write.csv(index.expl_B,file="NSHM/Assessment 2021/results/model/model_fit_adults_nshm_option_B.csv",row.names=F)

# Plot average and two survey indices
png(filename="NSHM/Assessment 2021/figures/model/zero_infl_model_adults_option_B.png", heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(index.expl_B$year,index.expl_B$all,type="b",pch=19,col="blue",cex=1,lwd=1.5,
     ylim=c(0,max(index.expl_B[,c(2:4)], na.rm = TRUE)),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list("Option B: drop FR-CGFS 2020",cex=2),bty="l")
lines(index.expl_B$year,index.expl_B$cgfs,type="b",pch=19,col="grey40",cex=1,lwd=1.5)
lines(index.expl_B$year,index.expl_B$ibts,type="b",pch=19,col="red",cex=1,lwd=1.5)
lines(index.expl_B$year,index.expl_B$all,type="b",pch=19,col="blue",cex=1,lwd=1.5)
legend("topright",legend=c("Average", "FR-CGFS", "NS-IBTS"),col=c("blue","grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()


#### OPTION C ####

# Load index as computed last year
index.expl_C <- read.csv("NSHM/Assessment 2020/results/model/model_fit_adults_nshm.csv", stringsAsFactors = FALSE)

#### Plot the two survey indices combined
png(filename="NSHM/Assessment 2021/figures/model/model_adults_option_A_B_C.png", heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(index.expl_A$year,index.expl_A$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5,
     ylim=c(0,max(index.expl_A[,c(2:4)], na.rm = TRUE)),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,bty="l")
lines(index.expl_B$year,index.expl_B$all,type="b",pch=19,col="blue",cex=1,lwd=1.5)
lines(index.expl_C$year,index.expl_C$all,type="b",pch=19,col="red",cex=1,lwd=1.5)
legend("topright",legend=c("A: keep FR-CGFS 2020", "B: drop FR-CGFS 2020", "C: drop 2020"),col=c("grey40","blue","red"),pch=19,bty="n",cex=1.5)
dev.off()
