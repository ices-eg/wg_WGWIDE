############################################################################################################-
# Hurdle model to estimate abundance index NSHOM based on NS-IBTS Q3 and FR-CGFS Q4
#
# Created by Alfonso Perez Rodriguez, adapted by Esther Beukhof
############################################################################################################-

rm(list=ls())

library(pscl)
library(ggplot2)
library(plyr)

# Set paths
datpath <- "NSHM/Assessment 2021/results/survey/"
respath <- "NSHM/Assessment 2021/results/model/"
figPath <- "NSHM/Assessment 2021/figures/model/"

# Load cleaned survey data
data <- read.csv(paste0(datpath,"survey_data_1992-2020_clean.csv"),stringsAsFactors = FALSE)

# Prepare the data for modelling
str(data)
data$Year <- as.factor(data$Year)
str(data)

# Due to COVID-19 impact, UK stations could not be visited during FR-CGFS 2020 and no reliable index value
# for 2020 can be estimated (option C chosen, see also b2 script). Therefore, exclude 2020 data
data <- subset(data, !Year %in% 2020)

# Weights parameters to multiply the fitted values. These weighting factors consider both the survey area 
# covered and the wing spread.
weightCGFS=0.24 #CGFS covers 14% of total NSHOM area, and up to 2014 gear was 52.6% of the IBTS wingspread
weightIBTS=0.76 #IBTS covers 86% of total NSHOM area, and had larger wingspread than CGFS up to 2014


#############################################################-
##### Exploitable substock: large/adult fish (>20 cm)  ######
#############################################################-

# Select adults
datos <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0

# Sum CPUE across length classes
dat   <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos, na.rm=TRUE)
dim(dat);str(dat)

# Check how many zeroes
table(dat$CPUE>0)

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
str(dat)

# Run the hurdle model 
mod.expl <- hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
### Warning given: In sqrt(diag(vc_count)[kx + 1]) : NaNs produced
summary(mod.expl)
### Warning given: In sqrt(diag(object$vcov)) : NaNs produced
### These are the same warnings as last years when the 2018 and 2019 data were added.
### Summary shows again that NAs are produced for the std error and p-value of the intercept for the count model,
### and that the theta parameter is zero.
diag(mod.expl$vcov) #doesn't show any NaNs, but negative value for the intercept -> probably causing the issue
### As the warnings say: they take the sqrt of the diagonal of the vcov, and sqrt of negative value will give indeed NaN
sqrt(diag(mod.expl$vcov))
### The model likely uses the diagonal of the vcov-matrix for estimating parameters. Due to the negative value/NaN 
### for the intercept, the model has problems with estimating the intercept coefficient.
### It remains unclear what causes this -> important to look at in the future.
### For now, continue with this model. Last two years zero-inflated model also tried, this gave no issues but similar
### parameter coefficients. The current warnings most likely does not affect the parameter coefficients.

# Multiply the model prediction by the weighting factors
dat$weight <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit    <- predict(mod.expl)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS           <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS      <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year <- as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS           <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS      <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year <- as.integer(as.character(survpred_IBTS$Year))

# Combine output
index.expl <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
index.expl$all <- rowMeans(index.expl[,!names(index.expl)=="year"])

# Add 2020 values as NA
index.expl <- rbind(index.expl, data.frame(year=2020, cgfs=NA, ibts=NA, all=NA))

# Save
write.csv(index.expl,paste0(respath,"model_fit_adults_nshm.csv"),row.names=F)
save(mod.expl, file=paste0(respath,"model_adults.RData"))
save(dat, file=paste0(respath,"data_model_adults.RData"))

# Plot average and two survey indices
png(filename=paste(figPath,"hurdle_model_adults.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(index.expl$year,index.expl$cgfs,type="b",pch=19,col="grey40",cex=1,lwd=1.5,
     ylim=c(0,max(index.expl[,c(2:4)], na.rm=TRUE)),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")
lines(index.expl$year,index.expl$ibts,type="b",pch=19,col="red",cex=1,lwd=1.5)
lines(index.expl$year,index.expl$all,type="b",pch=19,col="blue",cex=1,lwd=1.5)
legend("topright",legend=c("Average", "FR-CGFS", "NS-IBTS"),col=c("blue","grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()


# Plot index together with one from previous year
index.expl      <-read.csv(paste0(respath,"model_fit_adults_nshm.csv")) #load this year's index if not yet loaded
index.expl.last <-read.csv("NSHM/Assessment 2020/results/model/model_fit_adults_nshm.csv") #load last year's index 
xlim            <- c(min(index.expl$year), max(index.expl$year)) #define limits x-axis

png(filename=paste(figPath,"hurdle_model_adults_compare_last_year.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(index.expl.last$year,index.expl.last$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5, xlim=xlim,
     ylim=c(0,max(index.expl.last$all, na.rm=TRUE)),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")
lines(index.expl$year,index.expl$all,type="b",pch=19,col="red",cex=1,lwd=1.5)
legend("topright",legend=c("WGWIDE2020", "WGWIDE2021"),col=c("grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()


# Compare coefficients with last year
mod.expl.2021 <- mod.expl
load("NSHM/Assessment 2020/results/model/model_adults.RData")
mod.expl.2020 <- mod.expl

year_eff_2021    <- data.frame(parameter = names(coefficients(mod.expl.2021)), 
                           model=c(rep("count",66),rep("zero",length(coefficients(mod.expl.2021))-66)),
                           coefficient_2021 = coefficients(mod.expl.2021))
year_eff_2020    <- data.frame(parameter = names(coefficients(mod.expl.2020)), 
                               model=c(rep("count",57),rep("zero",length(coefficients(mod.expl.2020))-57)),
                               coefficient_2020 = coefficients(mod.expl.2020))
year_eff         <- merge(year_eff_2020, year_eff_2021, by=c("parameter","model"), all.y=TRUE)
year_eff$Diff    <- year_eff$coefficient_2021 - year_eff$coefficient_2020

png(filename=paste(figPath,"hurdle_model_adults_coefficients_compare_last_year.png",sep=""), height=1300, width=1500, units="px", pointsize=10, res=300)
par(mar=c(5,5,3,1))
plot(year_eff$coefficient_2020, year_eff$coefficient_2021, 
     xlab="Coefficients model WGWIDE 2020", ylab = "Coefficients model WGWIDE 2021")
abline(0,1, col="red")
text("Year2016:SurveyNS-IBTS",x = -1.5, y = 0.7)
dev.off()


#######################################################-
#####   Juvenile substock: small fish (<20 cm)   ######
#######################################################-

# Select juveniles
datos <- data[data$groupsize=="juveniles",]

# Sum CPUE across length classes
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dim(dat)

# Check how many zeroes
table(dat$CPUE>0) #around 44%

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
str(dat)

# Run the hurdle model
mod.juv <- hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
summary(mod.juv) #same warning as for adult model (this was also the case in 2020 assessment)

# Multiply the model prediction by the weighting factors
dat$weight <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit    <- predict(mod.juv)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS           <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS      <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year <- as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS           <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS      <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year <- as.integer(as.character(survpred_IBTS$Year))

# Combine output
index.juv <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
index.juv$all <- rowMeans(index.juv[,!names(index.juv)=="year"])

# Add 2020 values as NA
index.juv <- rbind(index.juv, data.frame(year=2020, cgfs=NA, ibts=NA, all=NA))

# Save
write.csv(index.juv,paste(respath,"model_fit_juv_nshm.csv",sep=""),row.names=F)
save(mod.juv, file=paste0(respath,"model_juv.RData"))
save(dat, file=paste0(respath,"data_model_juv.RData"))

# Plot average and two survey indices 
png(filename=paste(figPath,"hurdle_model_juveniles.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(index.juv$year,index.juv$cgfs,type="b",pch=19,col="grey40",cex=1,lwd=1.5,ylim=c(0,max(index.juv[,c(2:4)], na.rm=TRUE)),xlab="Year",
     ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list("<20cm substock",cex=2),bty="l")
lines(index.juv$year,index.juv$ibts,type="b",pch=19,col="red",cex=1,lwd=1.5)
lines(index.juv$year,index.juv$all,type="b",pch=19,col="blue",cex=1,lwd=1.5)
legend("top",legend=c("Average", "FR-CGFS", "NS-IBTS"),col=c("blue","grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()
