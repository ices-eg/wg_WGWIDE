############################################################################################################-
# Compare hurdle model of the adult substock with zero-inflated model
#
# Created by Alfonso Perez Rodriguez, adapted by Esther Beukhof
############################################################################################################-

rm(list=ls())

library(pscl)
library(ggplot2)
library(plyr)

# Set paths
datpath <- "NSHM/Assessment 2020/results/survey/"
respath <- "NSHM/Assessment 2020/results/model/"
figPath <- "NSHM/Assessment 2020/figures/model/"

# Load cleaned survey data
data <- read.csv(paste0(datpath,"survey_data_1992-2019_clean.csv"),stringsAsFactors = FALSE)

# Prepare the data for modelling
str(data)
data$Year <- as.factor(data$Year)
str(data)

# Weights parameters to multiply the fitted values. These weighting factors consider  both the survey area covered and the wing spread.
weightCGFS=0.24
weightIBTS=0.76

# Select adults
datos <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0

# Sum CPUE across length classes
dat   <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dim(dat);str(dat)

# Check how many zeroes
table(dat$CPUE>0) #around 40%

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
str(dat)


###### Hurdle model ########

# Run the hurdle model 
mod.expl <- hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
### Warning given: In sqrt(diag(vc_count)[kx + 1]) : NaNs produced
summary(mod.expl)
### Warning given: In sqrt(diag(object$vcov)) : NaNs produced
### These are the same warnings as last year when the 2018 data were added.
### Summary shows again that NAs are produced for the std error and p-value of the intercept for the count model,
### and that the theta parameter is zero.
diag(mod.expl$vcov) #doesn't show any NaNs, but negative value for the intercept -> probably causing the issue
### As the warnings say: they take the sqrt of the diagonal of the vcov, and sqrt of negative value will give indeed NaN
sqrt(diag(mod.expl$vcov))
### The model likely uses the diagonal of the vcov-matrix for estimating parameters. Due to the negative value/NaN 
### for the intercept, the model has problems with estimating the intercept coefficient.
### It remains unclear what causes this -> important to look at in the future.
### For now, continue with this model. Last year zero-inflated model also tried, this gave no issues but similar
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
index.expl <- data.frame(year=survpred_CGFS$Year, cgfs_hurdle=survpred_CGFS$idx, ibts_hurdle=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
index.expl$all_hurdle <- rowMeans(index.expl[,!names(index.expl)=="year"])



###### Zero-inflated model ########

# Run zero-inflated model for comparison, which at WKWIDE 2017 was evaluated as almost as good as the hurde model
# but higher dispersion statistic
mod.zi <- zeroinfl(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin") # with binomial for zeroes
summary(mod.zi) #no issues
## Compare parameter coefficients of the hurdle and ZI model
parm.coef <- data.frame(hurdle = coef(mod.expl), zero_infl = coef(mod.zi))
parm.coef$diff <- abs(parm.coef$hurdle - parm.coef$zero_infl) #calculate absolute difference between parameters
## Quite some differences between the zero part of the models, as shown also in plot below
parm.coef$part <- c(rep("count",44),rep("zero",nrow(parm.coef)-44))
p <- ggplot(parm.coef, aes(hurdle,zero_infl,colour=part)) + geom_point() + theme_bw()
print(p)
ggsave("Parameter coef hurdle and zero-infl model exploitable substock.png", plot = p, path = figPath)

# Multiply the model prediction by the weighting factors
dat$weight <- ifelse(dat$Survey=="FR-CGFS",weightCGFS,weightIBTS)
dat$fit    <- predict(mod.zi)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS           <- dat[dat$Survey=="FR-CGFS",]
survpred_CGFS      <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year <- as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS           <- dat[dat$Survey=="NS-IBTS",]
survpred_IBTS      <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year <- as.integer(as.character(survpred_IBTS$Year))

# Add to hurdle output
index.expl$cgfs_zeroInfl <- survpred_CGFS$idx
index.expl$ibts_zeroInfl <- survpred_IBTS$idx
index.expl$all_zeroInfl <- rowMeans(index.expl[,c("cgfs_zeroInfl","ibts_zeroInfl")])



###### Plot and compare ########

png(filename=paste(figPath,"hurdle_ZI_model_adults_all.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(index.expl$year,index.expl$all_hurdle,type="b",pch=19,col="red",cex=1,lwd=1.5,
     ylim=c(0,1500),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")
lines(index.expl$year,index.expl$all_zeroInfl,type="b",pch=19,col="grey40",cex=1,lwd=1.5)
legend("topright",legend=c("Hurdle", "Zero-inflated"),col=c("red","grey40"),pch=19,bty="n",cex=1.5)
dev.off()

png(filename=paste(figPath,"hurdle_against_ZI_adults.png",sep=""), height=2000, width=2200, units="px", pointsize=9, res=450)
plot(index.expl$all_hurdle, index.expl$all_zeroInfl, pch=19, main="Exploitable substock hurdle vs. zero-inflated",
     xlab="Abundance index hurdle model", ylab="Abundance index zero-inflated model")
abline(0,1,col="red", add=TRUE)
dev.off()
