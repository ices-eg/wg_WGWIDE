# Created by Alfonso Perez, adjusted by Esther Beukhof

##########################################################################################-
#################  Bootstrap survey index of Hurdle model to get CI ##########################
##########################################################################################-

rm(list=ls())

library(pscl)
library(plyr)
library(scales)

# Load cleaned survey data
data <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/survey_data_1992-2018_clean.csv",stringsAsFactors = FALSE)
# Define folder for output
resultpath="M:/My Documents/WGWIDE/Assessment 2019/NSHM/hurdle_model/output/"


#### Prepare the data for modelling
str(data)
# data.n$Survey <- as.factor(data.n$Survey)
data$Year <- as.factor(data$Year)
str(data)


#######################################################-
####  Larger than the defined cutoff length  #########
#######################################################-

datos  <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dim(dat);str(dat)

# Check how many zeroes
table(dat$CPUE>0) # bit more than 50%

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
# dat$Year <- as.factor(dat$Year)
str(dat)


# Model
#####################################################################################################################-

hurdle2=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
summary(hurdle2) 

model=hurdle2
choice= "Year x Survey & Year + Survey"
Cutoff_Lngt <- 20


# Bootstrap
#####################################################################################################################-
fit <- predict(model)
Pearson <- resid(model, type = "pearson")
VarComp <- resid(model, type = "response") / Pearson

Year=dat$Year
Survey=dat$Survey
# depthrange=dat$depthrange


# Weights parameters to multiply the fitted values. These weighting factors consider  
# both the survey area covered and the wing spread.
areaCGFS=0.24
areaIBTS=0.76
dat$percarea=ifelse(dat$Survey=="CGFS",areaCGFS,areaIBTS)

# Model prediction with confidence intervals: 999 times
boot_idx_adult_2 <- replicate(999, {
  CPUE <- pmax(round(fit + sample(Pearson) * VarComp, 0), 0)
  predict(hurdle(CPUE ~ Year * Survey | Year + Survey, link="logit", dist="negbin"), newdata = dat)*dat$percarea
})
# Add all repetitions to data frame with main data
boot_idx_adult=cbind(dat,boot_idx_adult_2)
# Save output
write.csv(boot_idx_adult,paste(resultpath,"adult_boot_idx_",choice,".csv",sep=""),row.names=F)

# Fit the model
newdata0 <- dat
newdata0$fit <- predict(model, newdata = dat, type = "response")*dat$percarea
# Add two new columns with the CI based on the bootstrapped data
newdata0[, (dim(newdata0)[2]+1):(dim(newdata0)[2]+2)] <- t(apply(boot_idx_adult_2, 1, quantile, c(0.025, 0.975)))
colnames(newdata0)=c("Survey",  "Year", "StatRec",  "HaulDur",  "HaulNo", "ShootLon", "ShootLat", "Depth",  "CPUE","percarea", "fit", "Perclow","Perchigh")
newdata0$model <- choice

# Estimate the index for the CGFS survey per year, including the confidence interval
dat_CGFS=newdata0[newdata0$Survey=="CGFS",]
survpred_CGFS=ddply(dat_CGFS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_CGFS$Year=as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey, including the confidence interval
dat_IBTS=newdata0[newdata0$Survey=="NS-IBTS",]
survpred_IBTS=ddply(dat_IBTS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_IBTS$Year=as.integer(as.character(survpred_IBTS$Year))

# Combine all in one data frame
large=data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx, cgfs_low=survpred_CGFS$CI_low, ibts_low=survpred_IBTS$CI_low,cgfs_high=survpred_CGFS$CI_high, ibts_high=survpred_IBTS$CI_high)

# Estimate the combined index as the average of the two survey indexes
large$all=rowMeans(large[,names(large) %in% c("cgfs","ibts")])
large$all_low=rowMeans(large[,names(large) %in% c("cgfs_low","ibts_low")])
large$all_high=rowMeans(large[,names(large) %in% c("cgfs_high","ibts_high")])

# Save
write.csv(large,paste(resultpath,"adult_surv_idx_",choice,".csv",sep=""),row.names=F)

# Create data frame with only the combined survey index
large_all=large[,names(large) %in% c("all","all_low","all_high")]
ymax=max(large_all)
ymin=min(large_all)

# Plot
png(filename=paste(resultpath,"adult_surv_idx_boot_",choice,".png",sep=""), heigh=1500, width=2500, units="px", pointsize=8, res=450)
plot(large$year,large$all,type="b",pch=19,col="grey40",ylim=c(ymin,ymax),
     xlab="Year", ylab="Survey index (indv/hour)",cex.lab=1.3,
     main=paste("Hurdle model >",Cutoff_Lngt,"cm  ",choice,sep=""))
polygon(c(sort(unique(large$year)),rev(sort(unique(large$year)))), 
        c(large$all_high,rev(large$all_low)), lty=0, col=alpha(2, 0.5))
dev.off()



#######################################################-
####  Smaller than the defined cutoff length  #########
#######################################################-

# Load cleaned survey data
datos  <- data[data$groupsize=="juveniles",]                 ##---when Number=0, LngtClas is set as 0
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dim(dat);str(dat)

# Check how many zeroes
table(dat$CPUE>0) # bit more than 50%

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
# dat$Year <- as.factor(dat$Year)
str(dat)


# Model
#####################################################################################################################-

hurdle2=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
summary(hurdle2) 

model=hurdle2
choice= "Year x Survey & Year + Survey"
Cutoff_Lngt <- 20


# Bootstrap
#####################################################################################################################-
fit <- predict(model)
Pearson <- resid(model, type = "pearson")
VarComp <- resid(model, type = "response") / Pearson

Year=dat$Year
Survey=dat$Survey
# depthrange=dat$depthrange


# Weights parameters to multiply the fitted values. These weighting factors consider  
# both the survey area covered and the wing spread.
areaCGFS=0.24
areaIBTS=0.76
dat$percarea=ifelse(dat$Survey=="CGFS",areaCGFS,areaIBTS)

# Model prediction with confidence intervals: 999 times
boot_idx_juv_2 <- replicate(999, {
  CPUE <- pmax(round(fit + sample(Pearson) * VarComp, 0), 0)
  predict(hurdle(CPUE ~ Year * Survey | Year + Survey, link="logit", dist="negbin"), newdata = dat)*dat$percarea
})
# Add all repetitions to data frame with main data
boot_idx_juv=cbind(dat,boot_idx_juv_2)
# Save output
write.csv(boot_idx_juv,paste(resultpath,"juvenile_boot_surv_idx_",choice,".csv",sep=""),row.names=F)

# Fit the model
newdata0 <- dat
newdata0$fit <- predict(model, newdata = dat, type = "response")*dat$percarea
# Add two new columns with the CI based on the bootstrapped data
newdata0[, (dim(newdata0)[2]+1):(dim(newdata0)[2]+2)] <- t(apply(boot_idx_juv_2, 1, quantile, c(0.025, 0.975)))
colnames(newdata0)=c("Survey",  "Year", "StatRec",  "HaulDur",  "HaulNo", "ShootLon", "ShootLat", "Depth",  "CPUE","percarea", "fit", "Perclow","Perchigh")
newdata0$model <- choice

# Estimate the index for the CGFS survey per year, including the confidence interval
dat_CGFS=newdata0[newdata0$Survey=="CGFS",]
survpred_CGFS=ddply(dat_CGFS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_CGFS$Year=as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey, including the confidence interval
dat_IBTS=newdata0[newdata0$Survey=="NS-IBTS",]
survpred_IBTS=ddply(dat_IBTS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_IBTS$Year=as.integer(as.character(survpred_IBTS$Year))

# Combine all in one data frame
small=data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx, cgfs_low=survpred_CGFS$CI_low, ibts_low=survpred_IBTS$CI_low,cgfs_high=survpred_CGFS$CI_high, ibts_high=survpred_IBTS$CI_high)

# Estimate the combined index as the average of the two survey indexes
small$all=rowMeans(small[,names(small) %in% c("cgfs","ibts")])
small$all_low=rowMeans(small[,names(small) %in% c("cgfs_low","ibts_low")])
small$all_high=rowMeans(small[,names(small) %in% c("cgfs_high","ibts_high")])

# Save
write.csv(small,paste(resultpath,"juvenile_surv_idx_",choice,".csv",sep=""),row.names=F)

# Create data frame with only the combined survey index
small_all=small[,names(small) %in% c("all","all_low","all_high")]
ymax=max(small_all)
ymin=min(small_all)

# Plot
png(filename=paste(resultpath,"juvenile_surv_idx_boot_",choice,".png",sep=""), height=1500, width=2500, units="px", pointsize=8, res=450)
plot(small$year,small$all,type="b",pch=19,col="grey40",ylim=c(ymin,ymax),
     xlab="Year", ylab="Survey index (indv/hour)",cex.lab=1.3,
     main=paste("Hurdle model <",Cutoff_Lngt,"cm  ",choice,sep=""))
polygon(c(sort(unique(small$year)),rev(sort(unique(small$year)))), 
        c(small$all_high,rev(small$all_low)), lty=0, col=alpha(2, 0.5))
dev.off()

