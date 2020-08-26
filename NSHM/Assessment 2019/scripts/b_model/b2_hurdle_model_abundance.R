# Created by Alfonso Perez, adjusted by Esther Beukhof

##########################################################################################-
#################  HURDLE models ##########################
##########################################################################################-

rm(list=ls())

library(pscl)

#datpath="M:/git/wg_WGWIDE/NSHM/data/"
#resultpath="M:/git/wg_WGWIDE/NSHM/results/"
#figPath <- "M:/git/wg_WGWIDE/NSHM/figures/model/"

datpath="NSHM/Assessment 2019/data/"
resultpath="C:/git/wg_WGWIDE/NSHM/results/"
figPath <- "C:/git/wg_WGWIDE/NSHM/figures/model/"

# Load cleaned survey data
#data <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/survey_data_1992-2018_clean.csv",stringsAsFactors = FALSE)
data <- read.csv(paste0(datpath,"survey_data_1992-2018_clean.csv",sep=""),stringsAsFactors = FALSE)


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


### Hurdle model: gives warning!
hurdle2=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
# aic2=AIC(hurdle2)
summary(hurdle2) # this gives a warning!!
vcov <- data.frame(hurdle2$vcov)
# Calculate dispersion statistic
chi2 <- sum(resid(hurdle2)^2)
df <- hurdle2$df.residual
dis.st <- chi2/df # 1.09. with Poisson: 34, so much higher and not good in dealing with overdispersion


# Weights parameters to multiply the fitted values. These weighting factors consider  both the survey area covered and the wing spread.
weightCGFS=0.24
weightIBTS=0.76

dat$weight=ifelse(dat$Survey=="CGFS",weightCGFS,weightIBTS)

# Multiply the model prediction by the weighting factors
dat$fit=predict(hurdle2)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS=dat[dat$Survey=="CGFS",]
survpred_CGFS=ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year=as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS=dat[dat$Survey=="NS-IBTS",]
survpred_IBTS=ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year=as.integer(as.character(survpred_IBTS$Year))


large=data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
large$all=rowMeans(large[,!names(large)=="year"])


write.csv(large,paste(resultpath,"model_fit_large_nshm.csv",sep=""),row.names=F)

### Plot average and two survey indices ----
png(filename=paste(figPath,"hurdle_model_adults.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(large$year,large$all,type="b",pch=19,col="blue",cex=1,lwd=1.5,ylim=c(0,max(large[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")
lines(large$year,large$cgfs,type="b",pch=19,col="grey40",cex=1,lwd=1.5)
lines(large$year,large$ibts,type="b",pch=19,col="red",cex=1,lwd=1.5)
legend("topright",legend=c("Average", "CGFS", "IBTS"),col=c("blue","grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()


png(filename=paste(figPath,"hurdle_model_adults_oldColours.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(large$year,large$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5,ylim=c(0,max(large[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list(">20cm substock",cex=2),bty="l")
lines(large$year,large$cgfs,type="b",pch=19,col="red",cex=1,lwd=1.5)
lines(large$year,large$ibts,type="b",pch=19,col="blue",cex=1,lwd=1.5)
legend("topright",legend=c("all surveys", "CGFS", "IBTS"),col=c("grey40","red","blue"),pch=19,bty="n",cex=1.5)
dev.off()


### Plot joint survey index with bootstrapped confidence interval ----
model=hurdle2
choice= "Year x Survey & Year + Survey"

# Bootstrap
fit <- predict(model)
Pearson <- resid(model, type = "pearson")
VarComp <- resid(model, type = "response") / Pearson

Year=dat$Year
Survey=dat$Survey
depthrange=dat$depthrange


# Weights parameters to multiply the fitted values. These weighting factors consider  both the survey area covered and the wing spread.
areaCGFS=0.24
areaIBTS=0.76
dat$percarea=ifelse(dat$Survey=="CGFS",areaCGFS,areaIBTS)


#######################################################-
####  Smaller than the defined cutoff length  #########
#######################################################-

datos  <- data[data$groupsize=="juveniles",]
dat <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)
dim(dat)

# Check how many zeroes
table(dat$CPUE>0) # bit more than 50%

# Round off CPUE values to make sure it's count data
dat$CPUE <- round(dat$CPUE,0)
# dat$Year <- as.factor(dat$Year)
str(dat)


# Models
hurdle.juv=hurdle(CPUE ~ Year * Survey | Year + Survey, data = dat, dist = "negbin", zero.dist = "binomial")
summary(hurdle.juv)


# Weights parameters to multiply the fitted values. These weighting factors consider  both the survey area covered and the wing spread.
weightCGFS=0.24
weightIBTS=0.76

dat$weight=ifelse(dat$Survey=="CGFS",weightCGFS,weightIBTS)

# Multiply the model prediction by the weighting factors
dat$fit=predict(hurdle.juv)*dat$weight

# Estimate the index for the CGFS survey
dat_CGFS=dat[dat$Survey=="CGFS",]
survpred_CGFS=ddply(dat_CGFS,.(Year),summarize,idx=mean(fit))
survpred_CGFS$Year=as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey
dat_IBTS=dat[dat$Survey=="NS-IBTS",]
survpred_IBTS=ddply(dat_IBTS,.(Year),summarize,idx=mean(fit))
survpred_IBTS$Year=as.integer(as.character(survpred_IBTS$Year))

small=data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx)

# Estimate the combined index as the average of the two survey indexes
small$all=rowMeans(small[,!names(small)=="year"])


write.csv(small,paste(resultpath,"model_fit_small_nshm.csv",sep=""),row.names=F)


### Plot average and two survey indices ----
png(filename=paste(figPath,"hurdle_model_juveniles.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(small$year,small$all,type="b",pch=19,col="blue",cex=1,lwd=1.5,ylim=c(0,max(small[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list("<20cm substock",cex=2),bty="l")
lines(small$year,small$cgfs,type="b",pch=19,col="grey40",cex=1,lwd=1.5)
lines(small$year,small$ibts,type="b",pch=19,col="red",cex=1,lwd=1.5)
legend("topright",legend=c("Average", "CGFS", "IBTS"),col=c("blue","grey40","red"),pch=19,bty="n",cex=1.5)
dev.off()

png(filename=paste(figPath,"hurdle_model_juveniles_oldColours.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
par(mar=c(5,5,3,1))
plot(small$year,small$all,type="b",pch=19,col="grey40",cex=1,lwd=1.5,ylim=c(0,max(small[,c(2:4)])),xlab="Year",ylab="mean CPUE index",cex.axis=1.5, cex.lab=1.7,main=list("<20cm substock",cex=2),bty="l")
lines(small$year,small$cgfs,type="b",pch=19,col="red",cex=1,lwd=1.5)
lines(small$year,small$ibts,type="b",pch=19,col="blue",cex=1,lwd=1.5)
legend("topright",legend=c("all surveys", "CGFS", "IBTS"),col=c("grey40","red","blue"),pch=19,bty="n",cex=1.5)
dev.off()


