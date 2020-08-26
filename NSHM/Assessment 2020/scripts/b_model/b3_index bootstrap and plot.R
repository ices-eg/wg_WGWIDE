############################################################################################################-
# Bootstrap survey index of Hurdle model to get confidence interval
#
# Created by Alfonso Perez Rodriguez, adapted by Esther Beukhof
############################################################################################################-

rm(list=ls())

library(pscl)
library(plyr)
library(scales)

# Set paths
datpath <- "NSHM/Assessment 2020/results/model/"
respath <- "NSHM/Assessment 2020/results/model/"
figPath <- "NSHM/Assessment 2020/figures/model/"

# Weights parameters to multiply the fitted values. These weighting factors consider both the survey area covered and the wing spread.
areaCGFS=0.24
areaIBTS=0.76


#############################################################-
##### Exploitable substock: large/adult fish (>20 cm)  ######
#############################################################-

# Load data and model
index.expl <- read.csv(paste0(datpath,"model_fit_adults_nshm.csv"), stringsAsFactors = FALSE)
load(paste0(datpath,"model_adults.RData"))
load(paste0(datpath,"data_model_adults.RData"))

# Add weights to the data
dat$percarea=ifelse(dat$Survey=="FR-CGFS",areaCGFS,areaIBTS)

# Prepare for bootstrapping
fit <- predict(mod.expl)
Pearson <- resid(mod.expl, type = "pearson")
VarComp <- resid(mod.expl, type = "response") / Pearson
Year   <- dat$Year
Survey <- dat$Survey

# Model prediction with confidence intervals: 999 times
boot_idx_adult_ <- replicate(999, {
  CPUE <- pmax(round(fit + sample(Pearson) * VarComp, 0), 0)
  predict(hurdle(CPUE ~ Year * Survey | Year + Survey, link="logit", dist="negbin"), newdata = dat)*dat$percarea
})

# Add all repetitions to data frame with main data
boot_idx_adult <- cbind(dat,boot_idx_adult_)

# Save output
write.csv(boot_idx_adult, paste0(respath,"adult_bootstr_index.csv"),row.names=F)

# Fit the model
newdata0     <- dat
newdata0$fit <- predict(mod.expl, newdata = dat, type = "response")*dat$percarea

# Add two new columns with the CI based on the bootstrapped data
newdata0[, (dim(newdata0)[2]+1):(dim(newdata0)[2]+2)] <- t(apply(boot_idx_adult_, 1, quantile, c(0.025, 0.975)))
colnames(newdata0) <- c("Survey",  "Year", "StatRec",  "HaulDur",  "HaulID", "ShootLon", "ShootLat", "Depth",  "CPUE","percarea", "fit", "Perclow","Perchigh")


# Estimate the index for the CGFS survey per year, including the confidence interval
dat_CGFS           <- newdata0[newdata0$Survey=="FR-CGFS",]
survpred_CGFS      <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_CGFS$Year <- as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey, including the confidence interval
dat_IBTS           <- newdata0[newdata0$Survey=="NS-IBTS",]
survpred_IBTS      <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_IBTS$Year <- as.integer(as.character(survpred_IBTS$Year))

# Combine all in one data frame
index.expl <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx, 
                         cgfs_low=survpred_CGFS$CI_low, ibts_low=survpred_IBTS$CI_low,
                         cgfs_high=survpred_CGFS$CI_high, ibts_high=survpred_IBTS$CI_high)

# Estimate the combined index as the average of the two survey indexes
index.expl$all      <- rowMeans(index.expl[,names(index.expl) %in% c("cgfs","ibts")])
index.expl$all_low  <- rowMeans(index.expl[,names(index.expl) %in% c("cgfs_low","ibts_low")])
index.expl$all_high <- rowMeans(index.expl[,names(index.expl) %in% c("cgfs_high","ibts_high")])

# Save
write.csv(index.expl, paste0(respath,"adult_surv_idx.csv"),row.names=F)

# Plot
ylim <- c(min(index.expl[,c("all","all_low","all_high")]), max(index.expl[,c("all","all_low","all_high")])) #define limits y-axis

png(filename=paste0(figPath,"adult_surv_idx_boot.png"), height=1700, width=2500, units="px", pointsize=8, res=450)
plot(as.numeric(index.expl$year), index.expl$all,type="b",pch=19,col="grey40",ylim=ylim,
     xlab="Year", ylab="Survey index (no./hour)",cex.lab=1.3,
     main="Abundance index exploitable substock")
polygon(c(sort(unique(index.expl$year)),rev(sort(unique(index.expl$year)))), 
        c(index.expl$all_high,rev(index.expl$all_low)), lty=0, col=alpha(2, 0.5))
dev.off()



#############################################################-
##### Juvenile substock: fish (>20 cm)  ######
#############################################################-

# Load data and model
index.juv <- read.csv(paste0(datpath,"model_fit_juv_nshm.csv"), stringsAsFactors = FALSE)
load(paste0(datpath,"model_juv.RData"))
load(paste0(datpath,"data_model_juv.RData"))

# Add weights to the data
dat$percarea=ifelse(dat$Survey=="FR-CGFS",areaCGFS,areaIBTS)

# Prepare for bootstrapping
fit     <- predict(mod.juv)
Pearson <- resid(mod.juv, type = "pearson")
VarComp <- resid(mod.juv, type = "response") / Pearson
Year    <- dat$Year
Survey  <- dat$Survey

# Model prediction with confidence intervals: 999 times
boot_idx_juv_ <- replicate(999, {
  CPUE <- pmax(round(fit + sample(Pearson) * VarComp, 0), 0)
  predict(hurdle(CPUE ~ Year * Survey | Year + Survey, link="logit", dist="negbin"), newdata = dat)*dat$percarea
})

# Add all repetitions to data frame with main data
boot_idx_juv <- cbind(dat,boot_idx_juv_)

# Save output
write.csv(boot_idx_juv, paste0(respath,"juv_bootstr_index.csv"),row.names=F)

# Fit the model
newdata0     <- dat
newdata0$fit <- predict(mod.juv, newdata = dat, type = "response")*dat$percarea

# Add two new columns with the CI based on the bootstrapped data
newdata0[, (dim(newdata0)[2]+1):(dim(newdata0)[2]+2)] <- t(apply(boot_idx_juv_, 1, quantile, c(0.025, 0.975)))
colnames(newdata0) <- c("Survey",  "Year", "StatRec",  "HaulDur",  "HaulID", "ShootLon", "ShootLat", "Depth",  "CPUE","percarea", "fit", "Perclow","Perchigh")

# Estimate the index for the CGFS survey per year, including the confidence interval
dat_CGFS           <- newdata0[newdata0$Survey=="FR-CGFS",]
survpred_CGFS      <- ddply(dat_CGFS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_CGFS$Year <- as.integer(as.character(survpred_CGFS$Year))

# Estimate the index for the IBTS survey, including the confidence interval
dat_IBTS           <- newdata0[newdata0$Survey=="NS-IBTS",]
survpred_IBTS      <- ddply(dat_IBTS,.(Year),summarize,idx=mean(fit),CI_low=mean(Perclow),CI_high=mean(Perchigh))
survpred_IBTS$Year <- as.integer(as.character(survpred_IBTS$Year))

# Combine all in one data frame
index.juv <- data.frame(year=survpred_CGFS$Year, cgfs=survpred_CGFS$idx, ibts=survpred_IBTS$idx, 
                         cgfs_low=survpred_CGFS$CI_low, ibts_low=survpred_IBTS$CI_low,
                         cgfs_high=survpred_CGFS$CI_high, ibts_high=survpred_IBTS$CI_high)

# Estimate the combined index as the average of the two survey indexes
index.juv$all      <- rowMeans(index.juv[,names(index.juv) %in% c("cgfs","ibts")])
index.juv$all_low  <- rowMeans(index.juv[,names(index.juv) %in% c("cgfs_low","ibts_low")])
index.juv$all_high <- rowMeans(index.juv[,names(index.juv) %in% c("cgfs_high","ibts_high")])

# Save
write.csv(index.juv, paste0(respath,"juv_surv_idx.csv"),row.names=F)

# Plot
ylim <- c(min(index.juv[,c("all","all_low","all_high")]), max(index.juv[,c("all","all_low","all_high")])) #define limits y-axis

png(filename=paste0(figPath,"juv_surv_idx_boot.png"), height=1700, width=2500, units="px", pointsize=8, res=450)
plot(as.numeric(index.juv$year), index.juv$all,type="b",pch=19,col="grey40",ylim=ylim,
     xlab="Year", ylab="Survey index (no./hour)",cex.lab=1.3,
     main="Abundance index juvenile substock")
polygon(c(sort(unique(index.juv$year)),rev(sort(unique(index.juv$year)))), 
        c(index.juv$all_high,rev(index.juv$all_low)), lty=0, col=alpha(2, 0.5))
dev.off()


