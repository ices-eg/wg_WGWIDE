############################################################################################################-
# Calculate the proportion of zeroes in each survey and plot
#
# Created by Alfonso Perez Rodriguez, adapted by Esther Beukhof
############################################################################################################-

rm(list=ls())

library(plyr)
library(ggplot2)

# Set paths
datpath    <- "NSHM/Assessment 2021/results/survey/"
outpath    <- "NSHM/Assessment 2021/results/model/"
figPath    <- "NSHM/Assessment 2021/figures/survey/"

# Load cleaned survey data
survey <- read.csv(paste0(datpath,"survey_data_1992-2020_clean.csv"),stringsAsFactors = FALSE)

# Summarize data by summing number of fish in each haul
dat <- ddply(survey,.(Year,Survey,HaulID,groupsize),summarize,Number=sum(Number))
head(dat)

# Turn numbers to binary (basically presence/absence)
dat$Count <- ifelse(dat$Number==0 ,0,1)

# Sum the presences across all hauls per year
res       <- ddply(dat,.(groupsize,Survey,Year),summarize,total=length(Count),presence=sum(Count))

# Calculate the proportion of hauls with presence of NSHOM for each survey
res$propzero <- 1-(res$presence/res$total)

# Save
write.csv(res,paste(outpath,"Proportion_Zeros_Surveys.csv",sep=""), row.names=F)

# Plot
p <- ggplot(res, aes(Year, propzero, colour=Survey)) + 
  geom_line() + geom_point() + 
  theme_bw() +
  labs(y="Proportion of hauls with absence of NSHOM") +
  facet_wrap(~groupsize)
ggsave("Proportion_Zeros_Surveys.png",plot = p, path = figPath, width=10, height=5)
