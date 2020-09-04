############################################################################################################-
# Plot length frequency data
#
# Created by Benoit Berges, adapted by Esther Beukhof
############################################################################################################-

rm(list=ls())

library(reshape2)
library(ggplot2)

# Set paths
dataPath <- "NSHM/Assessment 2020/results/length/"
resPath  <- "NSHM/Assessment 2020/results/length/"
figPath  <- "NSHM/Assessment 2020/figures/length/"


########################## InterCatch length frequencies ########################## 

# Load this years LF data
LF <- read.csv(paste0(dataPath,"NSHM_LF_area_2019.csv"), stringsAsFactors = FALSE)
colnames(LF)[c(2:5)] <- c("27.7.d","27.4.a","27.4.b","27.4.c")

# Put into long format
LF_ <- melt(LF, id.vars = "length_class", value.name = "prop", variable.name = "area")
colnames(LF_)[c(2,3)] <- c("area","prop")
LF_$area <- as.character(LF_$area)

# Plot
p <- ggplot(LF_, aes(length_class,prop)) + 
  geom_col(width = 0.5, fill="red") + 
  theme_bw() +
  labs(x="Length (cm)", y="Proportional number of fish", title="NSHM length frequency catches 2019") +
  facet_wrap(~area, nrow=4)
print(p)            
ggsave("LF_2019_area.png",plot = p, path = figPath, width = 5, height = 7)

# Load historic data
LF_2016 <- read.csv("NSHM/Assessment 2020/data/length/InterCatch/historic/NSHM_LF_area_2016.csv", stringsAsFactors = FALSE)
LF_2017 <- read.csv("NSHM/Assessment 2020/data/length/InterCatch/historic/NSHM_LF_area_2017.csv", stringsAsFactors = FALSE)
LF_2018 <- read.csv("NSHM/Assessment 2020/data/length/InterCatch/historic/NSHM_LF_area_2018.csv", stringsAsFactors = FALSE)
colnames(LF_2016)[2] <- "27.7.d"
colnames(LF_2017)[2] <- "27.7.d"
colnames(LF_2018)[c(2:3)] <- c("27.7.d","27.4.a")

# Put into long format, add year column and combine with most recent year
LF_2016_ <- melt(LF_2016, id.vars = "length_class", value.name = "prop", variable.name = "area")
colnames(LF_2016_)[c(2,3)] <- c("area","prop")
LF_2016_$year <- 2016

LF_2017_ <- melt(LF_2017, id.vars = "length_class", value.name = "prop", variable.name = "area")
colnames(LF_2017_)[c(2,3)] <- c("area","prop")
LF_2017_$year <- 2017

LF_2018_ <- melt(LF_2018, id.vars = "length_class", value.name = "prop", variable.name = "area")
colnames(LF_2018_)[c(2,3)] <- c("area","prop")
LF_2018_$year <- 2018

LF_all <- LF_
LF_all$year <- 2019
LF_all <- rbind(LF_all,LF_2018_,LF_2017_,LF_2016_)

# Plot by area and year
LF_all_7d <- subset(LF_all, area %in% "27.7.d")
p <- ggplot(LF_all_7d, aes(length_class,prop)) + 
  geom_col(width = 0.5, fill="red") + 
  theme_bw() +
  labs(x="Length (cm)", y="Proportional number of fish", title="NSHM length frequency catches 27.7.d") +
  facet_wrap(~year, ncol=1)
print(p)            
ggsave("LF_years_7d.png",plot = p, path = figPath, width = 5, height = 7)

LF_all_4a <- subset(LF_all, area %in% "27.4.a")
p <- ggplot(LF_all_4a, aes(length_class,prop)) + 
  geom_col(width = 0.5, fill="red") + 
  theme_bw() +
  labs(x="Length (cm)", y="Proportional number of fish", title="NSHM length frequency catches 27.4.a") +
  facet_wrap(~year, ncol=1)
print(p)            
ggsave("LF_years_4a.png",plot = p, path = figPath,  width = 4, height = 5)


# Calculate mean length for each year in 7.d
years <- sort(unique(LF_all_7d$year))
meanLength_7d <- data.frame(year=years, length=rep(NA,length(years)))
for(i in 1:length(years)){
  dat <- subset(LF_all_7d, year %in% years[i])
  meanLength_7d[i,2] <- sum(dat$length_class * dat$prop)
}

# Calculate mean length for each year in 4.a
years <- sort(unique(LF_all_4a$year))
meanLength_4a <- data.frame(year=years, length=rep(NA,length(years)))
for(i in 1:length(years)){
  dat <- subset(LF_all_4a, year %in% years[i])
  meanLength_4a[i,2] <- sum(dat$length_class * dat$prop)
}

########################## PFA length frequencies ########################## 

# Load this years LF data
LF_pfa <- read.csv(paste0(dataPath,"NSHM_LF_area_PFA_2019.csv"), stringsAsFactors = FALSE)
colnames(LF_pfa)[c(2:3)] <- c("27.7.d","27.4.c")

# Put into long format
LF_pfa_ <- melt(LF_pfa, id.vars = "length_class", value.name = "prop", variable.name = "area")
colnames(LF_pfa_)[c(2,3)] <- c("area","prop")
LF_pfa_$area <- as.character(LF_pfa_$area)

# Plot
p <- ggplot(LF_pfa_, aes(length_class,prop)) + 
  geom_col(width = 0.5, fill="red") + 
  theme_bw() +
  labs(x="Length (cm)", y="Proportional number of fish", title="NSHM length frequency PFA catches 2019") +
  facet_wrap(~area, nrow=4)
print(p)            
ggsave("LF_2019_PFA_area.png",plot = p, path = figPath, width = 4, height = 5)

# Load historic data
LF_pfa_2016 <- read.csv("NSHM/Assessment 2020/data/length/PFA/historic/NSHM_LF_area_PFA_2016.csv", stringsAsFactors = FALSE)
LF_pfa_2017 <- read.csv("NSHM/Assessment 2020/data/length/PFA/historic/NSHM_LF_area_PFA_2017.csv", stringsAsFactors = FALSE)
LF_pfa_2018 <- read.csv("NSHM/Assessment 2020/data/length/PFA/historic/NSHM_LF_area_PFA_2018.csv", stringsAsFactors = FALSE)
colnames(LF_pfa_2016)[2] <- "27.7.d"
colnames(LF_pfa_2017)[2] <- "27.7.d"
colnames(LF_pfa_2018)[2] <- "27.7.d"

# Put into long format, add year column and combine with most recent year
LF_pfa_2016_ <- melt(LF_pfa_2016, id.vars = "length_class", value.name = "prop", variable.name = "area")
colnames(LF_pfa_2016_)[c(2,3)] <- c("area","prop")
LF_pfa_2016_$year <- 2016

LF_pfa_2017_ <- melt(LF_pfa_2017, id.vars = "length_class", value.name = "prop", variable.name = "area")
colnames(LF_pfa_2017_)[c(2,3)] <- c("area","prop")
LF_pfa_2017_$year <- 2017

LF_pfa_2018_ <- melt(LF_pfa_2018, id.vars = "length_class", value.name = "prop", variable.name = "area")
colnames(LF_pfa_2018_)[c(2,3)] <- c("area","prop")
LF_pfa_2018_$year <- 2018

LF_pfa_all <- LF_pfa_
LF_pfa_all$year <- 2019
LF_pfa_all <- rbind(LF_pfa_all,LF_pfa_2018_,LF_pfa_2017_,LF_pfa_2016_)

# Plot by area and year
LF_pfa_all_7d <- subset(LF_pfa_all, area %in% "27.7.d")
p <- ggplot(LF_pfa_all_7d, aes(length_class,prop)) + 
  geom_col(width = 0.5, fill="red") + 
  theme_bw() +
  labs(x="Length (cm)", y="Proportional number of fish", title="NSHM length frequency PFA catches 27.7.d") +
  facet_wrap(~year, ncol=1)
print(p)            
ggsave("LF_PFA_years_7d.png",plot = p, path = figPath, width = 5, height = 7)

# Calculate mean length for each year in 7.d
years <- sort(unique(LF_pfa_all_7d$year))
meanLength_pfa_7d <- data.frame(year=years, length=rep(NA,length(years)))
for(i in 1:length(years)){
  dat <- subset(LF_pfa_all_7d, year %in% years[i])
  meanLength_pfa_7d[i,2] <- sum(dat$length_class * dat$prop)
}



########################## Compare IC and PFA length frequencies 7.d ########################## 

# Combine the IC and PFA data
LF_all_7d$Source     <- "InterCatch"
LF_pfa_all_7d$Source <- "PFA"
LF_7d_compare        <- rbind(LF_all_7d, LF_pfa_all_7d)

# Plot by area and year
p <- ggplot(LF_7d_compare, aes(length_class,prop, fill=Source)) + 
  geom_col(position = position_dodge()) + 
  theme_bw() + theme(title = element_text(size=8)) +
  labs(x="Length (cm)", y="Proportional number of fish", title="NSHM length frequency InterCatch and PFA catches 27.7.d") +
  facet_wrap(~year, ncol=1)
print(p)            
ggsave("LF_compare_years_7d.png",plot = p, path = figPath, width = 6, height = 7)
