############################################################################################################-
# Plot survey data 
#
# Created by Alfonso Pérez Rodríguez, adapted by Esther Beukhof
############################################################################################################-

library(ggplot2)
library(grid)
library(gridExtra)
library(maps)

# Set paths
figpath <- "NSHM/Assessment 2020/figures/survey/"
outpath <- "NSHM/Assessment 2020/results/survey/"

# Load data
load("NSHM/Assessment 2020/results/survey/survey_data_1992-2018_clean.RData")

# Set years
Years <- c(2014:2019)

# Set latitude and longitude for map
range(survey_exp$ShootLat)
range(survey_exp$ShootLong)
limx <- c(-5,10)
limy <- c(49,58)
# map  <- get_map(location = c(lon = 3.2, lat = 53.5), zoom = 6, maptype = c("satellite"), color = c("color"), crop=T)


############################################################################################################-
##### LENGTH DISTRIBUTIONS -----

# 1. Length frequency distriubtion NS-IBTS ----

# Select IBTS data
dataIBTS <- survey_exp[survey_exp$Survey=="NS-IBTS",]

# Length freq in relative numbers
lengthFreq <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClass>0], by=list(dataIBTS$LngtClass[dataIBTS$LngtClass>0]), 
                        FUN=sum)$x/sum(dataIBTS$CPUE[dataIBTS$LngtClass>0],na.rm=T)

names(lengthFreq)   <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClass>0], by=list(dataIBTS$LngtClass[dataIBTS$LngtClass>0]), FUN=sum)$Group.1

png(filename=paste(figpath,"IBTS_length_relat_freq.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
barplot(lengthFreq, las=1, col="red", border="grey40", main="Length of North Sea horse mackerel caught in NS-IBTS (all years combined)", cex.main= 0.9, xlab="Length in cm", ylab="Relative occurrence",xlim=c(0,45))
dev.off()

# Length frequency in absolute numbers
Nhauls     <- length(unique(dataIBTS$HaulID))
lengthFreq <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClass>0], by=list(dataIBTS$LngtClass[dataIBTS$LngtClass>0]), FUN=sum)$x/Nhauls

names(lengthFreq) <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClass>0], by=list(dataIBTS$LngtClass[dataIBTS$LngtClass>0]), FUN=sum)$Group.1

png(filename=paste(figpath,"IBTS_length_abs_freq.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
barplot(lengthFreq/1000, las=1, col="red", border="grey40", main="length of North Sea horse mackerel in NS-IBTS (all years combined)", cex.main= 0.9, xlab="Length in cm", ylab="Absolute numbers x10^3",xlim=c(0,45), mgp=c(3,1,0))
dev.off()


## Explore length distribution of IBTS catches total by year
# Reduce the range of years to the last six years
dataIBTS$Year <- as.numeric(as.character(dataIBTS$Year))
dataIBTS      <- dataIBTS[dataIBTS$Year>(max(dataIBTS$Year)-6),]

# Relative numbers
lengthMatYrs              <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClass>0], by=list(dataIBTS$Year[dataIBTS$LngtClass>0],dataIBTS$LngtClass[dataIBTS$LngtClass>0]), FUN=sum)
colnames(lengthMatYrs)    <- c("Year", "Length", "Catch")
lengths                   <- data.frame(Length=seq(min(lengthMatYrs$Length),max(lengthMatYrs$Length),1))

png(filename=paste(figpath,"IBTS_length_relat_freq_by_year.png",sep=""), heigh=3000, width=2000, units="px", pointsize=9, res=450)
layout(matrix(1:6,3,2,byrow=T))
years=sort(unique(dataIBTS$Year))
cont=length(years)
maxy=0.4
for(i in 1:cont)
{
  
  lengthMatYr       <- lengthMatYrs[lengthMatYrs$Year==years[i],]
  plotValsYr        <- merge(lengths, lengthMatYr[,-1], by="Length", all=T)
  plotVals              <- plotValsYr$Catch/sum(plotValsYr$Catch,na.rm=T)
  names(plotVals)       <- plotValsYr$Length
  if(i==1 | i==3) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="", ylab="Relative occurrence", ylim=c(0,0.4), bty="l",xlim=c(0,45))} else {
    if(i==2 | i==4) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="", ylab="", ylim=c(0,0.4), bty="l",xlim=c(0,45))} else {
      if(i==5) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="Length in cm", ylab="Relative occurrence", ylim=c(0,0.4), bty="l",xlim=c(0,45))} else {
        if(i==6) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="Length in cm", ylab="", ylim=c(0,0.4), bty="l",xlim=c(0,45))}
      }
    }
  }
}

dev.off()


# Absolute numbers
lengthMatYrs              <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClass>0], by=list(dataIBTS$Year[dataIBTS$LngtClass>0],dataIBTS$LngtClass[dataIBTS$LngtClass>0]), FUN=sum)
colnames(lengthMatYrs)    <- c("Year", "Length", "Catch")
lengths                   <- data.frame(Length=seq(min(lengthMatYrs$Length),max(lengthMatYrs$Length),1))

png(filename=paste(figpath,"IBTS_length_abs_freq_by_year.png",sep=""), heigh=3000, width=2000, units="px", pointsize=9, res=450)
layout(matrix(1:6,3,2,byrow=T))
years=sort(unique(dataIBTS$Year))
cont=length(years)
maxy=1.5
for(i in 1:cont)
{
  subdataIBTS=subset(dataIBTS,Year==years[i])
  NhaulsYr=length(unique(subdataIBTS$HaulID))
  lengthMatYr       <- lengthMatYrs[lengthMatYrs$Year==years[i],]
  plotValsYr        <- merge(lengths, lengthMatYr[,-1], by="Length", all=T)
  plotVals              <- (plotValsYr$Catch/NhaulsYr)/1000
  names(plotVals)       <- plotValsYr$Length
  plotVals[is.na(plotVals)]=0
  if(i==1 | i==3) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="", ylab="Total number x10^3", ylim=c(0,maxy), bty="l",xlim=c(0,45))} else {
    if(i==2 | i==4) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="", ylab="", ylim=c(0,maxy), bty="l",xlim=c(0,45))} else {
      if(i==5) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="Length in cm", ylab="Total number x10^3", ylim=c(0,maxy), bty="l",xlim=c(0,45))} else {
        if(i==6) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="Length in cm", ylab="", ylim=c(0,maxy), bty="l",xlim=c(0,45))}
      }
    }
  }
}

dev.off()



# 2. Length frequency distriubtion FR-CGFS ----

# Select survey
dataCGFS <- survey_exp[survey_exp$Survey=="FR-CGFS",]

# Relative numbers
lengthFreq=aggregate(dataCGFS$CPUE[dataCGFS$LngtClass>0], by=list(dataCGFS$LngtClass[dataCGFS$LngtClass>0]), FUN=sum)$x/sum(dataCGFS$CPUE[dataCGFS$LngtClass>0],na.rm=T)

names(lengthFreq)   <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClass>0], by=list(dataCGFS$LngtClass[dataCGFS$LngtClass>0]), FUN=sum)$Group.1

png(filename=paste(figpath,"CGFS_length_relat_freq.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
barplot(lengthFreq, las=1, col="red", border="grey40", main="Lengths of North Sea horse mackerel caught in FR-CGFS (all years combined)", cex.main= 0.9, xlab="Length in cm", ylab="Relative occurrence",xlim=c(0,45))
dev.off()

# Absolute numbers
Nhauls     <- length(unique(survey_exp$HaulID))
lengthFreq <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClass>0], by=list(dataCGFS$LngtClass[dataCGFS$LngtClass>0]), FUN=sum)$x/Nhauls

names(lengthFreq) <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClass>0], by=list(dataCGFS$LngtClass[dataCGFS$LngtClass>0]), FUN=sum)$Group.1

png(filename=paste(figpath,"CGFS_length_abs_freq.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
barplot(lengthFreq/1000, las=1, col="red", border="grey40", main="Lengths of North Sea horse mackerel in FR-CGFS (all years combined)", cex.main= 0.9, xlab="Length in cm", ylab="Absolute numbers x10^3",xlim=c(0,45), mgp=c(3,1,0))
dev.off()


# Explore length distribution of CGFS catches total by year
# Reduce the range of years to the last six years
dataCGFS$Year <- as.numeric(as.character(dataCGFS$Year))
dataCGFS      <- dataCGFS[dataCGFS$Year>(max(dataCGFS$Year)-6),]

# Relative numbers
lengthMatYrs              <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClass>0], by=list(dataCGFS$Year[dataCGFS$LngtClass>0],dataCGFS$LngtClass[dataCGFS$LngtClass>0]), FUN=sum)
colnames(lengthMatYrs)    <- c("Year", "Length", "Catch")
lengths                   <- data.frame(Length=seq(min(lengthMatYrs$Length),max(lengthMatYrs$Length),1))

png(filename=paste(figpath,"CGFS_length_relat_freq_by_year.png",sep=""), heigh=3000, width=2000, units="px", pointsize=9, res=450)
layout(matrix(1:6,3,2,byrow=T))
years=sort(unique(dataCGFS$Year))
cont=length(years)
maxy=0.4
for(i in 1:cont)
{
  
  lengthMatYr       <- lengthMatYrs[lengthMatYrs$Year==years[i],]
  plotValsYr        <- merge(lengths, lengthMatYr[,-1], by="Length", all=T)
  plotVals              <- plotValsYr$Catch/sum(plotValsYr$Catch,na.rm=T)
  names(plotVals)       <- plotValsYr$Length
  if(i==1 | i==3) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="", ylab="Relative occurrence", ylim=c(0,0.4), bty="l",xlim=c(0,45))} else {
    if(i==2 | i==4) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="", ylab="", ylim=c(0,0.4), bty="l",xlim=c(0,45))} else {
      if(i==5) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="Length in cm", ylab="Relative occurrence", ylim=c(0,0.4), bty="l",xlim=c(0,45))} else {
        if(i==6) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="Length in cm", ylab="", ylim=c(0,0.4), bty="l",xlim=c(0,45))}
      }
    }
  }
}

dev.off()

# Absolute numbers
lengthMatYrs              <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClass>0], by=list(dataCGFS$Year[dataCGFS$LngtClass>0],dataCGFS$LngtClass[dataCGFS$LngtClass>0]), FUN=sum)
colnames(lengthMatYrs)    <- c("Year", "Length", "Catch")
lengths                   <- data.frame(Length=seq(min(lengthMatYrs$Length),max(lengthMatYrs$Length),1))

png(filename=paste(figpath,"CGFS_length_abs_freq_by_year.png",sep=""), heigh=3000, width=2000, units="px", pointsize=9, res=450)
layout(matrix(1:6,3,2,byrow=T))
years=sort(unique(dataCGFS$Year))
cont=length(years)
maxy=4.5
for(i in 1:cont)
{
  subdataCGFS=subset(dataCGFS,Year==years[i])
  NhaulsYr=length(unique(subdataCGFS$HaulID))
  lengthMatYr       <- lengthMatYrs[lengthMatYrs$Year==years[i],]
  plotValsYr        <- merge(lengths, lengthMatYr[,-1], by="Length", all=T)
  plotVals              <- (plotValsYr$Catch/NhaulsYr)/1000
  names(plotVals)       <- plotValsYr$Length
  plotVals[is.na(plotVals)]=0
  if(i==1 | i==3) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="", ylab="Total number x10^3", ylim=c(0,maxy), bty="l",xlim=c(0,45))} else {
    if(i==2 | i==4) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="", ylab="", ylim=c(0,maxy), bty="l",xlim=c(0,45))} else {
      if(i==5) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="Length in cm", ylab="Total number x10^3", ylim=c(0,maxy), bty="l",xlim=c(0,45))} else {
        if(i==6) {barplot(plotVals, beside=T, las=1, col="red", border="grey40", main=paste("Year ", years[i], sep=""), xlab="Length in cm", ylab="", ylim=c(0,maxy), bty="l",xlim=c(0,45))}
      }
    }
  }
}

dev.off()



############################################################################################################-
##### EXPLOITABLE STOCK -----

# Select only exploitable stock
datos <- survey_exp[survey_exp$groupsize=="exploitable",]                 
datos <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos)


##### 1. Time series -----

# Estimate mean and standard deviation of CPUE per Survey and Year. 
#The mean value is expected to be very similar to the value produced by the hurdle model.

# Mean
meanCPUE          <- data.frame(tapply(datos$CPUE,list(datos$Year,datos$Survey),mean))
meanCPUE$Year     <- as.numeric(rownames(meanCPUE))
meanCPUE$pond_ave <- (meanCPUE$FR.CGFS*0.24+meanCPUE$NS.IBTS*0.76) #multiply CPUE of each survey with weight determined in 2017 benchmark

png(filename=paste(figpath,"Adult_Average_CPUE_by_trawl.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
plot(meanCPUE$Year,meanCPUE$FR.CGFS, type="b", col="grey40", ylim=c(0,max(meanCPUE)),pch=19,ylab="Mean CPUE",xlab="Year", main="Exploitable stock")
lines(meanCPUE$Year,meanCPUE$NS.IBTS, type="b", col="red",pch=19)
legend("topright",legend=c("FR-CGFS", "NS-IBTS"),lty=1,pch=19,col=c("grey40","red"),bty="n")
dev.off()

write.csv(meanCPUE,paste(outpath,"meanCPUE_surveys.csv",sep=""),row.names=F)

# Standard deviation
sdCPUE      <- data.frame(tapply(datos$CPUE,list(datos$Year,datos$Survey),sd))
sdCPUE$Year <- as.numeric(rownames(meanCPUE))

png(filename=paste(figpath,"Adult_Standard_Deviation_CPUE_by_trawl.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
plot(sdCPUE$Year,sdCPUE$FR.CGFS, type="b", col="grey40", ylim=c(0,max(sdCPUE)),pch=19,ylab="Standard deviation CPUE",xlab="Year", main="Exploitable stock")
lines(sdCPUE$Year,sdCPUE$NS.IBTS, type="b", col="red",pch=19)
legend("topright",legend=c("FR-CGFS", "NS-IBTS"),lty=1,pch=19,col=c("grey40","red"),bty="n")
dev.off()


# Now histogram with the number of cases per CPUE in the late period
years <- sort(unique(dataIBTS$Year[dataIBTS$Year>(max(dataIBTS$Year)-5)]))

# First the IBTS
datosIBTS <- datos[datos$Survey=="NS-IBTS",]
selection <- subset(datosIBTS, Year==years[1] | Year==years[2] | Year==years[3] | Year==years[4] | Year==years[5])

table(selection$CPUE)
table(sqrt(selection$CPUE))

png(filename=paste(figpath,"Adult_Histogram_IBTS_sqroot_CPUE.png",sep=""), height=2000, width=2000, units="px", pointsize=9, res=450)
ggplot(selection, aes(sqrt(CPUE), fill=Year)) + 
  geom_histogram(binwidth=5) + 
  scale_x_continuous(limits=c(0,125)) + 
  scale_y_continuous(limits=c(0,20)) + 
  theme_bw() + ggtitle("Exploitable stock NS-IBTS") + 
  theme(plot.title = element_text(hjust = 0.5, size=18),
        legend.position = "none") + 
  facet_grid(Year ~ .)
dev.off()


# Second the FR-CGFS
datosCGFS <- datos[datos$Survey=="FR-CGFS",]
selection <- subset(datosCGFS, Year==years[1] | Year==years[2] | Year==years[3] | Year==years[4] | Year==years[5])

table(selection$CPUE)
table(sqrt(selection$CPUE))

png(filename=paste(figpath,"Adult_Histogram_CGFS_sqroot_CPUE.png",sep=""), height=2000, width=2000, units="px", pointsize=9, res=450)
ggplot(selection, aes(sqrt(CPUE), fill=Year)) + 
  geom_histogram(binwidth=5) + 
  scale_x_continuous(limits=c(0,125)) + 
  scale_y_continuous(limits=c(0,20)) + 
  theme_bw() + ggtitle("Exploitable stock FR-CGFS") + 
  theme(plot.title = element_text(hjust = 0.5, size=18),
        legend.position = "none") + 
  facet_grid(Year ~ .)
dev.off()


##### 2. Maps -----

# Bubble plots representing spatial patterns of CPUE in different years
#  Map now several years together 2014-2018.
selecdata <- subset(datos, Year %in% Years)

png(filename=paste(figpath,"Adult_bubble_plot_CPUE_",years[1],"-",years[6],".png",sep=""), heigh=1600, width=2500, units="px", pointsize=5, res=300)

par(oma=c(0,0,0,0))
mapPoints <- ggplot() +  
  borders(fill="azure3",colour = "azure3") +
  coord_quickmap(xlim=limx,ylim=limy) +
  geom_point(aes(x = ShootLong, y = ShootLat, size = sqrt(CPUE)), data = selecdata, alpha = .5, colour="red") +  
  facet_wrap(~Year, labeller = label_both, nrow=2, ncol=3) + 
  theme(text = element_text(size=14)) + 
  labs(x="Longitude",y="Latitude") + 
  ggtitle("Exploitable substock") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18))
print(mapPoints)

dev.off()


# Now map only the CGFS for the last six years
selecdata <- subset(datos, Year %in% years & Survey %in% "FR-CGFS")

png(filename=paste(figpath,"Adult_bubble_plot_CPUE_",years[1],"-",years[6],"_CGFS.png",sep=""), heigh=1600, width=2500, units="px", pointsize=5, res=300)

par(oma=c(0,0,0,0))
mapPoints <- ggplot() +  
  borders(fill="azure3",colour = "azure3") +
  coord_quickmap(xlim=c(-2,2),ylim=c(49,51.5)) +  
  geom_point(aes(x = ShootLong, y = ShootLat, size = sqrt(CPUE)), data = selecdata, alpha = .5,colour="red") +  
  facet_wrap(~Year, labeller = label_both, nrow=2, ncol=3) + 
  theme(text = element_text(size=14)) + 
  labs(x="Longitude",y="Latitude") + ggtitle("Exploitable substock")+  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18))
print(mapPoints)

dev.off()



############################################################################################################-
##### JUVENILE STOCK -----

datos <- survey_exp[survey_exp$groupsize=="juveniles",]
datos <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulID + ShootLong + ShootLat + Depth, FUN=sum, data=datos)


##### 1. Time series -----

# Estimate mean and standard deviation of CPUE per Survey and Year. 
#The mean value is expected to be very similar to the value produced by the hurdle model.

# Mean
meanCPUE          <- data.frame(tapply(datos$CPUE,list(datos$Year,datos$Survey),mean))
meanCPUE$Year     <- as.numeric(rownames(meanCPUE))
meanCPUE$pond_ave <- (meanCPUE$FR.CGFS*0.24+meanCPUE$NS.IBTS*0.76) #multiply CPUE of each survey with weight determined in 2017 benchmark

png(filename=paste(figpath,"Juvenile_Average_CPUE_by_trawl.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
plot(meanCPUE$Year,meanCPUE$FR.CGFS, type="b", col="grey40", ylim=c(0,max(meanCPUE)),pch=19,ylab="Mean CPUE",xlab="Year", main="Juvenile stock")
lines(meanCPUE$Year,meanCPUE$NS.IBTS, type="b", col="red",pch=19)
legend("topright",legend=c("FR-CGFS", "NS-IBTS"),lty=1,pch=19,col=c("grey40","red"),bty="n")
dev.off()

# Standard deviation
sdCPUE      <- data.frame(tapply(datos$CPUE,list(datos$Year,datos$Survey),sd))
sdCPUE$Year <- as.numeric(rownames(meanCPUE))

png(filename=paste(figpath,"Juvenile_Standard_Deviation_CPUE_by_trawl.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
plot(sdCPUE$Year,sdCPUE$FR.CGFS, type="b", col="grey40", ylim=c(0,max(sdCPUE)),pch=19,ylab="Standard deviation CPUE",xlab="Year", main="Juvenile stock")
lines(sdCPUE$Year,sdCPUE$NS.IBTS, type="b", col="red",pch=19)
legend("topright",legend=c("FR-CGFS", "NS-IBTS"),lty=1,pch=19,col=c("grey40","red"),bty="n")
dev.off()


# Now histogram with the number of cases per CPUE in the late period 2013-2017
# Now histogram with the number of cases per CPUE in the late period
years <- sort(unique(dataIBTS$Year[dataIBTS$Year>(max(dataIBTS$Year)-5)]))

# First the IBTS
datosIBTS <- datos[datos$Survey=="NS-IBTS",]
selection <- subset(datosIBTS, Year==years[1] | Year==years[2] | Year==years[3] | Year==years[4] | Year==years[5])

table(selection$CPUE)
table(sqrt(selection$CPUE))
range(sqrt(selection$CPUE))

png(filename=paste(figpath,"Juvenile_Histogram_IBTS_sqroot_CPUE.png",sep=""), heigh=2000, width=2000, units="px", pointsize=9, res=450)
ggplot(selection, aes(sqrt(CPUE), fill=Year)) + 
  geom_histogram(binwidth=5) + 
  scale_x_continuous(limits=c(0,840)) + 
  scale_y_continuous(limits=c(0,15)) + 
  ggtitle("Juvenile stock IBTS") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18),
        legend.position = "none") + 
  facet_grid(Year ~ .)
dev.off()


# Second the CGFS
datosCGFS <- datos[datos$Survey=="FR-CGFS",]
selection <- subset(datosCGFS, Year==years[1] | Year==years[2] | Year==years[3] | Year==years[4] | Year==years[5])

table(selection$CPUE)
table(sqrt(selection$CPUE))
range(sqrt(selection$CPUE))

png(filename=paste(figpath,"Juvenile_Histogram_CGFS_sqroot_CPUE.png",sep=""), heigh=2000, width=2000, units="px", pointsize=9, res=450)
ggplot(selection, aes(sqrt(CPUE), fill=Year)) + 
  geom_histogram(binwidth=5) + 
  scale_x_continuous(limits=c(0,350)) + 
  scale_y_continuous(limits=c(0,12)) + 
  ggtitle("Juvenile stock CGFS") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18),
        legend.position = "none") + 
  facet_grid(Year ~ .)
dev.off()



##### 2. Maps -----

# Bubble plots representing spatial patterns of CPUE in different years

# Plot maps of all years together
selecdata <- subset(datos, Year %in% Years)

png(filename=paste(figpath,"Juvenile_bubble_plot_CPUE_",years[1],"-",years[6],".png",sep=""), height=1600, width=2500, units="px", pointsize=5, res=300)

par(oma=c(0,0,0,0))
mapPoints <- ggplot() +  
  borders(fill="azure3",colour = "azure3") +
  coord_quickmap(xlim=limx,ylim=limy) +
  geom_point(aes(x = ShootLong, y = ShootLat, size = sqrt(CPUE)), data = selecdata, alpha = .5, colour="red") +  
  facet_wrap(~Year, labeller = label_both, nrow=2, ncol=3) + 
  theme(text = element_text(size=14)) + 
  labs(x="Longitude",y="Latitude") + 
  ggtitle("Juvenile substock") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18))
print(mapPoints)

dev.off()


# Plot maps of all years focussing on CGFS
selecdata=subset(datos, Year %in% Years & Survey %in% "FR-CGFS")

png(filename=paste(figpath,"Juvenile_bubble_plot_CPUE_",Years[1],"-",Years[6],"_CGFS.png",sep=""), height=1600, width=2500, units="px", pointsize=5, res=300)

par(oma=c(0,0,0,0))
mapPoints <- ggplot() +  
  borders(fill="azure3",colour = "azure3") +
  coord_quickmap(xlim=c(-2,2),ylim=c(49,51.5)) +  
  geom_point(aes(x = ShootLong, y = ShootLat, size = sqrt(CPUE)), data = selecdata, alpha = .5, colour="red") +  
  facet_wrap(~Year, labeller = label_both, nrow=2, ncol=3) + 
  theme(text = element_text(size=14)) + 
  labs(x="Longitude",y="Latitude") + 
  ggtitle("Juvenile substock") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18))
print(mapPoints)

dev.off()

