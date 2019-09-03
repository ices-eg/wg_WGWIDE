# Created by Alfonso Perez, adapted by Esther Beukhod

rm(list=ls())

options(stringsAsFactors=FALSE)

library(plyr)

# datpath="C:/IMARES/FAMA/ICES/WGWIDE/August_2018/NSHM/data/survey_data/"
# resultpath="C:/IMARES/FAMA/ICES/WGWIDE/August_2018/NSHM/data_exploration/survey_data/"
#datpath="M:/My Documents/WGWIDE/Assessment 2019/NSHM/data/survey_data/"
#resultpath="M:/My Documents/WGWIDE/Assessment 2019/NSHM/data_exploration/survey_data/"

datpath="C:/git/wg_WGWIDE/NSHM/data/"
resultpath="C:/git/wg_WGWIDE/NSHM/results/"
figPath <- "C:/git/wg_WGWIDE/NSHM/figures/survey/"

# I open and prepare the database with the fishing sets.
sets_cgfs=read.csv(paste(datpath,"Exchange_CGFS_HH_1988-2018.csv",sep=""),head=T,stringsAsFactors = FALSE)
sets_cgfs$Survey=rep("CGFS", length(sets_cgfs$Survey))
#Change the Gwen Dwez vessel by XX because in the length frequency database I don´t have the GWD.
# sets_cgfs$Ship=revalue(sets_cgfs$Ship, c("GWD"="XX", "THA2"="THA2"))
sets_ibts=read.csv(paste(datpath,"Exchange_NS-IBTS_HH_1991-2018.csv",sep=""),head=T,stringsAsFactors = FALSE)

# Merge  CGFS and NS-IBTS fishing set databases
sets=rbind(sets_cgfs, sets_ibts)
sets=sets[sets$HaulVal=="V",]
sets$HaulNo=paste(sets$Survey, "-", sets$Year, "-", sets$Ship, "-", sets$HaulNo, sep="")

selec=c("HaulNo", "Survey", "Year", "StatRec", "HaulDur", "ShootLat", "ShootLong", "Depth","DataType")
sets=sets[,names(sets) %in% selec]

# I create a database in which all hauls with all possible sizes for the NSHM will be
temp=expand.grid(HaulNo=sort(unique(sets$HaulNo)), LngtClas=c(0:60))
temp=temp[with(temp, order(HaulNo,LngtClas)),]
dim(temp)

# I merged the database of fishing sets with this database with sizes
sets=merge(sets,temp,by.x="HaulNo", by.y="HaulNo")
dim(sets)

sets$code=paste(sets$HaulNo,"-",sets$LngtClas,sep="")
dim(sets)

# I open and prepare the database with the number of individuals captured by size.
data=read.table(paste(resultpath,"survey_data_1991-2018.txt",sep=""),head=T)

data$code=paste(data$HaulNo,"-",data$LngtClas,sep="")

selec=c("code", "Number","SubFactor")
data=data[,names(data) %in% selec]

dim(data)

# I merge the size frequency databases with that of the fishing sets with all possible sizes. In this way I have a database in which all the fishing sets will be, with all the sizes and all the abundances, including all the zeros for those sizes and fishing sets for which no individual was fished.
data=merge(sets,data,by.x="code",by.y="code",all.x=TRUE)
dim(data)

summary(data)
data$Number[is.na(data$Number)]=0
summary(data)

# Next, prepare the data base for the analysisdim(data)
data=data[data$Depth>0,]
dim(data)
# data=na.omit(data)
dim(data)
data=data[data$LngtClas>=0,]
dim(data)

##---define rectangles----The Statistical rectangles from areas 3a and 4a are excluded since the surveys are conducted on Q3 and Q4, when the Western stock is assumed to be the one inhabiting this areas. The statistical rectangles from the CGFS survey area are all included in this list.
rectangles=c("27E8", "27E9", "27F0", "28E8", "28E9", "28F0", "28F1", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0", "30F1", "31F1", "31F2", "39F1", "39F2", "40F1", "40F2", "40F3", "40F4", "41F2", "41F3", "41F4", "42F3", "42F4", "43F4", "36F0", "37F0", "37F1", "38E9", "38F0", "38F1", "39F0", "32F1", "33F2", "34F2", "35F1", "35F2", "36F1", "36F2", "32F2", "32F3", "33F3", "33F4", "34F3", "34F4", "35F3", "35F4", "36F3", "36F4", "36F5", "36F6", "37F2", "37F3", "37F4", "37F5", "37F6", "37F7", "38F2", "38F3", "38F4", "38F5", "38F6", "38F7", "39F3", "39F4", "39F5", "39F6", "39F7", "40F5", "40F6", "40F7", "41F7", "42F5", "42F6")
table(data$Survey)
data=data[data$StatRec %in% rectangles,]
table(data$Survey)

# Remove those hauls conducted in statistical rectangles 33F1 and 33F2
dim(data)
data=data[data$StatRec!="31F1", ]
data=data[data$StatRec!="31F2", ]
dim(data)

# Remove those fishing sets that have been conducted in the mouth of the Seine river.
dim(data)
data=data[data$ShootLat>49.4735 | data$ShootLong<(-0.09442),]
dim(data)

## Define the years that are going to be used in the analysis
intYears      <- c(1992:2018)
table(data$Survey)
data <- data[data$Year %in% intYears,]
table(data$Survey)

## Calculate the CPUE per hour
table(data$HaulDur)
data <- data[!(data$HaulDur<15 | data$HaulDur>45),] 
# data$CPUE <- round(data$Number/(data$HaulDur/60),digits=0)  
data$CPUE <- NA
# If datatype=C (catch per unit of effort (1 h trawling)), then number at length per haul = number at length * subfactor
data[data$DataType=="C",]$CPUE <- round((data[data$DataType=="C",]$Number)*(data[data$DataType=="C",]$SubFactor),0)
#If dataatype=R (raw dataa), then number at length per haul = number at length * subfactor * haul duration/60
data[data$DataType=="R",]$CPUE <- round((data[data$DataType=="R",]$Number)*(data[data$DataType=="R",]$SubFactor)*60/(data[data$DataType=="R",]$HaulDur),0)
summary(data)
data$CPUE[is.na(data$CPUE)]=0
summary(data)

# Convert abundance of CGFS by GWD to match with new vessel THA
data$CPUE <- with(data,ifelse(Survey %in% "CGFS" & Year<2015, CPUE*10.363,CPUE))

### Separate the data in juveniles and exploitable stock based in the 20cm size limit decided during the benchmark. 
data$groupsize=ifelse(data$LngtClas<20,"juveniles","exploitable")

# Next, those fishing sets for which there is no catch for any of the two groupsizes and for which the LngtClas is zero, I am going to assign this fishing sets to both juvenile and exploitable stocks.
sel=data[data$LngtClas==0,]
sel$groupsize=rep("exploitable",length(sel$Year))
data=rbind(data,sel)

# Next, reset the levels of factors for year and HaulNo
data$Year <- factor(data$Year)
data$HaulNo <- factor(data$HaulNo)

#Save
head(data)
summary(data)
write.csv(data, paste(resultpath,"survey_data_1992-2018_clean.csv",sep = ""),row.names = FALSE)

#Check CPUE data
library(ggplot2)
datplot <- data
str(datplot)
datplot$Year <- as.numeric(as.character(datplot$Year))
str(datplot)
p <- ggplot(datplot,aes(x=Year,y=sqrt(CPUE),group=Year)) +
  geom_boxplot() +
  facet_wrap(~Survey) + ggtitle("")
print(p)


################################################################################################
################################ Length distributions ##########################################
################################################################################################

##################################################
#- Explore length distribution of IBTS catches total

dataIBTS=data[data$Survey=="NS-IBTS",]

######### En n?meros relativos
lengthFreq=aggregate(dataIBTS$CPUE[dataIBTS$LngtClas>0], by=list(dataIBTS$LngtClas[dataIBTS$LngtClas>0]), FUN=sum)$x/sum(dataIBTS$CPUE[dataIBTS$LngtClas>0],na.rm=T)

names(lengthFreq)   <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClas>0], by=list(dataIBTS$LngtClas[dataIBTS$LngtClas>0]), FUN=sum)$Group.1

png(filename=paste(figPath,"IBTS_length_relat_freq.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
barplot(lengthFreq, las=1, col="red", border="grey40", main="Histogram of lengths of horse mackerel caught in the IBTS (all years combined)", cex.main= 0.9, xlab="Length in cm", ylab="Relative occurrence",xlim=c(0,45))
dev.off()


########## En n?meros absolutos
Nhauls=length(unique(dataIBTS$HaulNo))
lengthFreq=aggregate(dataIBTS$CPUE[dataIBTS$LngtClas>0], by=list(dataIBTS$LngtClas[dataIBTS$LngtClas>0]), FUN=sum)$x/Nhauls

names(lengthFreq)   <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClas>0], by=list(dataIBTS$LngtClas[dataIBTS$LngtClas>0]), FUN=sum)$Group.1

png(filename=paste(figPath,"IBTS_length_abs_freq.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
barplot(lengthFreq/1000, las=1, col="red", border="grey40", main="Histogram of lengths of horse mackerel caught in the IBTS (all years combined)", cex.main= 0.9, xlab="Length in cm", ylab="Absolute numbers x10^3",xlim=c(0,45), mgp=c(3,1,0))
dev.off()


#- Explore length distribution of IBTS catches total by year
# Reduce the range of years to the last six years
dataIBTS$Year=as.numeric(as.character(dataIBTS$Year))
dataIBTS=dataIBTS[dataIBTS$Year>(max(dataIBTS$Year)-6),]

######### En n?meros relativos
lengthMatYrs              <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClas>0], by=list(dataIBTS$Year[dataIBTS$LngtClas>0],dataIBTS$LngtClas[dataIBTS$LngtClas>0]), FUN=sum)
colnames(lengthMatYrs)    <- c("Year", "Length", "Catch")
lengths                   <- data.frame(Length=seq(min(lengthMatYrs$Length),max(lengthMatYrs$Length),1))


png(filename=paste(figPath,"IBTS_length_relat_freq_by_year.png",sep=""), heigh=3000, width=2000, units="px", pointsize=9, res=450)
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


###### En n?meros absolutos
lengthMatYrs              <- aggregate(dataIBTS$CPUE[dataIBTS$LngtClas>0], by=list(dataIBTS$Year[dataIBTS$LngtClas>0],dataIBTS$LngtClas[dataIBTS$LngtClas>0]), FUN=sum)
colnames(lengthMatYrs)    <- c("Year", "Length", "Catch")
lengths                   <- data.frame(Length=seq(min(lengthMatYrs$Length),max(lengthMatYrs$Length),1))


png(filename=paste(figPath,"IBTS_length_abs_freq_by_year.png",sep=""), heigh=3000, width=2000, units="px", pointsize=9, res=450)
layout(matrix(1:6,3,2,byrow=T))
years=sort(unique(dataIBTS$Year))
cont=length(years)
maxy=1.5
for(i in 1:cont)
{
  subdataIBTS=subset(dataIBTS,Year==years[i])
  NhaulsYr=length(unique(subdataIBTS$HaulNo))
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



##################################################
#- Explore length distribution of CGFS catches total

dataCGFS=data[data$Survey=="CGFS",]

######### En n?meros relativos
lengthFreq=aggregate(dataCGFS$CPUE[dataCGFS$LngtClas>0], by=list(dataCGFS$LngtClas[dataCGFS$LngtClas>0]), FUN=sum)$x/sum(dataCGFS$CPUE[dataCGFS$LngtClas>0],na.rm=T)

names(lengthFreq)   <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClas>0], by=list(dataCGFS$LngtClas[dataCGFS$LngtClas>0]), FUN=sum)$Group.1

png(filename=paste(figPath,"CGFS_length_relat_freq.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
barplot(lengthFreq, las=1, col="red", border="grey40", main="Histogram of lengths of horse mackerel caught in the CGFS (all years combined)", cex.main= 0.9, xlab="Length in cm", ylab="Relative occurrence",xlim=c(0,45))
dev.off()


########## En n?meros absolutos
Nhauls=length(unique(dataCGFS$HaulNo))
lengthFreq=aggregate(dataCGFS$CPUE[dataCGFS$LngtClas>0], by=list(dataCGFS$LngtClas[dataCGFS$LngtClas>0]), FUN=sum)$x/Nhauls

names(lengthFreq)   <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClas>0], by=list(dataCGFS$LngtClas[dataCGFS$LngtClas>0]), FUN=sum)$Group.1

png(filename=paste(figPath,"CGFS_length_abs_freq.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
barplot(lengthFreq/1000, las=1, col="red", border="grey40", main="Histogram of lengths of horse mackerel caught in the CGFS (all years combined)", cex.main= 0.9, xlab="Length in cm", ylab="Absolute numbers x10^3",xlim=c(0,45), mgp=c(3,1,0))
dev.off()


#- Explore length distribution of CGFS catches total by year
# Reduce the range of years to the last six years
dataCGFS$Year=as.numeric(as.character(dataCGFS$Year))
dataCGFS=dataCGFS[dataCGFS$Year>(max(dataCGFS$Year)-6),]

######### En n?meros relativos
lengthMatYrs              <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClas>0], by=list(dataCGFS$Year[dataCGFS$LngtClas>0],dataCGFS$LngtClas[dataCGFS$LngtClas>0]), FUN=sum)
colnames(lengthMatYrs)    <- c("Year", "Length", "Catch")
lengths                   <- data.frame(Length=seq(min(lengthMatYrs$Length),max(lengthMatYrs$Length),1))


png(filename=paste(figPath,"CGFS_length_relat_freq_by_year.png",sep=""), heigh=3000, width=2000, units="px", pointsize=9, res=450)
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


###### En n?meros absolutos
lengthMatYrs              <- aggregate(dataCGFS$CPUE[dataCGFS$LngtClas>0], by=list(dataCGFS$Year[dataCGFS$LngtClas>0],dataCGFS$LngtClas[dataCGFS$LngtClas>0]), FUN=sum)
colnames(lengthMatYrs)    <- c("Year", "Length", "Catch")
lengths                   <- data.frame(Length=seq(min(lengthMatYrs$Length),max(lengthMatYrs$Length),1))


png(filename=paste(figPath,"CGFS_length_abs_freq_by_year.png",sep=""), heigh=3000, width=2000, units="px", pointsize=9, res=450)
layout(matrix(1:6,3,2,byrow=T))
years=sort(unique(dataCGFS$Year))
cont=length(years)
maxy=4.5
for(i in 1:cont)
{
  subdataCGFS=subset(dataCGFS,Year==years[i])
  NhaulsYr=length(unique(subdataCGFS$HaulNo))
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




################################################################################################
################################  Análisis of time and spacial variation in CPUE  ##############
################################################################################################


#######################################################
####  Larger than the defined cutoff length  #########

datos  <- data[data$groupsize=="exploitable",]                 ##---when Number=0, LngtClas is set as 0
datos <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)


# Estimate mean and standard deviation of CPUE per Survey and Year. The mean value is expected to be very similar to the value produced by the hurdle model.

# Mean
meanCPUE=data.frame(tapply(datos$CPUE,list(datos$Year,datos$Survey),mean))
meanCPUE$Year=as.numeric(rownames(meanCPUE))
meanCPUE$pond_ave=(meanCPUE$CGFS*0.24+meanCPUE$NS.IBTS*0.76)

png(filename=paste(figPath,"Adult_Average_CPUE_by_trawl.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
plot(meanCPUE$Year,meanCPUE$CGFS, type="b", col="grey40", ylim=c(0,max(meanCPUE)),pch=19,ylab="Mean CPUE",xlab="Year", main="Exploitable stock")
lines(meanCPUE$Year,meanCPUE$NS.IBTS, type="b", col="red",pch=19)
legend("topright",legend=c("CGFS", "NS-IBTS"),lty=1,pch=19,col=c("grey40","red"),bty="n")
dev.off()

write.csv(meanCPUE,paste(resultpath,"meanCPUE_surveys.csv",sep=""),row.names=F)

# Standard deviation
sdCPUE=data.frame(tapply(datos$CPUE,list(datos$Year,datos$Survey),sd))
sdCPUE$Year=as.numeric(rownames(meanCPUE))

png(filename=paste(figPath,"Adult_Standard_Deviation_CPUE_by_trawl.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
plot(sdCPUE$Year,sdCPUE$CGFS, type="b", col="grey40", ylim=c(0,max(sdCPUE)),pch=19,ylab="Standard deviation CPUE",xlab="Year", main="Exploitable stock")
lines(sdCPUE$Year,sdCPUE$NS.IBTS, type="b", col="red",pch=19)
legend("topright",legend=c("CGFS", "NS-IBTS"),lty=1,pch=19,col=c("grey40","red"),bty="n")
dev.off()


# Now histogram with the number of cases per CPUE in the late period
library(ggplot2)

# First the IBTS
datosIBTS=datos[datos$Survey=="NS-IBTS",]
selection=subset(datosIBTS, Year==2014 | Year==2015 | Year==2016 | Year==2017 | Year==2018)

table(selection$CPUE)
table(sqrt(selection$CPUE))

png(filename=paste(figPath,"Adult_Histogram_IBTS_sqroot_CPUE.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
ggplot(selection, aes(sqrt(CPUE), fill=Year)) + geom_histogram(binwidth=5) + scale_x_continuous(limits=c(0,200)) + scale_y_continuous(limits=c(0,30)) + ggtitle("Exploitable stock IBTS") + theme(plot.title = element_text(hjust = 0.5, size=18)) + facet_grid(Year ~ .)
dev.off()


# Second the CGFS
datosCGFS=datos[datos$Survey=="CGFS",]
selection=subset(datosIBTS, Year==2014 | Year==2015 | Year==2016 | Year==2017 | Year==2018)

table(selection$CPUE)
table(sqrt(selection$CPUE))

png(filename=paste(figPath,"Adult_Histogram_CGFS_sqroot_CPUE.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
ggplot(selection, aes(sqrt(CPUE), fill=Year)) + geom_histogram(binwidth=5) + scale_x_continuous(limits=c(0,360)) + scale_y_continuous(limits=c(0,40)) + ggtitle("Exploitable stock CGFS") + theme(plot.title = element_text(hjust = 0.5, size=18)) + facet_grid(Year ~ .)
dev.off()


# Bubble plots representing spatial patterns of CPUE in different years
library(grid)
library(gridExtra)
library(maps)

#Set latitude and longitude for map
range(sets$ShootLat)
range(sets$ShootLong)
limx <- c(-5,10)
limy <- c(49,58)

#  Map now several years together 2014-2018.
selecdata=subset(datos, Year %in% c(2013:2018))

png(filename=paste(figPath,"Adult_bubble_plot_CPUE_2013-2018.png",sep=""), heigh=1600, width=2500, units="px", pointsize=5, res=300)

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
selecdata=subset(datos, Year %in% c(2013:2018) & Survey %in% "CGFS")

png(filename=paste(figPath,"Adult_bubble_plot_CPUE_2013-2018_CGFS.png",sep=""), heigh=1600, width=2500, units="px", pointsize=5, res=300)

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




#######################################################
####  Smaller than the defined cutoff length  #########
#######################################################

datos  <- data[data$groupsize=="juveniles",]
datos <- aggregate(CPUE ~ Survey + Year + StatRec + HaulDur + HaulNo + ShootLong + ShootLat + Depth, FUN=sum, data=datos)

# Estimate mean and standard deviation of CPUE per Survey and Year. The mean value is expected to be very similar to the value produced by the hurdle model.

# Mean
meanCPUE=data.frame(tapply(datos$CPUE,list(datos$Year,datos$Survey),mean))
meanCPUE$Year=as.numeric(rownames(meanCPUE))
meanCPUE$pond_ave=(meanCPUE$CGFS*0.24+meanCPUE$NS.IBTS*0.76)

png(filename=paste(figPath,"Juvenile_Average_CPUE_by_trawl.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
plot(meanCPUE$Year,meanCPUE$CGFS, type="b", col="grey40", ylim=c(0,max(meanCPUE)),pch=19,ylab="Mean CPUE",xlab="Year", main="Juvenile stock")
lines(meanCPUE$Year,meanCPUE$NS.IBTS, type="b", col="red",pch=19)
legend("topright",legend=c("CGFS", "NS-IBTS"),lty=1,pch=19,col=c("grey40","red"),bty="n")
dev.off()

# Standard deviation
sdCPUE=data.frame(tapply(datos$CPUE,list(datos$Year,datos$Survey),sd))
sdCPUE$Year=as.numeric(rownames(meanCPUE))

png(filename=paste(figPath,"Juvenile_Standard_Deviation_CPUE_by_trawl.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
plot(sdCPUE$Year,sdCPUE$CGFS, type="b", col="grey40", ylim=c(0,max(sdCPUE)),pch=19,ylab="Standard deviation CPUE",xlab="Year", main="Juvenile stock")
lines(sdCPUE$Year,sdCPUE$NS.IBTS, type="b", col="red",pch=19)
legend("topright",legend=c("CGFS", "NS-IBTS"),lty=1,pch=19,col=c("grey40","red"),bty="n")
dev.off()


# Now histogram with the number of cases per CPUE in the late period 2013-2017
library(ggplot2)

# First the IBTS
datosIBTS=datos[datos$Survey=="NS-IBTS",]
selection=subset(datosIBTS, Year==2014 | Year==2015 | Year==2016 | Year==2017 | Year==2018)

table(selection$CPUE)
table(sqrt(selection$CPUE))

png(filename=paste(figPath,"Juvenile_Histogram_IBTS_sqroot_CPUE.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
ggplot(selection, aes(sqrt(CPUE), fill=Year)) + geom_histogram(binwidth=5) + scale_x_continuous(limits=c(0,1050)) + scale_y_continuous(limits=c(0,15)) + ggtitle("Juvenile stock IBTS") + theme(plot.title = element_text(hjust = 0.5, size=18)) + facet_grid(Year ~ .)
dev.off()


# Second the CGFS
datosCGFS=datos[datos$Survey=="CGFS",]
selection=subset(datosCGFS, Year==2014 | Year==2015 | Year==2016 | Year==2017 | Year==2018)

table(selection$CPUE)
table(sqrt(selection$CPUE))

png(filename=paste(figPath,"Juvenile_Histogram_CGFS_sqroot_CPUE.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
ggplot(selection, aes(sqrt(CPUE), fill=Year)) + geom_histogram(binwidth=5) + scale_x_continuous(limits=c(0,400)) + scale_y_continuous(limits=c(0,12)) + ggtitle("Juvenile stock CGFS") + theme(plot.title = element_text(hjust = 0.5, size=18)) + facet_grid(Year ~ .)
dev.off()




# Bubble plots representing spatial patterns of CPUE in different years

#Set latitude and longitude for map
range(sets$ShootLat)
range(sets$ShootLong)
limx <- c(-5,10)
limy <- c(49,58)
map <- get_map(location = c(lon = 3.2, lat = 53.5), zoom = 6, maptype = c("satellite"), color = c("color"), crop=T)



# Plot maps of all years together
selecdata=subset(datos, Year %in% c(2013:2018))

png(filename=paste(figPath,"Juvenile_bubble_plot_CPUE_2013-2018.png",sep=""), height=1600, width=2500, units="px", pointsize=5, res=300)

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
selecdata=subset(datos, Year %in% c(2013:2018) & Survey %in% "CGFS")

png(filename=paste(figPath,"Juvenile_bubble_plot_CPUE_2013-2018_CGFS.png",sep=""), height=1600, width=2500, units="px", pointsize=5, res=300)

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




