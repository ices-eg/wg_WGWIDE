# Created by Alfonso Perez, adapted by Esther Beukhof

rm(list=ls())

# Increase the number of lines shown in R console
options(max.print=100000)
options(stringsAsFactors=FALSE)


library(lattice)
library(plyr)
library(scales)
library(pscl)
library(MASS)
library(lmtest)
library(reshape)

datpath="C:/git/wg_WGWIDE/NSHM/data/"
resultpath="C:/git/wg_WGWIDE/NSHM/results/"

#hhdata <- getDATRAS(record = "HH", survey = "NS-IBTS", years = 1991:2018, quarters = 3)
#hldata_IBTS <- getDATRAS(record = "HL", survey = "NS-IBTS", years = 1991:2018, quarters = 3)
#hhdata <- getDATRAS(record = "HH", survey = "FR-CGFS", years = 1991:2018, quarters = 4)
#hldata <- getDATRAS(record = "HL", survey = "FR-CGFS", years = 1991:2018, quarters = 4)

# olddatpath="C:/IMARES/FAMA/ICES/WGWIDE/August_2017/NSHM/data/survey_data/"
# newdatpath="C:/IMARES/FAMA/ICES/WGWIDE/August_2018/NSHM/data/survey_data/"


####################-
# Data CGFS ----
####################-

##### First the length distribution
#cgfs <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data/survey_data/DATRAS_all_years/Exchange_CGFS_HL_1988-2018.csv")
cgfs <- read.csv(paste0(datpath,"Exchange_CGFS_HL_1988-2018.csv"))
dim(cgfs)
# Select for horse mackerel. It is necessary using two codes for Trachurus trachurus, 
# the TSN code that was used until 1997 and the WoRMS AphiaID that whas used since 1998
cgfs=cgfs[cgfs$SpecCode==126822 | cgfs$SpecCode==168588,]
dim(cgfs)
cgfs$Survey=revalue(cgfs$Survey, c("FR-CGFS"="CGFS"))
dim(cgfs)
cgfs$HaulNo=paste(cgfs$Survey,"-",cgfs$Year,"-",cgfs$Ship,"-",cgfs$HaulNo,sep="")
select=c("Survey", "Year", "HaulNo", "LngtClass", "HLNoAtLngt","SubFactor")
cgfs=cgfs[,names(cgfs) %in% select]
head(cgfs)
names(cgfs)=c("Survey", "HaulNo", "Year", "SubFactor","LngtClas", "Number")
cgfs$HaulNo=factor(cgfs$HaulNo)
dim(cgfs)
head(cgfs)

##### Next the fishing sets information
#sets_cgfs=read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data/survey_data/DATRAS_all_years/Exchange_CGFS_HH_1988-2018.csv")
sets_cgfs=read.csv(paste0(datpath,"Exchange_CGFS_HH_1988-2018.csv"))
dim(sets_cgfs)
# sets_cgfs$StatRec= revalue(sets_cgfs$StatRec, c("2.70E+09"="27E8", "2.70E+10"="27E9", "2.80E+09"="28E8", "2.80E+10"="28E9", "2.90E+09"="29E8", "2.90E+10"="29E9", "3.00E+10"="30E9"))
# dim(sets_cgfs)
sets_cgfs$Survey=revalue(sets_cgfs$Survey, c("FR-CGFS"="CGFS"))
dim(sets_cgfs)
sets_cgfs$HaulNo=paste(sets_cgfs$Survey,"-",sets_cgfs$Year,"-",sets_cgfs$Ship,"-",sets_cgfs$HaulNo,sep="")
select=c("HaulNo","HaulDur", "ShootLat", "ShootLong", "Depth", "StatRec","DataType")
statisrec=sets_cgfs[,names(sets_cgfs) %in% select]
dim(statisrec)
head(statisrec)
names(statisrec)=c("HaulNo", "HaulDur", "ShootLat", "ShootLon", "StatRec", "Depth","DataType")
head(statisrec)

# I merge now the length distribution and the fishing set information.
cgfs=merge(cgfs, statisrec, by.x="HaulNo", by.y="HaulNo", all.x=T, all.y=T)
dim(cgfs)

# Remove the NA cases, those fishing sets for which there was no meassurement of fishes.
# cgfs2017=na.omit(cgfs2017)
cgfs <- cgfs[!is.na(cgfs$Number),]
dim(cgfs)

cgfs=cgfs[,c("HaulNo","Survey","Year","StatRec","HaulDur","ShootLat","ShootLon","Depth","LngtClas","Number","SubFactor","DataType")]
head(cgfs)

# Divide the Length class by 10 to present it in cm instead of mm
summary(cgfs$LngtClas)
cgfs$LngtClas=cgfs$LngtClas/10

# Remove length bellow zero in case there is any
cgfs=cgfs[cgfs$LngtClas>=0,]

dim(cgfs)
sum(table(cgfs$Depth))
# No hay p?rdida de datos por NA.

summary(cgfs)


####################-
# Ahora el IBTS ----
####################-

##### First the length distribution
#ibts <- read.csv("M:/My Documents/WGWIDE/Assessment 2019/NSHM/data/survey_data/DATRAS_all_years/Exchange_NS-IBTS_HL_1991-2018.csv")
ibts <- read.csv(paste0(datpath,"Exchange_NS-IBTS_HL_1991-2018.csv"))
dim(ibts)
# Select for horse mackerel. It is necessary using two codes for Trachurus trachurus, 
# the TSN code that was used until 1997 and the WoRMS AphiaID that whas used since 1998
ibts=ibts[ibts$SpecCode==126822 | ibts$SpecCode==168588,]
dim(ibts)

# # It seems that some fishing vessels meassure the fish in mm while others do it in cm. Here I find out which ones do it in mm.
# tapply(ibts$HLNoAtLngt,list(ibts$LngtClass, ibts$Ship), sum)
# 
# # In this year the vessels DANS2, END and SCO3 are the vessels that have meassured the fish in mm. Hence, I am going to divide by 10 the fishes meassured in these vessels.
# mm_ibts=ibts[ibts$Ship=="DAN2" | ibts$Ship=="END" | ibts$Ship=="SCO3", ]
# dim(mm_ibts)
# 
# cm_ibts=ibts[ibts$Ship=="58UO" | ibts$Ship=="DANS" | ibts$Ship=="WAH3", ]
# dim(cm_ibts)
# 
# mm_ibts$LngtClass=mm_ibts$LngtClass/10
# dim(mm_ibts)
# 
# ibts=rbind(mm_ibts, cm_ibts)
# dim(ibts)
# 
# # Checking that all lengths are in cm now
# tapply(ibts$HLNoAtLngt,list(ibts$LngtClass, ibts$Ship), sum)


# Check length measurement
table(ibts$LngtCode) # '.' in mm, '1' in cm, '-9' is invalid
tapply(ibts$HLNoAtLngt,list(ibts$LngtClass, ibts$Ship), sum)
# Convert from mm to cm where necessary
ibts$LngtClass <- with(ibts,ifelse(LngtCode %in% "1" & !LngtCode %in% "-9",LngtClass,LngtClass/10))
tapply(ibts$HLNoAtLngt,list(ibts$LngtClass, ibts$Ship), sum)

# Remove length below zero in case there is any
ibts=ibts[ibts$LngtClass>=0,]
dim(ibts)


# Now continue preparing the database.
ibts$HaulNo=paste(ibts$Survey,"-",ibts$Year,"-",ibts$Ship,"-",ibts$HaulNo,sep="")
dim(ibts)
select=c("Survey", "Year", "HaulNo", "LngtClass", "HLNoAtLngt","SubFactor")
dim(ibts)
ibts=ibts[,names(ibts) %in% select]
dim(ibts)
head(ibts)
names(ibts)=c("Survey", "HaulNo", "Year", "SubFactor","LngtClas", "Number")
head(ibts)


##### Next the fishing sets information
sets_ibts <- read.csv(paste0(datpath,"Exchange_NS-IBTS_HH_1991-2018.csv"))
dim(sets_ibts)
# sets_ibts$StatRec= revalue(sets_ibts$StatRec, c("3.80E+10"="38E9", "3.90E+09"="39E8", "3.90E+10"="39E9", "4.00E+09"="40E8", "4.00E+10"="40E9", "4.10E+08"="41E7", "4.10E+09"="41E8", "4.10E+10"="41E9", "4.20E+08"="42E7", "4.20E+09"="42E8", "4.20E+10"="42E9", "4.30E+09"="43E8", "4.30E+10"="43E9", "4.40E+07"="44E6", "4.40E+08"="44E7", "4.40E+09"="44E8", "4.40E+10"="44E9", "4.50E+07"="45E6", "4.50E+08"="45E7", "4.50E+09"="45E8", "4.50E+10"="45E9", "4.60E+07"="46E6", "4.60E+08"="46E7", "4.60E+09"="46E8", "4.60E+10"="46E9", "4.70E+07"="47E6", "4.70E+08"="47E7", "4.70E+09"="47E8", "4.70E+10"="47E10", "4.80E+07"="48E6", "4.80E+08"="48E7", "4.80E+09"="48E8", "4.80E+10"="48E9", "4.90E+07"="49E6", "4.90E+08"="49E7", "4.90E+09"="49E8", "4.90E+10"="49E9", "5.00E+08"="50E7", "5.00E+09"="50E8", "5.00E+10"="50E9", "5.10E+09"="51E8", "5.10E+10"="51E9", "5.20E+10"="52E9"))
# dim(sets_ibts)
sets_ibts$HaulNo=paste(sets_ibts$Survey,"-",sets_ibts$Year,"-",sets_ibts$Ship,"-",sets_ibts$HaulNo,sep="")
selec=c("HaulNo","HaulDur", "ShootLat", "ShootLong", "Depth", "StatRec","DataType")
statisrec=sets_ibts[,names(sets_ibts) %in% selec]
head(statisrec)
names(statisrec)=c("HaulNo","HaulDur", "ShootLat", "ShootLon", "StatRec", "Depth","DataType")
dim(sets_ibts)
head(statisrec)

# I merge now the length distribution and the fishing set information.
ibts=merge(ibts, statisrec, by.x="HaulNo", by.y="HaulNo", all.x=T, all.y=T)
dim(ibts)

# Remove the NA cases, those fishing sets for which there was no meassurement of fishes.
# ibts=na.omit(ibts)
ibts <- ibts[!is.na(ibts$Number),]
dim(ibts)

ibts=ibts[,c("HaulNo","Survey","Year","StatRec","HaulDur","ShootLat","ShootLon","Depth","LngtClas","Number","SubFactor","DataType")]
head(ibts)


dim(ibts)
sum(table(ibts$Depth))
# No hay p?rdida de datos por NA.
summary(ibts)


# Ahora fusiono la base de datos con todos los surveys previos con la que contiene el survey del 2017
data=rbind(cgfs,ibts)
data <- data[,c("HaulNo","Survey","Year","StatRec","HaulDur","ShootLat","ShootLon","Depth","LngtClas","Number","SubFactor","DataType")]
summary(data)

write.table(data,paste0(resultpath,"survey_data_1991-2018.txt"),row.names = F)



# Check number data
#library(ggplot2)
#datplot <- data
#str(datplot)
#datplot$Year <- as.numeric(as.character(datplot$Year))
#str(datplot)
#p <- ggplot(datplot,aes(x=Year,y=sqrt(Number),group=Year)) +
#  geom_boxplot() +
#  facet_wrap(~Survey) + ggtitle("")
#print(p)










