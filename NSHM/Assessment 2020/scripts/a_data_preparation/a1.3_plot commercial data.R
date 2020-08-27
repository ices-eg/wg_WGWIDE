############################################################################################################-
# Plot commercial data
#
# Created by Alfonso Pérez Rodríguez, adapted by Benoit Berges (2019) and Esther Beukhof (2020)
############################################################################################################-

library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)

#################### define paths #######################-

dataPath  <- "NSHM/Assessment 2020/data/commercial/"
resPath   <- "NSHM/Assessment 2020/results/commercial/"
figPath   <- "NSHM/Assessment 2020/figures/commercial/"

# Set last year
endYear <- 2019


#################### read data   #########################-

# Commercial catch data
load(paste0(resPath,"NSHM_FLStock.RData"))

# Historic TAC data
hist_TAC        <- read.table(paste0(dataPath,"TAC/hist_TAC.csv"),sep=',', header = TRUE, check.names=FALSE)
hist_TAC        <- hist_TAC[hist_TAC$year <= endYear,]



###########################################################################################-
################################## Output tables   ########################################
###########################################################################################-


#################### Annual catch: total, discards and landings   #########################

catchtArray            <- array(NA,dim=c(length(dimnames(stk@catch)$year),6))
rownames(catchtArray)  <- dimnames(stk@catch)$year
colnames(catchtArray)  <- c('TAC','catches','utilisation','landings','discards','discard_rate')

catchtArray[,'discard_rate']  <- stk@discards[,,,,'all']/stk@catch[,,,,'all']*100
catchtArray[,'landings']      <- stk@landings[,,,,'all']
catchtArray[,'discards']      <- stk@discards[,,,,'all']
catchtArray[,'catches']       <- stk@catch[,,,,'all']
catchtArray[match(as.character(unique(hist_TAC$year)),rownames(catchtArray)),
            'utilisation'] <- stk@catch[,match(as.character(unique(hist_TAC$year)),rownames(catchtArray)),,,'all']/
  hist_TAC[hist_TAC$country == 'all',]$TAC
catchtArray[match(as.character(unique(hist_TAC$year)),rownames(catchtArray)),'TAC'] <- hist_TAC[hist_TAC$country == 'all',]$TAC

write.table(catchtArray,paste0(resPath,'catch_TAC.csv'),sep=',',col.names=TRUE, row.names = FALSE)



###########################################################################################-
#################################### Figures   ############################################
###########################################################################################-

#################### Annual catch: total, discards and landings   #########################

png(paste(figPath, "catch_landings_discards_year.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

startYear <- 2000
maxYaxis <- max(stk@catch[,,,,'all'])*1e-3

par(mar=c(5,5,3,3)) 
plot(as.numeric(colnames(stk@catch[,,,,'all'])),
     stk@catch[,,,,'all']*1e-3,
     type="b",pch=19,col="black",bty="l",xlab="Year",ylab="",
     xlim=c(startYear,max(as.numeric(colnames(stk@catch[,,,,'all'])))),
     ylim=c(0,maxYaxis+5),cex.axis=1.2,cex.lab=1.5,las=1)
mtext(expression(paste(plain("Catch x10") ^{3}, plain(" tons"))), side = 2, line = 3, cex = 1.5)
lines(as.numeric(colnames(stk@landings[,,,,'all'])),
      stk@landings[,,,,'all']*1e-3,
      type="b",pch=19,col="blue")
lines(as.numeric(colnames(stk@discards[,,,,'all'])),
      stk@discards[,,,,'all']*1e-3,
      type="b",pch=19,col="darkred")
legend("topright",legend=c("Catch","Landings","Discards"),pch=19,lty=1,col=c("black","blue","darkred"),bty="n",cex=1.5)

dev.off()


#############################  Annual catch, discards and landings by area  ###############################

png(paste(figPath, "catch_landings_discards_year_area.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

layout(matrix(c(1:6),2,3,byrow=T))

par(oma=c(2,7,2,2))
par(mar=c(3,3,2,2)) 

startYear <- 2000
areas <- dimnames(stk)$area
areas <- areas[areas != 'all']
maxYaxis <- max(apply(stk@catch[,,,,areas],2,max, na.rm=TRUE))*1e-3

flag  <- 0
for (iArea in areas)
{
  plot(as.numeric(colnames(stk@catch[,,,,iArea])),
       stk@catch[,,,,iArea]*1e-3,
       type="b",pch=19,col="black",bty="l",xlab="",ylab="",
       xlim=c(startYear,endYear),
       ylim=c(0,maxYaxis),cex.axis=1.2,
       cex.lab=1.5,las=1,main=list(paste("area  ",iArea,sep=""),cex=1.5))
  lines(as.numeric(colnames(stk@landings[,,,,iArea])),
        stk@landings[,,,,iArea]*1e-3,
        type="b",pch=19,col="blue")
  
  x <- as.numeric(colnames(stk@catch[,,,,iArea]))
  y <- stk@discards[,,,,iArea]*1e-3
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x,
        y,
        type="b",pch=19,col="darkred")
  if(flag==0){
    legend("topright",legend=c("Catch","Landings","Discards"),pch=19,lty=1,col=c("black","blue","darkred"),bty="n",cex=1.5)
    flag <- 1
  }
}
mtext(expression(paste(plain("Catch x10") ^{3}, plain(" tons"))), side = 2, line = 3, cex = 1.5,outer=T)
mtext("Year", side = 1, line = 0, cex = 1.5,outer=T)

dev.off()


######################### catch, landings, discards year 2019 by area  ######################################

tempVar <- as.data.frame(stk)
tempVar <- tempVar[ tempVar$slot == 'catch' | tempVar$slot == 'discards' | tempVar$slot == 'landings',]
tempVar <- tempVar[ tempVar$age == 'all' & tempVar$year == 2019,]
tempVar <- tempVar[tempVar$area != 'all',]


png(paste(figPath, paste("catch_landings_discards_area_",as.character(endYear),'.png'), sep=""), 
    width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(tempVar, aes(area, fill=slot, weight=data)) 
p = p + geom_bar(position="dodge", width=0.75) 
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle(paste0("Catch year ",endYear)) + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
p = p + scale_fill_manual(values = c("deepskyblue4","gold3","firebrick4"))
print(p)

dev.off()



####################### total catch areas 7d and the other 4 areas (4b, 4c, 4a and 3a) ########################

startYear <- 2000

tempVar <- as.data.frame(stk)
tempVar <- tempVar[   tempVar$slot == 'catch' & 
                        tempVar$age == 'all' & 
                        tempVar$area != 'all' & 
                        tempVar$year >= startYear,]
tempVar$data <- tempVar$data*1e-3

png(paste(figPath, "annual_catch_by_area.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(tempVar, aes(year, fill=area, weight=data)) 
p = p + geom_bar(position="stack", width=0.75) 
p= p + scale_fill_brewer(palette="Dark2")
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by area") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


####################### proportion catch areas 7d and the other 4 areas (4b, 4c, 4a and 3q) ####################### 

startYear <- 2000

stk2 <- stk[,,,,1:5]
tempVar <- drop(stk2@catch)/drop(areaSums(stk2@catch))
for(iArea in colnames(tempVar)){
  stk2@catch[,,,,iArea] <- tempVar[,colnames(tempVar) == iArea]
}
tempVar <- as.data.frame(stk2)
tempVar <- tempVar[   tempVar$slot == 'catch' & 
                        tempVar$age == 'all' & 
                        tempVar$area != 'all' & 
                        tempVar$year >= startYear,]


png(paste(figPath, "annual_catch_by_area_proportions.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(tempVar, aes(year, fill=area, weight=data)) 
p = p + geom_bar(position="stack", width=0.75) 
p= p + scale_fill_brewer(palette="Dark2")
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Proportion of catch")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Proportional catch by area") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


#######################  caton 7.d versus other areas  ####################### 

startYear <- 2000

tempVar <- as.data.frame(stk)
tempVar <- tempVar[   tempVar$slot == 'catch' & 
                        tempVar$age == 'all' & 
                        tempVar$area != 'all' & 
                        tempVar$year >= startYear,]
tempVar$data <- tempVar$data*1e-3

addLevel <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Other")))
  return(x)
}

tempVar <- as.data.frame(lapply(tempVar, addLevel))
tempVar$area[tempVar$area != '27.7.d'] <- 'Other'


png(paste(figPath, "annual_catch_27.7.d_versus_otherares.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(tempVar, aes(year, fill=area, weight=data)) 
p = p + geom_bar(position="stack", width=0.75) 
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch area 27.7.d versus other areas") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
p = p + scale_fill_manual(values = c("deepskyblue4","firebrick4"))
print(p)

dev.off()


########################  caton 7.d versus other areas in proportions  #######################  

startYear <- 2000

stk2 <- stk[,,,,1:5]
tempVar <- drop(stk2@catch)/drop(areaSums(stk2@catch))
for(iArea in colnames(tempVar)){
  stk2@catch[,,,,iArea] <- tempVar[,colnames(tempVar) == iArea]
}
tempVar <- as.data.frame(stk2)
tempVar <- tempVar[   tempVar$slot == 'catch' & 
                        tempVar$age == 'all' & 
                        tempVar$area != 'all' & 
                        tempVar$year >= startYear,]

addLevel <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "Other")))
  return(x)
}

tempVar <- as.data.frame(lapply(tempVar, addLevel))
tempVar$area[tempVar$area != '27.7.d'] <- 'Other'


png(paste(figPath, "annual_catch_27.7.d_versus_otherares_proportions.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(tempVar, aes(year, fill=area, weight=data)) 
p = p + geom_bar(position="stack", width=0.75) 
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Proportion of catch")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Proportional catch area 27.7.d versus other areas") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
p = p + scale_fill_manual(values = c("deepskyblue4","firebrick4"))
print(p)

dev.off()


###################### Distribution of catches country  ###################################

# Load catch data from InterCatch output
data=read.csv(paste0(dataPath,"InterCatch/Ages from InterCatch/NSHM_2019_catch_table.csv"), stringsAsFactors = FALSE)

# Group some categories or change names
data$CatchCategory=revalue(data$CatchCategory, c("BMS landing"="Landings", "Logbook Registered Discard"="Discards"))
data$Area = revalue(data$Area,c("27.3.a.nshm"="27.3.a","27.4.a.nshm"="27.4.a"))

# Select data of interest
choice=c("Country","Year","CatchCategory","Season","CATON")
data=data[,names(data) %in% choice]
colnames(data)=c("country","year","category","season","catch")
data$season[data$season == 2019] <- "all"


#catch in tons
data$catch=data$catch/1000


png(paste(figPath, "catch_country_quarter.png", sep=""), width=1500, heigh= 1500, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(country, fill=season, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set1", name="Quarter")
p = p + theme(text = element_text(size=10))+labs(x="Country",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8,angle = 90)) + 
  theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by country and quarter") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()



###################### Distribution of catches by season and country  ###################################

# Load catch data from InterCatch output
data=read.csv(paste0(dataPath,"InterCatch/Ages from InterCatch/NSHM_2019_catch_table.csv"), stringsAsFactors = FALSE)

# Group some categories or change names
data$CatchCategory=revalue(data$CatchCategory, c("BMS landing"="Landings", "Logbook Registered Discard"="Discards"))
data$Area = revalue(data$Area,c("27.3.a.nshm"="27.3.a","27.4.a.nshm"="27.4.a"))


# Select data of interest
choice=c("Country","Year","CatchCategory","Area","Season","CATON")
data=data[,names(data) %in% choice]
colnames(data)=c("country","year","category","area","season","catch")

#sum up UK catches and replace main data where UK data is split up
UKcatches         <- aggregate(catch ~ category + area + season, data=data[data$country %in% c("UK (England)","UK(Northern Ireland)","UK(Scotland)"),], FUN=sum)
UKcatches$country <- "UK"
UKcatches$year    <- 2019
UKcatches$season[UKcatches$season == 2019] <- "all"
UKcatches         <- UKcatches[,c("country","year","category","area","season","catch")]
data              <- subset(data, !country %in% c("UK (England)","UK(Northern Ireland)","UK(Scotland)"))
data              <- rbind(data,UKcatches)

#catch in tons
data$catch=data$catch/1000


png(paste(figPath, "catch_quarter_country.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(season, fill=country, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set3", )
p = p + theme(text = element_text(size=10))+labs(x="Quarter",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by quarter and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()



###################### Distribution of catches by season and area  ###################################

png(paste(figPath, "catch_quarter_area.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(season, fill=area, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set1")
p = p + theme(text = element_text(size=10))+labs(x="Quarter",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by quarter and area") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()



###################### Distribution of catches by category and area  ###################################

png(paste(figPath, "catch_area_category.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(area, fill=category, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set1")
p = p + theme(text = element_text(size=10))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by season and category") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()



###################### Different figures for distribution of catches by area, season, category and area  #################################

png(paste(figPath, "catch_category_country.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(category, fill=country, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set3")
p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch category per country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()



png(paste(figPath, "catch_area_quarter_category_country.png", sep=""), width=2500, heigh= 1150, units="px", pointsize=5, bg="white", res=300)

p=ggplot(data,aes(category, fill=country, weight=catch))
p= p + geom_bar(position="stack", width=0.75) + facet_wrap(~ area + season, nrow=1)
p= p + scale_fill_brewer(palette="Set3")
p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by area, quarter, category and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


png(paste(figPath, "catch_area_category_country.png", sep=""), width=2500, heigh= 1150, units="px", pointsize=5, bg="white", res=300)

p=ggplot(data,aes(category, fill=country, weight=catch))
p= p + geom_bar(position="stack", width=0.75) + facet_wrap(~ area, nrow=1)
p= p + scale_fill_brewer(palette="Set3")
p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by area, category and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()



####################  Table with catch by area, category and country   ####################  

table=as.data.frame(tapply(round(data$catch,digits=0), list(data$area, data$country), sum))
table$area=rownames(table)
table=table[,c(dim(table)[2],1:dim(table)[2]-1)]
table[is.na(table)]=0

write.table(table,paste0(resPath,'catch_area_country.csv'),sep=',',col.names=TRUE, row.names = FALSE)

### PDF
pdf(file = paste(figPath, "table_catch_area_category_country.pdf", sep=""))

grid.table(table, rows=NULL, cols=colnames(table), theme=ttheme_default(base_size = 5))

dev.off()





#################### Mean weight at age   #########################

startYear <- 2000

tempVar <- as.data.frame(stk@catch.wt[as.character(1:10),as.character(startYear:endYear),,,'all'])
tempVar$age <- factor(tempVar$age, levels=c("1","2","3","4","5","6","7","8","9","10"))


png(paste(figPath, "mean_weight_at_age.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(tempVar, aes(x = year, y = data, colour=age, group = age)) + geom_point() + geom_line() 
p = p + theme(text = element_text(size=10))+labs(x="Year",y="Weight (kg)")  
p = p  + ggtitle("Mean weight at age (kg)") + theme(plot.title = element_text(color="grey40", size=5, face="bold.italic", hjust=0.5))
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8), legend.key = element_rect(fill = "white")) 
p = p + theme(plot.title = element_text(size = 12, face = "bold"))
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p + theme(axis.line = element_line(color="grey40", size = 0.3))

print(p)

dev.off()



############################ Mean length at age  ##############################

startYear <- 2000

tempVar <- as.data.frame(stk@m[as.character(1:10),as.character(startYear:endYear),,,'all'])
tempVar$age <- factor(tempVar$age, levels=c("1","2","3","4","5","6","7","8","9","10"))


png(paste(figPath, "mean_length_at_age.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(tempVar, aes(x = year, y = data, color = age, group = age)) + geom_point() + geom_line() 
p = p + theme(text = element_text(size=10))+labs(x="Year",y="Length (cm)")  
p = p  + ggtitle("Mean length at age (cm)") + theme(plot.title = element_text(color="grey40", size=5, face="bold.italic", hjust=0.5))
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8), legend.key = element_rect(fill = "white")) 
p = p + theme(plot.title = element_text(size = 12, face = "bold"))
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p + theme(axis.line = element_line(color="grey40", size = 0.3))
print(p)

dev.off()



######################### Catch at age in numbers (millones)  all areas ##############################

catch=read.csv(paste0(dataPath,"canum.csv"), stringsAsFactors = FALSE)
colnames(catch)[2:ncol(catch)] <- c(1995:endYear)
catch=melt(catch,id=c("Age"))
colnames(catch) <- c("age","year",'catch')
#catch=rename(catch,c(Age="age", variable="year", value="catch"))
#catch$year=as.numeric(substring(catch$year,2,5))
#catch$age=as.numeric(as.character(catch$age))
catch$age[is.na(catch$age)]=15
catch=catch[catch$age<16,]
catch$age=factor(catch$age)
catch$catch[is.na(catch$catch)]=0
catch$year=as.numeric(levels(catch$year))[catch$year]


png(paste(figPath, "catch_N_at_age.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
p = ggplot(data=catch, aes(x=year, y=age)) 
p = p + geom_point(aes(size=catch/max(catch)),alpha=0.5)
p = p + scale_shape(solid = FALSE) 
p = p + scale_size_continuous(range=c(0,8))
p = p + scale_x_continuous(breaks=seq(min(catch$year), max(catch$year), by = 5))
p = p +  theme(legend.position = "none")
p = p  + ggtitle("NSHM: catch at age (N; observed) all areas") + theme(plot.title = element_text(color="grey40", size=7, face="bold.italic", hjust=0.5))
p = p + theme(plot.title = element_text(size = 15, face = "bold"))
p = p + theme(text = element_text(size=15))
print(p)
dev.off()




######################### Catch at age in numbers (millones) only in 7d  ##############################

dat=read.csv(paste0(dataPath,"canum_area.csv"), stringsAsFactors = FALSE)

dat=dat[dat$age>0,]

#colnames(dat)=c("age","year","3.a","4.a","4.b","4.c","4.bc","7d")

colnames(dat)=c("age","year","IIIa","IVa","IVb","IVc","VIId")

dat[is.na(dat)]=0

dat$outVIId=rowSums(dat[,3:7])

dat$total=dat$VIId+dat$outVIId


png(paste(figPath, "catch_N_at_age_7d.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
p = ggplot(data=dat, aes(x=year, y=age)) 
p = p + geom_point(aes(size=VIId/max(total)),alpha=0.5)
p = p + scale_shape(solid = FALSE) 
p = p + scale_size_continuous(range=c(0,10))
p = p +  theme(legend.position = "none")
p = p  + ggtitle("NSHM: catch at age (N; observed) 27.7.d") + theme(plot.title = element_text(color="grey40", size=6.5, face="bold.italic", hjust=0.5))
p = p + theme(plot.title = element_text(size = 14, face = "bold"))
p = p + theme(text = element_text(size=15))
print(p)
dev.off()


png(paste(figPath, "catch_N_at_age_other_minus_7d.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
p = ggplot(data=dat, aes(x=year, y=age)) 
p = p + geom_point(aes(size=outVIId/max(total)),alpha=0.5)
p = p + scale_shape(solid = FALSE) 
p = p + scale_size_continuous(range=c(0,10))
p = p +  theme(legend.position = "none")
p = p  + ggtitle("NSHM: catch at age (N; observed) out of 27.7.d") + theme(plot.title = element_text(color="grey40", size=5, face="bold.italic", hjust=0.5))
p = p + theme(plot.title = element_text(size = 14, face = "bold"))
p = p + theme(text = element_text(size=15))
print(p)
dev.off()



#########################  Catch Curves y Total mortality (Z) by cohort  ################################

endYearCohort   <- 2008
startYearCohort <- 1992
stkCohort <- FLCohort(log(stk@catch.n[,,,,'all']))

png(paste(figPath, "Catch_curves_by_cohort.png", sep=""), width=2500, heigh= 1500, units="px", pointsize=7, bg="white", res=300)
plot(stkCohort[,as.character(startYearCohort:endYearCohort)])
dev.off()

coh       <- startYearCohort:endYearCohort
cont      <- length(coh)
par_a     <- vector(length=cont)
par_b     <- vector(length=cont)
pvalue_b  <- vector(length=cont)
for(i in 1:cont)
{
  datsel=data[data$cohort==coh[i],]
  
  datsel <- as.data.frame(stkCohort[,as.character(coh[i])])
  # Elimino los a?os sin capturas (NA) y los que se apuntaron como 0 (problemas con logaritmos)
  datsel <- datsel[!is.na(datsel$data),]
  datsel <- datsel[datsel$data>0,]
  model=lm(log(data)~age,data=datsel)
  par_a[i]=model$coefficients[1]
  par_b[i]=model$coefficients[2]
  pvalue_b[i]=summary(model)$coef[8]
  plot(datsel$age,log(datsel$data),type="b",main=coh[i])
}

result=data.frame(par_a,par_b,pvalue_b)

png(paste(figPath, "Catch_curves_by_cohort_par_slope.png", sep=""), width=2500, heigh= 1500, units="px", pointsize=10, bg="white", res=300)
plot(coh,-par_b, type="b", pch=19, xlab="Cohort", ylab="Total mortality Z", main="Total mortality by cohort", cex.main=1.8, cex.axis=1.3, cex.lab=1.5, ylim=c(0,max(-par_b)))
dev.off()

write.csv(drop(stkCohort),paste(resPath,"catch_at_age_cohort.csv",sep=","), row.names=F)



####################  catch against TAC for current year  #########################

windows()

cl        <- 1.5
ca        <- 1.2
fam       <- ""
fonts     <- 2
parmar    <- rep(0.4,4)
paroma    <- (c(4,4,1,1)+0.1)
mtextline <- 4
ltextcex  <- 1.2
figtype   <- "png"

catch           <- read.csv(paste0(dataPath,"caton_area_cat.csv"))
catchCountry    <- read.csv(paste0(dataPath,"InterCatch/Ages from InterCatch/NSHM_2019_catch_table.csv"), sep=",", head=T)
historical_TAC  <- read.csv(paste0(dataPath,"TAC/hist_TAC.csv"), sep=",", head=T)

uniqueCountries <- as.character(unique(historical_TAC$country))

outArray            <- array(NA,dim=c(2,length(uniqueCountries)))
rownames(outArray)  <- c('TAC','TAC_use')
colnames(outArray)  <- uniqueCountries

for(i in uniqueCountries){
  if(i == 'UK'){
    iUK <- c('UK (England)','UK(Scotland)')
    outArray['TAC',i]     <- subset(historical_TAC, year == endYear & country == i)$TAC
    outArray['TAC_use',i] <- sum(subset(catchCountry, Country == iUK[1] | Country == iUK[2])$CATON)*1e-3
  } else if((i == 'all')){
    outArray['TAC',i]     <- subset(historical_TAC, year == endYear & country == i)$TAC
    outArray['TAC_use',i] <- sum(catchCountry$CATON)*1e-3
    
  } else if(is.na(match(i,catchCountry$Country))){
    outArray['TAC',i]     <- subset(historical_TAC, year == endYear & country == i)$TAC
    outArray['TAC_use',i] <- 0
  } else{
    outArray['TAC',i]     <- subset(historical_TAC, year == endYear & country == i)$TAC
    outArray['TAC_use',i] <- sum(subset(catchCountry, Country == i)$CATON)*1e-3
  }
}

barplot(outArray,beside=T,col = c('blue','red'),las=2,main='2019 TAC utilisation')
legend("topright", 
       legend = c("TAC", "TAC use"), 
       fill = c("blue", "red"))

savePlot(paste0(figPath,"Catch_TAC_country"),type=figtype)



####################  historical TAC and utilisation (since 2000)  #########################

uniqueYears <- 2000:2019 # historical TAC only from 2000 in table
outArray    <- array(NA,dim=c(length(uniqueYears),6))
rownames(outArray)  <- uniqueYears
colnames(outArray)  <- c('TAC','catches','utilisation','landings','discards','discard_rate')

for(i in uniqueYears){
  outArray[as.character(i),'TAC']     <- historical_TAC$TAC[historical_TAC$year == i & historical_TAC$country == 'all']
  outArray[as.character(i),'catches'] <- sum(catch$caton[catch$Year == i])
  outArray[as.character(i),'utilisation'] <- outArray[as.character(i),'catches']/outArray[as.character(i),'TAC']
  
  outArray[as.character(i),'landings']  <- sum(catch$caton[   catch$Year == i & 
                                                                catch$CatchCategory == 'Landings' |
                                                                catch$CatchCategory == 'BMS landing'])
  outArray[as.character(i),'discards']  <- sum(catch$caton[   catch$Year == i & 
                                                                catch$CatchCategory == 'Discards'])
  
  outArray[as.character(i),'discard_rate'] <- outArray[as.character(i),'discards']/outArray[as.character(i),'catches']*100
}

write.csv(outArray,paste(resPath,"TAC_catch_TAC_use_land_disc_discrate_2000-2019.csv",sep=""),row.names = TRUE)

xrange <- c(2000,endYear)
yrange <- range(pretty(c(0,max(outArray,na.rm=T))))

windows()

par(oma=paroma,yaxs="i")
plot(0,0,col="white",xlim=xrange,ylim=yrange,xlab="Years",ylab="",
     cex.lab=cl,cex.axis=ca,font.lab=fonts,font=fonts,las=1)
mtext(side=2,line=mtextline,text="Catch / TAC (tonnes)",cex=cl,font=fonts)
rect((2000:(endYear))-0.5,0,(2000:(endYear))+0.5,outArray[,'catches'],col="grey",lwd=2)
lines(x=rep(2000:(endYear),each=2)+c(-0.5,0.5),y=rep(outArray[,'TAC'],each=2),lwd=4)
legend("bottom",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",bg="white",pt.cex=c(2),box.lty=0)
title(main="Catch and TAC")

savePlot(paste0(figPath,"historic_Catch_TAC"),type=figtype)
