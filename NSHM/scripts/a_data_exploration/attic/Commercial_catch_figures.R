

rm(list=ls())

library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)


inpath="C:/Users/berge057/OneDrive - WageningenUR/projects/WG/2019_WGWIDE/2019 assessment/NSHM/data/commercial_data/data_xa_analysis/"
outpath="C:/Users/berge057/OneDrive - WageningenUR/projects/WG/2019_WGWIDE/2019 assessment/NSHM/data_exploration/commercial catch/"

startYear <- 1999
endYear   <- 2018

#################### Annual catch: total, discards and landings   #########################

catch=read.csv(paste(inpath,"Nsea_CATON.csv",sep=""), sep=",", head=T)

colnames(catch)=c("year","area","category","catch")

catch$category=revalue(catch$category, c("BMS landing"="Landings", "Catch"="Landings", "Logbook Registered Discard"="Discards"))

catch$catch=catch$catch/1000000

catchyear=ddply(catch,.(year),summarize, catch=sum(catch))

catchyearcat=ddply(catch,.(year,category),summarize, catch=sum(catch))

catchlanding=subset(catchyearcat,category=="Landings")

catchdiscards=subset(catchyearcat,category=="Discards")

png(paste(outpath, "catch_landings_discards_year.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
par(mar=c(5,5,3,3)) 
plot(catchyear$year,
     catchyear$catch,
     type="b",pch=19,col="black",bty="l",xlab="Year",ylab="",xlim=c(startYear,endYear),ylim=c(0,max(catchyear$catch)+5),cex.axis=1.2,cex.lab=1.5,las=1)
mtext(expression(paste(plain("Catch x10") ^{3}, plain(" tons"))), side = 2, line = 3, cex = 1.5)
lines(catchlanding$year,catchlanding$catch,type="b",pch=19,col="blue")
lines(catchdiscards$year,catchdiscards$catch,type="b",pch=19,col="darkred")
legend("topright",legend=c("Catch","Landings","Discards"),pch=19,lty=1,col=c("black","blue","darkred"),bty="n",cex=1.5)
dev.off()


#############################  Annual catch, discards and landings by area  ###############################


png(paste(outpath, "catch_landings_discards_year_area.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

layout(matrix(c(1:6),2,3,byrow=T))

par(oma=c(2,7,2,2))
par(mar=c(3,3,2,2)) 

areas=sort(unique(catch$area))
contareas=(length(areas))
for (i in 1:contareas)
{
  dat=catch[catch$area==areas[i],]
  
  catchyear=ddply(dat,.(year),summarize, catch=sum(catch))
  catchyearcat=ddply(dat,.(year,category),summarize, catch=sum(catch))
  catchlanding=subset(catchyearcat,category=="Landings")
  catchdiscards=subset(catchyearcat,category=="Discards")
  plot(catchyear$year,
       catchyear$catch,
       type="b",pch=19,col="black",bty="l",xlab="",ylab="",xlim=c(startYear,endYear),ylim=c(0,max(catch$catch)),cex.axis=1.2,cex.lab=1.5,las=1,main=list(paste("area  ",as.character(areas[i]),sep=""),cex=1.5))
  lines(catchlanding$year,catchlanding$catch,type="b",pch=19,col="blue")
  lines(catchdiscards$year,catchdiscards$catch,type="b",pch=19,col="darkred")
  if(i==1){legend("topright",legend=c("Catch","Landings","Discards"),pch=19,lty=1,col=c("black","blue","darkred"),bty="n",cex=1.5)}
  
}
mtext(expression(paste(plain("Catch x10") ^{3}, plain(" tons"))), side = 2, line = 3, cex = 1.5,outer=T)
mtext("Year", side = 1, line = 0, cex = 1.5,outer=T)

dev.off()


######################### catch, landings, discards year 2017 by area  ######################################

catchCurrent=subset(catch,year==endYear)
catchCurrent=ddply(catchCurrent,.(year,area,category),summarize, catch=sum(catch))
catchCurrentcatch=ddply(catchCurrent,.(year,area),summarize, catch=sum(catch))
catchCurrentcatch$category=rep("catch",length(catchCurrentcatch$year))
catchCurrentcatch=catchCurrentcatch[,c("year","area","category","catch")]
catchCurrent=rbind(catchCurrent,catchCurrentcatch)

png(paste(outpath, 
          paste("catch_landings_discards_area_",as.character(endYear),'.png'), 
          sep=""), 
    width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(catchCurrent, aes(area, fill=category, weight=catch)) 
p = p + geom_bar(position="dodge", width=0.75) 
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch year 2018") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
p = p + scale_fill_manual(values = c("deepskyblue4","gold3","firebrick4"))
print(p)

dev.off()



####################### total catch areas 7d and the other 4 areas (4b, 4c, 4a and 3q)

catch=read.csv(paste(inpath,"Nsea_CATON_div_2000_18.csv",sep=""),sep=",", head=T)

colnames(catch)=c("year","area","category","catch")

catch$category=revalue(catch$category, c("BMS landing"="Landings", "Catch"="Landings", "Logbook Registered Discard"="Discards"))

catch$catch=catch$catch/1000000

catch$area = revalue(catch$area,c("27.3.a.nshm"="27.3.a","27.4.a.nshm"="27.4.a"))

catch=ddply(catch,.(year,area),summarize, catch=sum(catch))

total=ddply(catch,.(year),summarize,total=sum(catch))

catch=merge(catch,total,by.x="year",by.y="year")

catch$prop=catch$catch/catch$total

png(paste(outpath, "annual_catch_by_area.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(catch, aes(year, fill=area, weight=catch)) 
p = p + geom_bar(position="stack", width=0.75) 
p= p + scale_fill_brewer(palette="Dark2")
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch areas 27.7.d versus other areas") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


png(paste(outpath, "annual_catch_by_area_proportions.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(catch, aes(year, fill=area, weight=prop)) 
p = p + geom_bar(position="stack", width=0.75) 
p= p + scale_fill_brewer(palette="Dark2")
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch areas 27.7.d versus other areas") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


# Ahora con las áreas 27.3a, 27.4.a, 27.4.b y 27.4.c juntoas como "others"

catch=read.csv(paste(inpath,"Nsea_CATON_div_2000_18.csv",sep=""),sep=",",head=T)

colnames(catch)=c("year","area","category","catch")

catch$category=revalue(catch$category, c("BMS landing"="Landings", "Catch"="Landings", "Logbook Registered Discard"="Discards"))

catch$catch=catch$catch/1000000

catch$area_II = revalue(catch$area,c("27.3.a.nshm"="others","27.4.a.nshm"="others","27.4.b"="others","27.4.c"="others","27.7.d"="27.7.d"))

catch=ddply(catch,.(year,area_II),summarize, catch=sum(catch))

total=ddply(catch,.(year),summarize,total=sum(catch))

catch=merge(catch,total,by.x="year",by.y="year")

catch$prop=catch$catch/catch$total

png(paste(outpath, "annual_catch_27.7.d_versus_otherares.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(catch, aes(year, fill=area_II, weight=catch)) 
p = p + geom_bar(position="stack", width=0.75) 
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch areas 27.7.d versus other areas") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
p = p + scale_fill_manual(values = c("deepskyblue4","firebrick4"))
print(p)

dev.off()


png(paste(outpath, "annual_catch_27.7.d_versus_otherares_proportions.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p = ggplot(catch, aes(year, fill=area_II, weight=prop)) 
p = p + geom_bar(position="stack", width=0.75) 
p = p + theme(text = element_text(size=8))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 6.5)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch areas 27.7.d versus other areas") + theme(plot.title = element_text(color="grey40", size=10, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
p = p + scale_fill_manual(values = c("deepskyblue4","firebrick4"))
print(p)

dev.off()



###################### Distribution of catches by season and country  ###################################

data=read.csv(paste(inpath,"NSHM_2018_todo_catch.csv",sep=""), sep=",", head=T)

data$CatchCategory=revalue(data$CatchCategory, c("BMS landing"="Landings", "Logbook Registered Discard"="Discards"))

choice=c("Country","Year","CatchCategory","Area","Season","CATON")
data=data[,names(data) %in% choice]
colnames(data)=c("country","year","category","area","season","catch")

#catch in tons
data$catch=data$catch/1000

png(paste(outpath, "catch_season_country.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(season, fill=country, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set1")
p = p + theme(text = element_text(size=10))+labs(x="Season",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by season and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()

tapply(data$catch,list(data$season,data$country),sum)


###################### Distribution of catches by season and area  ###################################

png(paste(outpath, "catch_season_area.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(season, fill=area, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set1")
p = p + theme(text = element_text(size=10))+labs(x="Season",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by season and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


###################### Distribution of catches by category and area  ###################################


png(paste(outpath, "catch_area_category.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(area, fill=category, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set1")
p = p + theme(text = element_text(size=10))+labs(x="Area",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by season and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


###################### Different figures for distribution of catches by area, season, category and area  #################################


png(paste(outpath, "catch_category_country.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

p=ggplot(data,aes(category, fill=country, weight=catch))
p= p + geom_bar(position="stack", width=0.75)
p= p + scale_fill_brewer(palette="Set1")
p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by season and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()



data$area = revalue(data$area,c("27.3.a.nshm"="27.3.a","27.4.a.nshm"="27.4.a"))

png(paste(outpath, "catch_area_season_category_country.png", sep=""), width=2500, heigh= 1150, units="px", pointsize=5, bg="white", res=300)

p=ggplot(data,aes(category, fill=country, weight=catch))
p= p + geom_bar(position="stack", width=0.75) + facet_wrap(~ area + season, nrow=1)
p= p + scale_fill_brewer(palette="Set1")
p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by area, season, category and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


png(paste(outpath, "catch_area_category_country.png", sep=""), width=2500, heigh= 1150, units="px", pointsize=5, bg="white", res=300)

p=ggplot(data,aes(category, fill=country, weight=catch))
p= p + geom_bar(position="stack", width=0.75) + facet_wrap(~ area, nrow=1)
p= p + scale_fill_brewer(palette="Set1")
p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p  + ggtitle("Catch by area, category and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
print(p)

dev.off()


#############################  Table con catch by area, category and country #############


table=as.data.frame(tapply(round(data$catch,digits=0), list(data$area, data$country), sum))
table$area=rownames(table)
table=table[,c(dim(table)[2],1:dim(table)[2]-1)]
table[is.na(table)]=0

### PDF
pdf(file = paste(outpath, "table_catch_area_category_country.pdf", sep=""))

grid.table(table, rows=NULL, cols=colnames(table), theme=ttheme_default(base_size = 5))

dev.off()





#################### Mean weight at age   #########################

weight=read.csv(paste(inpath,"weight_at_age.csv",sep=""),head=T)

weight=melt(weight,id=c("Age"))
weight=rename(weight,c(Age="age", variable="year", value="weight"))
weight$year=as.numeric(substring(weight$year,2,5))
weight$age=as.numeric(as.character(weight$age))
weight$age[is.na(weight$age)]=15
weight=weight[weight$age<11,]
weight$age=factor(weight$age)
weight$weight[is.na(weight$weight)]=0

weight=weight[weight$year>1999,]
  
  png(paste(outpath, "mean_weight_at_age.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
  
  p=ggplot(weight, aes(x = year, y = weight, colour=age, group = age)) + geom_point() + geom_line() 
  p = p + theme(text = element_text(size=10))+labs(x="Year",y="Weight (kg)")  
  p = p  + ggtitle("Mean weigh at age (kg)") + theme(plot.title = element_text(color="grey40", size=5, face="bold.italic", hjust=0.5))
  p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8), legend.key = element_rect(fill = "white")) 
  p = p + theme(plot.title = element_text(size = 12, face = "bold"))
  p = p + theme(panel.background = element_rect(fill = "white")) 
  p = p + theme(axis.line = element_line(color="grey40", size = 0.3))
  
  print(p)
  
  dev.off()
  

############################ Mean length at age  ##############################


length=read.csv(paste(inpath,"length_at_age.csv",sep=""),head=T)

length=melt(length,id=c("Age"))
length=rename(length,c(Age="age", variable="year", value="length"))
length$year=as.numeric(substring(length$year,2,5))
length$age=as.numeric(as.character(length$age))
length$age[is.na(length$age)]=15
length=length[length$age<11,]
length$age=factor(length$age)
length$length[is.na(length$length)]=0

length=length[length$year>1999,]


png(paste(outpath, "mean_length_at_age.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
p=ggplot(length, aes(x = year, y = length, color = age, group = age)) + geom_point() + geom_line() 
p = p + theme(text = element_text(size=10))+labs(x="Year",y="Length (cm)")  
p = p  + ggtitle("Mean length at age (cm)") + theme(plot.title = element_text(color="grey40", size=5, face="bold.italic", hjust=0.5))
p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8), legend.key = element_rect(fill = "white")) 
p = p + theme(plot.title = element_text(size = 12, face = "bold"))
p = p + theme(panel.background = element_rect(fill = "white")) 
p = p + theme(axis.line = element_line(color="grey40", size = 0.3))
print(p)
dev.off()


######################### Catch at age in numbers (millones)  all areas ##############################


catch=read.csv(paste(inpath,"catch_N_at_age.csv",sep=""),head=T)

catch=melt(catch,id=c("Age"))
catch=rename(catch,c(Age="age", variable="year", value="catch"))
catch$year=as.numeric(substring(catch$year,2,5))
catch$age=as.numeric(as.character(catch$age))
catch$age[is.na(catch$age)]=15
catch=catch[catch$age<16,]
catch$age=factor(catch$age)
catch$catch[is.na(catch$catch)]=0


png(paste(outpath, "catch_N_at_age.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
p = ggplot(data=catch, aes(x=year, y=age)) 
p = p + geom_point(aes(size=catch/max(catch)),alpha=0.5)
p = p + scale_shape(solid = FALSE) 
p = p + scale_size_continuous(range=c(0,8))
p = p +  theme(legend.position = "none")
p = p  + ggtitle("NSHM: catch at age (N; observed) all areas") + theme(plot.title = element_text(color="grey40", size=7, face="bold.italic", hjust=0.5))
p = p + theme(plot.title = element_text(size = 15, face = "bold"))
p = p + theme(text = element_text(size=15))
print(p)
dev.off()




######################### Catch at age in numbers (millones) only in 7d  ##############################


dat=read.csv(paste(inpath,"catch_at_age_year_area.csv",sep=""),head=T,sep=",",dec=".")

dat=dat[dat$age>0,]

#colnames(dat)=c("age","year","3.a","4.a","4.b","4.c","4.bc","7d")

colnames(dat)=c("age","year","IIIa","IVa","IVb","IVc","IVbc","VIId")

dat[is.na(dat)]=0

dat$outVIId=rowSums(dat[,3:7])

dat$total=dat$VIId+dat$outVIId


png(paste(outpath, "catch_N_at_age_7d.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
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


png(paste(outpath, "catch_N_at_age_other_menos_7d.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
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


dat=read.csv(paste(inpath,"catch_N_at_age.csv",sep=""),head=T)

data=melt(dat, id=c("Age"))
data=rename(data,c(Age="age",variable="year", value="catch"))

data$year=as.integer(substr(data$year,2,5))

data$age=as.numeric(as.character(data$age))

data$age[is.na(data$age)]=15

data$cohort=data$year-data$age


# Preparo la base de datos para la figura

# Selecciono edades mayores de 3, que es cuando se considera que la selectividad al arte es total.

data=data[data$age>2 & data$age<15,]

select=tapply(data$catch,list(data$age,data$cohort),max)
select

# Selecciono las cohortes que tienen datos desde la edad 3 a la 10, y estas son de 1992 a 2004.

#data=subset(data,cohort>1991 | cohort<2005,select=c(age,catch,cohort))

data=data[data$cohort>1991,]
data=data[data$cohort<2008,]


library(lattice)
xyplot(log(catch)~age, groups=cohort, data=data, type="l")

data$Cohort=factor(data$cohort)
png(paste(outpath, "Catch_curves_by_cohort.png", sep=""), width=2500, heigh= 1500, units="px", pointsize=7, bg="white", res=300)
xyplot(log(catch)~age|Cohort, data=data, type="l", col="darkblue")
dev.off()


coh=sort(unique(data$cohort))

cont=length(coh)
par_a=vector(length=cont)
par_b=vector(length=cont)
pvalue_b=vector(length=cont)
for(i in 1:cont)
{
  datsel=data[data$cohort==coh[i],]
  # Elimino los a?os sin capturas (NA) y los que se apuntaron como 0 (problemas con logaritmos)
  datsel=na.exclude(datsel)
  datsel=datsel[datsel$catch>0,]
  model=lm(log(catch)~age,data=datsel)
  par_a[i]=model$coefficients[1]
  par_b[i]=model$coefficients[2]
  pvalue_b[i]=summary(model)$coef[8]
  plot(datsel$age,log(datsel$catch),type="b",main=coh[i])
}

result=data.frame(par_a,par_b,pvalue_b)

cohort=sort(unique(data$cohort))

png(paste(outpath, "Catch_curves_by_cohort_par_slope.png", sep=""), width=2500, heigh= 1500, units="px", pointsize=10, bg="white", res=300)
plot(cohort,-par_b, type="b", pch=19, xlab="Cohort", ylab="Total mortality Z", main="Total mortality by cohort", cex.main=1.8, cex.axis=1.3, cex.lab=1.5, ylim=c(0,max(-par_b+0.1)))
dev.off()

data$logcatch=log(data$catch)
write.csv(data,paste(outpath,"catch_at_age_cohort.csv",sep=","), row.names=F)

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
figtype   <- "emf"

catch           <- read.csv(paste(inpath,"Nsea_CATON_div_2000_18.csv",sep=""), sep=",", head=T)
catchCountry    <- read.csv(paste(inpath,"NSHM_2018_todo_catch.csv",sep=""), sep=",", head=T)
historical_TAC  <- read.csv(paste(inpath,"historical_TAC.csv",sep=""), sep=",", head=T)

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

barplot(outArray,beside=T,col = c('blue','red'),las=2,main='2018 TAC utilisation')
legend("topright", 
       legend = c("TAC", "TAC use"), 
       fill = c("blue", "red"))

savePlot(paste0(outpath,"Catch_TAC_country"),type=figtype)


####################  historical TAC and utilisation (since 2000)  #########################

uniqueYears <- unique(catch$Year)
outArray    <- array(NA,dim=c(length(uniqueYears),2))
rownames(outArray)  <- uniqueYears
colnames(outArray)  <- c('TAC','TAC_use')

for(i in uniqueYears){
  outArray[as.character(i),'TAC']     <- historical_TAC$TAC[historical_TAC$year == i & historical_TAC$country == 'all']
  outArray[as.character(i),'TAC_use'] <- sum(catch$caton[catch$Year == i])*1e-3
}

xrange <- c(2000,endYear)
yrange <- range(pretty(c(0,max(outArray,na.rm=T))))

par(oma=paroma,yaxs="i")
plot(0,0,col="white",xlim=xrange,ylim=yrange,xlab="Years",ylab="",
     cex.lab=cl,cex.axis=ca,font.lab=fonts,font=fonts,las=1)
mtext(side=2,line=mtextline,text="Catch / TAC (tonnes)",cex=cl,font=fonts)
rect((2000:(endYear))-0.5,0,(2000:(endYear))+0.5,outArray[,'TAC_use'],col="grey",lwd=2)
lines(x=rep(2000:(endYear),each=2)+c(-0.5,0.5),y=rep(outArray[,'TAC'],each=2),lwd=4)
legend("bottom",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",bg="white",pt.cex=c(2),box.lty=0)
title(main="Catch and TAC")

savePlot(paste0(outpath,"historic_Catch_TAC"),type=figtype)



#########################################################################################


