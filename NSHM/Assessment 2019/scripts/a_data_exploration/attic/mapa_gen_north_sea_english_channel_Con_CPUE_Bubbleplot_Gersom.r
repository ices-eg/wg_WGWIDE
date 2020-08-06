

rm(list=ls())

setwd("C:/IMARES/FAMA/ICES/WGWIDE/August_2017/NSHM/data/mapa_ICES_para_R")

resultpath="C:/IMARES/FAMA/ICES/WGWIDE/August_2017/NSHM/output/survey_data_exploration/"


#################################################################
## 3- Mapa esfuerzo: representando esfuerzo modalidad de pesca
#################################################################
# 5 colores de gama rojo-amarillo; batimetrías de 200, 500 y 1000 m; ejes izquierdo-superior con etiqueta de 1ºx 1º latitud-longitud; ejes derecho-inferior con etiquetas correspondientes a los códigos de rectángulo ICES; leyenda sin caja; evitar demasiados nombres que congestionen el mapa

library(mapdata)
library(maptools)
library(RODBC)
library(maps)
library(rgdal)
# Carga shapefiles... cambia el directorio a donde saques los argchivos que te envió en el shapes.rar
if (!exists("bath100")) {bath100<-readOGR("100m.dbf","100m")}
if (!exists("bathy.geb")) {bathy.geb<-readOGR("bathy_geb.dbf","bathy_geb")}
if (!exists("ices.div")) {ices.div<-readOGR("ices_div.dbf","ices_div")}
if (!exists("ices_squares_simple")) {ices_squares_simple<-readOGR("ices_squares_simple.dbf","ices_squares_simple")} #03/11/2008
if (!exists("bathy.geb2")) {bathy.geb2<-readOGR("bathy_geb2.dbf","bathy_geb2")}
if (!exists("ICES_Areas_20051103")) {ICES_Areas_20051103<-readOGR("ICES_Areas_20051103.dbf","ICES_Areas_20051103")}
win.metafile(filename = "mapa12_North_sea_English_channel_CPUE_2015.emf",9.5,15,pointsize=15)
# carga el mapa general con type="n" es decir no lo pintes pero coge los márgenes
map(database = "worldHires", xlim = c(-6,12), ylim = c(48,62),type="n")
                                         
##PARA AÑADIR COLOR A LAS CUADRICULAS:

#creamos un data.frame con 2 columnas la ID (que esta en el objeto de ices square simple) y el nombre del rectangulo
names(ices_squares_simple)
kk<-as.data.frame(cbind(ID=ices_squares_simple$ID,ICESNAME=as.character(ices_squares_simple$ICESNAME)))

#numero de mareas por rectangulo se ve en archivo txt kk3 de ejemplo. tiene que ser mismo formato.se hace por txt por problemas cuadricula vs.Excel
#kkk<-read.table("kk3.txt", header = TRUE, sep = ";", dec=".",fill = TRUE)

plot(ices_squares_simple, col=cores[classe],add=T)


#! dibuja batimetría  #las capas de batimetrias son 100  200  500  800 1000 1500 2000  asi que se elign al gusto
#plot(bathy.geb2[ bathy.geb2[["DEPTH"]]==200, ],add=T,col=gray(.65),lwd=.1)
#plot(bathy.geb2[ bathy.geb2[["DEPTH"]]==500, ],add=T,col=gray(.75),lwd=.1)
#plot(bathy.geb2[ bathy.geb2[["DEPTH"]]==1000, ],add=T,col=gray(.80),lwd=.1)
#http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html
if (!exists("bathy.dat")) {bathy.dat<-read.table('bat_eur-2000.txt',sep=",")}
names(bathy.dat)<-c('lon','lat','depth')
bathy.dat$depth[bathy.dat$depth>0]<-NA    #Avoid points above water
head(bathy.dat)
bathy.mat<-matrix(bathy.dat$depth,nrow=length(unique(bathy.dat$lon)),ncol=length(unique(bathy.dat$lat)))[,order(unique(bathy.dat$lat))]
land.mask<-(bathy.mat*0)+1
range(land.mask,na.rm=T)


contour(unique(bathy.dat$lon),sort(unique(bathy.dat$lat)),bathy.mat,levels=-c(200),labcex=0.4,col=gray(.65),lwd=.1,add=T)
contour(unique(bathy.dat$lon),sort(unique(bathy.dat$lat)),bathy.mat,levels=-c(500),labcex=0.4,col=gray(.75),lwd=.1,add=T)
contour(unique(bathy.dat$lon),sort(unique(bathy.dat$lat)),bathy.mat,levels=-c(1000),labcex=0.4,col=gray(.80),lwd=.1,add=T)

# el grid
grid(col=gray(.8),lwd=.5)

# areas ices
# png(filename=paste(resultpath,"prueba_ices_map.png",sep=""), heigh=2000, width=3500, units="px", pointsize=9, res=450)
# 
plot(ices_squares_simple,add=T,col=NA,border=gray(.9),lwd=.5)  # 03/11/2010

#ZEE
plot(ICES_Areas_20051103[1,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[4,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[9,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[10,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[13,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[19,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[23,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[38,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[41,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[63,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[44,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[48,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[53,],add=T,col=NA,border="navy",lty=33,lwd=2)
plot(ICES_Areas_20051103[62,],add=T,col=NA,border="navy",lty=33,lwd=2)


# dev.off()


############################################################################################

# Bubble plot CPUE

library(plotly)
# 
data <- read.csv("bubbles_CPUE_juveniles.csv",head=T)
data=subset(data,Year==2015)

radius <- sqrt(data$CPUE/pi)/10
symbols(data$ShootLon, data$ShootLat, circles=radius, inches=0.35, fg="white", bg="red")


############################################################################################

# areas ices
#plot(ices_squares_simple,add=T,col=NA,border=gray(.9),lwd=.5)  # 03/11/2008 17:56:04
plot(ices.div,add=T,col=NA,border=gray(.7),lwd=2)
plot(ices.div,add=T,col=NA,border=gray(.7),lwd=2)
names (ICES_Areas_20051103@data)
 
# coge los nombres de las áreas ices que vas a poner cartelitos
icesdivs<-c(12,13,21,29,30,31,34,7,11,6,5,1,3,9,14)
# pon los nombres de las áreas ices en sus sitios
text(getSpPPolygonsLabptSlots(ices.div)[icesdivs,],col=gray(.2),
as.character(ices.div@data$ICESCODE)[icesdivs],cex=.65,font=2,pos=2)
# algunas areas ices cuyos nombres salen bajo los mapas de países (VIIIc y IXa).
text(-5.5,44.2,col=gray(.2),as.character(ices.div@data$ICESCODE)[33],cex=.65,font=2,pos=2)
text(-.5,45,col=gray(.2),as.character(ices.div@data$ICESCODE)[32],cex=.65,font=2,pos=2)
text(-32,43,col=gray(.2),as.character(ices.div@data$ICESCODE)[28],cex=.65,font=2,pos=2)
text(-8.9,41,col=gray(.2),as.character(ices.div@data$ICESCODE)[35],cex=.65,font=2,pos=2)
text(-6,49,col=gray(.2),as.character(ices.div@data$ICESCODE)[27],cex=.65,font=2,pos=2)#7h
text(-4.5,51.5,col=gray(.2),as.character(ices.div@data$ICESCODE)[23],cex=.65,font=2,pos=2)#7g
text(-9.5,54,col=gray(.2),as.character(ices.div@data$ICESCODE)[19],cex=.65,font=2,pos=2)#7b
text(-9.5,51.5,col=gray(.2),as.character(ices.div@data$ICESCODE)[22],cex=.65,font=2,pos=2)#7j
text(-23,63,col=gray(.2),as.character(ices.div@data$ICESCODE)[6],cex=.65,font=2,pos=2)#5a
text(-14.5,54,col=gray(.2),as.character(ices.div@data$ICESCODE)[18],cex=.65,font=2,pos=2)#7c
text(-31,54,col=gray(.2),"XII",cex=.65,font=2,pos=2)
# carga el mapa de verdad, si lo cargas al principio se lía con todas las isóbatas y demás
map(database = "worldHires",  xlim = c(-6,12), ylim = c(48,62),fill=T,col="khaki",add=T)

#añadimos leyenda
#legend(-6, 41.5,  leglabs(lims, "<", ">"),   bty='n', fill=cores,cex=1.1, title="esfuerzo  LTL11 \n (días de pesca)")    #añadimos leyenda y ponemos signos < > a coveniencia# poner titulo de leyenda si es necesario

#eliminamos basura
rm(kk)
rm(kkk)
rm(pp)
rm(cores)
rm(classe)
rm(lims)

# ejes
axis(2,las=1,at=seq(35.,75.,by=1),cex.axis=.7,tck=-.01,font=2,mgp=c(2,.5,0))  #izq
axis(1,at=seq(-42.5,9.5,by=1),labels= c("A7","A8","A9","B0","B1","B2","B3","B4","B5","B6","B7","B8","B9","C0","C1","C2","C3","C4","C5","C6","C7","C8","C9","D0","D1","D2","D3","D4","D5","D6","D7","D8","D9","E0","E1","E2","E3","E4","E5","E6","E7","E8","E9","F0","F1","F2","F3","F4","F5","F6","F7","F8","F9"),cex.axis=.6,tck=-.00,mgp=c(2,.5,0))  #abajo
axis(1,at=seq(-45,11,by=1),labels=F,cex.axis=.7,tck=-.01,mgp=c(2,.5,0))  #abajo
axis(3,at=seq(-45,11,by=1),labels=seq(-45,11,by=1),cex.axis=.7,tck=-.01,font=2,mgp=c(2,.5,0))   #up
axis(4,las=1,at=seq(35.75,72.75,by=1),labels= c( "00","02","04","06","08","10","12","14","16","18","20","22","24","26","28","30","32","34","36","38","40","42","44","46","48","50","52","54","56","58" ,"60","62","64","66","68","70","72","74"),cex.axis=.6,tck=-.00,mgp=c(2,.5,0))   #derecha
axis(4,las=1,at=seq(35.5,75,by=0.5),labels=F, cex.axis=.7,tck=-.01,mgp=c(2,.5,0))   #derecha
box()
mtext("Latitud",side=2,line=1.5,cex=.8)
mtext("Longitud",side=3,line=1.5,cex=.8)
mtext("Cuadrícula ICES",side=1,line=1.5,cex=.8)
mtext("Cuadrícula ICES",side=4,line=1.05,cex=.8)


# cartelitos de lugares ya ubicados
text(-19,59.2,"Banco\nHatton",cex=.63,font=4)
#text(-14,59,"George Bligh\nbank",cex=.63,font=4)
text(-8.5,36.5,"Golfo de \nCádiz",cex=.63,font=4)
text(-8.7,58.9,"Hébridas \nOccidentales",cex=.63,font=2,srt=30)
text(-13.5,56.25,"Banco Rockall ",cex=.63,font=2,srt=60)
#text(-11.2,57.05,"Anton Dorhn\nSeamount",cex=.7,font=4)
text(-13.8,53.2,"Banco \nPorcupine",cex=.63,font=4)
text(-7.5,53,"Irlanda",cex=.7,font=2,srt=50)
text(-1.5,52.5,"Reino\nUnido",cex=.7,font=2)
text(-9.2,50.5," Mar \nCéltico",cex=.63,font=4,srt=-1)
text(-10.4,49.35,"Gran\nSol",cex=.63,font=2,srt=20)
text(-4,40,"España",cex=.7,font=2)
text(3,46,"Francia",cex=.7,font=2)
text(-7.9,40,"Portugal",cex=.7,font=2,srt=270)
text(-5.5,45.1,"Golfo de \nVizcaya",srt=-25,font=2,cex=.63)
text(-30,37,"Islas \nAzores",cex=.66,font=2)
text(-18,65,"Islandia",cex=.65,font=2)
text(-36,70,"Groelandia",cex=.7,font=2,srt=20)
text(-4,62.75,"I. Faroe",cex=.66,font=2)
dev.off()


