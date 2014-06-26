load("E:/workingdirectory/gis/haiti/haiti0.RData")
map0 <- gadm

load("E:/workingdirectory/gis/haiti/haiti1.RData")
map1 <- gadm

load("E:/workingdirectory/gis/haiti/haiti2.RData")
map2 <- gadm

load("E:/workingdirectory/gis/haiti/haiti3.RData")
map3 <- gadm

map3$color <- adjustcolor("black", alpha.f=0.2)
map3$color[which(grepl("Carrefour", map3$NAME_3)==TRUE)] <- adjustcolor("darkred", alpha.f=0.75)
map3$color[which(grepl("Thomonde", map3$NAME_3)==TRUE)] <- adjustcolor("darkgreen", alpha.f=0.75)

plot(map3, border=adjustcolor("black", alpha.f=0.5),  col=map3$color)

labelpos <- data.frame(do.call(rbind, lapply(map3@polygons, function(x) x@labpt)))
names(labelpos) <- c("x","y")                        
map3@data <- data.frame(map3@data, labelpos)
map3$labelpos <- labelpos
map3$labelposx <- labelpos$x
map3$labelposy <- labelpos$y
zippy3 <- unique(sort(map3$NAME_3))
zippy3 <- as.character(zippy3)
map3$text3 <- 1
for (i in zippy3){map3$text3[which(map3$NAME_3 == i)] <- i }

text(labelpos$x, labelpos$y, label=map3$text3, cex=0.1, col=adjustcolor("black", alpha.f=0.4))

library(maptools)
library(maps)
library(RColorBrewer)
library(classInt)
mapDen <- readShapePoly("E:/workingdirectory/gis/haiti/popDensity/Haiti_ADM3_stats.shp")
plot(mapDen)
summary(mapDen)



plotvar<-(mapDen$POP_DENS)^(1/4.5)
nclr<- 8 # number of bins (3-8)
min<- floor(min(plotvar))
max<- ceiling(max(plotvar))
breaks<- (max-min) / nclr
plotclr<- brewer.pal(nclr, "Reds")
class<- classIntervals(plotvar, nclr, style ="fixed", fixedBreaks=seq(min, max, breaks))

colcode<- findColours(class, plotclr)
colcode2<-gsub(",","-", gsub("[[]|[)]|[]]","", names(attr(colcode, "table"))))
colcode3 <- round(as.numeric(unlist(strsplit(colcode2, "-")))^4.5, digits=0)
colcode4 <- c()
for (i in seq(1, length(colcode3),2)){
  colcode4[i] <- paste0(colcode3[i], "-", colcode3[i+1])
}
colcode5 <- colcode4[seq(1, length(colcode4),2)]

### Plot the Map
plot(mapDen, border=FALSE, fill=TRUE, col=colcode)# ONCE YOUVE DEFINED PLOT VAR
###Add a Legend
legend("topleft",
       legend = colcode5, 
       title= "Residents per square kilometer",
       fill=attr(colcode, "palette"),
       cex= 0.56, bty="n", border=FALSE) 

#plot(map0, border=adjustcolor("black", alpha.f=0.3), add=TRUE)
#plot(map1, border=adjustcolor("black", alpha.f=0.3), add=TRUE)
#plot(map2, border=adjustcolor("black", alpha.f=0.3), add=TRUE)
#plot(map3, border=adjustcolor("black", alpha.f=0.3), add=TRUE)

labelpos2 <- data.frame(do.call(rbind, lapply(mapDen@polygons, function(x) x@labpt)))
names(labelpos2) <- c("x","y")                        
mapDen@data <- data.frame(mapDen@data, labelpos2)
mapDen$labelpos2 <- labelpos2
mapDen$labelpos2x <- labelpos2$x
mapDen$labelpos2y <- labelpos2$y
zippy <- unique(sort(mapDen$POP_DENS))
zippy <- as.character(zippy)
mapDen$text <- 1
for (i in zippy){mapDen$text[which(mapDen$POP_DENS == i)] <- i }

text(labelpos2$x, labelpos2$y, label=round(as.numeric(mapDen$text), digits=1), 
     cex=0.1, col=adjustcolor("black", alpha.f=0.4))

zippy2 <- unique(sort(mapDen$COMMUNE))
zippy2 <- as.character(zippy2)

mapDen$COMMUNE <- as.character(mapDen$COMMUNE)
mapDen$text2 <- 1
for (i in zippy2){mapDen$text2[which(mapDen$COMMUNE == i)] <- i }

text(labelpos2$x, labelpos2$y, label=mapDen$text2, , 
     cex=0.5, col=adjustcolor("black", alpha.f=0.4))


##################COUNTOUR MAP
plot(mapDen)
filled.contour(x = rev(sort(labelpos2$x)),
               y = sort(labelpos2$y),
               z=mapDen[,2:3])


########UN POP
unPop <- read.csv("E:/workingdirectory/haiti/meeting2014-04-11/unPopulation2012.csv", skip=16)
unPop <- unPop[which(unPop$Major.area..region..country.or.area.. == "Haiti" &
                       unPop$Reference.date..as.of.1.July. == 2010),]
unPop <- unPop[colnames(unPop[which(grepl("X",colnames(unPop)))])]
row.names(unPop) <-NULL
unPop <- as.data.frame(t(unPop))
colnames(unPop) <- "number"
unPop$age <- row.names(unPop)
row.names(unPop) <- NULL
unPop <- unPop[which(unPop$age != "X80."),]
mybp <- barplot(as.numeric(gsub(" ", "", unPop$number)), 
                names.arg=gsub("[.]", "-", gsub("X", "", unPop$age)),
                cex.names=0.6,
                las=3,
                xlab="Population (millions)", ylim=c(0, 1500))
text(x=mybp[,1], y=as.numeric(gsub(" ", "", unPop$number))+50,
     label = paste0(round(as.numeric(gsub(" ", "", unPop$number)) / 
       sum(as.numeric(gsub(" ", "", unPop$number)))*100, digits=2), "%"),
     cex=0.55, col=adjustcolor("black", alpha.f=0.75))

unPop$age2 <- as.numeric(gsub(" ", "", unPop$number))
save.image("E:/workingdirectory/haiti/meeting2014-04-11/mapAndDensity.RData")
