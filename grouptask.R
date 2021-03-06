library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(plyr)
library(viridis)
library(tidyr)
library(naniar)
library(stringr)
library(ComputationalMovementAnalysisData)

#____________________________________________________________________________________
#load data
#install.packages("devtools") 
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")

wildschwein_BE<- wildschwein_BE
#%>%st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

setwd("C:/Users/simon/Desktop/Simon/Simon/MSc/Pattern and Trends/semester_project/grouptask")
Feldaufnahmen <- read_sf("Feldaufnahmen_Fanel.gpkg")%>%
  st_as_sf(coords = geom, crs = 2056, remove = FALSE)



#preprocessing
ggplot() +
  geom_sf(data=Feldaufnahmen, aes(fill = Frucht))+
  geom_point(data = wildschwein_BE, aes(E, N, color = TierName))


#Filtern nach 15 min-Messungen
wildschwein_BE$DatetimeUTC<-as.POSIXct(as.character(wildschwein_BE$DatetimeUTC), format = "%Y-%m-%d %H:%M:%OS",tz = "UTC")
wildschwein_BE$Minute <- minute(wildschwein_BE$DatetimeUTC)
wildschwein_BE <- wildschwein_BE%>%filter(Minute == c(00, 15, 30, 45))
wildschwein_BE <- wildschwein_BE[,-(9),drop=FALSE]




#_________________________________________________________________________________
#Trainingsample erstellen. Danach mit dem ganzen Datensatz rechnen
#trainingsample <- wildschwein_BE%>%filter(TierName %in% c("Ueli", "Caroline"), DatetimeUTC > ymd_hms("2015-04-01 00:00:00"), DatetimeUTC < ymd_hms("2016-06-01 00:00:00"))
#trainingsample <- wildschwein_BE%>%filter(DatetimeUTC > ymd_hms("2015-01-01 00:00:00"), DatetimeUTC < ymd_hms("2016-01-01 00:00:00"))
trainingsample <- wildschwein_BE

#Eucl.Dist moving window
trainingsample <- trainingsample %>%
  mutate(
    stepMean = rowMeans(                       
      cbind(                                   
        sqrt((lag(E,6)-E)^2+(lag(E,6)-E)^2),         
        sqrt((lag(E,5)-E)^2+(lag(E,5)-E)^2),         
        sqrt((lag(E,4)-E)^2+(lag(E,4)-E)^2),         
        sqrt((lag(E,3)-E)^2+(lag(E,3)-E)^2),   
        sqrt((lag(E,2)-E)^2+(lag(E,2)-E)^2),   
        sqrt((lag(E,1)-E)^2+(lag(E,1)-E)^2),   
        sqrt((E-lead(E,1))^2+(E-lead(E,1))^2),  
        sqrt((E-lead(E,2))^2+(E-lead(E,2))^2),
        sqrt((E-lead(E,3))^2+(E-lead(E,3))^2),  
        sqrt((E-lead(E,4))^2+(E-lead(E,4))^2),  
        sqrt((E-lead(E,5))^2+(E-lead(E,5))^2),  
        sqrt((E-lead(E,6))^2+(E-lead(E,6))^2)  
      )                                        
    )
  )


# Remove moving points
trainingsample_stops <- trainingsample %>% 
  ungroup() %>%
  mutate(static = stepMean < 8.69)

#Segment-based analysis
#defining ID's for trajectories
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

trainingsample_stops <- trainingsample_stops %>%
  mutate(segment_id = rle_id(static))
head(trainingsample_stops)

#länge der trajectn messen
trainingsample_stops$Dauer <- 1
trainingsample_stops_dauer<-aggregate(trainingsample_stops[, c(12)], list(trainingsample_stops$segment_id), sum)
names(trainingsample_stops_dauer)[1] <- "segment_id"
trainingsample_stops<-left_join(trainingsample_stops, trainingsample_stops_dauer, by="segment_id")

#remove "moving" segments
trainingsample_filter <- trainingsample_stops %>%
  filter(static)

trainingsample_filter%>%
  ggplot()  +
  geom_point(aes(E, N, color = TierName))+
  coord_fixed()


#_________________________________________________
#define segementcenter 
trainingsample_center<- aggregate(trainingsample_filter[, c(5:6)], list(trainingsample_filter$segment_id), mean)

#join for TierID/TierName
#trainingsample_filter%>%st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)
#trainingsample_center%>%st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

names(trainingsample_center)[1] <- "segment_id"

trainingsample_center_join<-left_join(trainingsample_filter, trainingsample_center, by="segment_id")
#%>%st_as_sf(coords = c("E.x" "N.x"), crs = 2056, remove = FALSE)

head(trainingsample_center_join)
ggplot() +
  geom_sf(data = Feldaufnahmen, aes(fill = Frucht))+
  geom_point(data = trainingsample_center_join, aes(E.y, N.y, color = TierName, size = Dauer.y))


#_______________________________________________
#Feldaufnahmen joinen and clean dataframe
wildschwein <- trainingsample_center_join
head(wildschwein)
wildschwein <- wildschwein[,-(3),drop=FALSE]
head(wildschwein)
wildschwein <- wildschwein[,-(7),drop=FALSE]
head(wildschwein)
wildschwein <- wildschwein[,-(7:8),drop=FALSE]
head(wildschwein)
wildschwein <- wildschwein[,-(8),drop=FALSE]
head(wildschwein)




#Feldaufnahmen kategorisieren, NA's entfernen
Feldaufnahmen_korr<-Feldaufnahmen%>%
  mutate(Frucht = str_replace(Frucht, "Acker|Ackerbohnen|Baumschule|Blumenbeet|Bohnen|Brokkoli|Erbsen|Fenchel|Flachs|Flugplatz|Gemuese|Gewaechshaus|Karotten|Kartoffeln|Kohl|Kohlrabi|Kuerbis|Lauch|Lupinen|Mangold|Obstplantage|Rhabarber|Rueben|Salat|Sellerie|Spargel|Spinat|Zucchetti|Zwiebeln", "Niedere Kulturen"))%>%
  mutate(Frucht = str_replace(Frucht, "NiedereKulturenbohnen|Niedere Kulturenrabi|Niedere Kulturenbohnen", "Niedere Kulturen"))%>%
  mutate(Frucht = str_replace(Frucht, "Gerste|Hafer|Roggen|Weizen", "Getreide"))%>%
  mutate(Frucht = str_replace(Frucht, "Wiese|Weide", "Wiese/Weide"))%>%
  mutate(Frucht = str_replace(Frucht, "Buntbrache|Brache", "zus.(Bunt)-Brache"))%>%
  mutate(Frucht = str_replace(Frucht, "Raps", "zus. Raps"))


wildschwein<-wildschwein%>%st_as_sf(coords = c("E.y", "N.y"), crs = 2056, remove = FALSE)

wildschwein <-st_join(wildschwein,Feldaufnahmen_korr, suffix = c("E.y", "N.y"))
wildschwein <-wildschwein%>% drop_na(Frucht)


ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Frucht))+
  geom_point(data = wildschwein, aes(E.y, N.y, color = TierName, size = Dauer.y))

ggplot() +
  geom_sf(data=Feldaufnahmen, aes())+
  geom_point(data = wildschwein, aes(E.y, N.y, color = Frucht, size = Dauer.y))

#Aufteilen nach Jahreszeit
wildschwein$DatetimeUTC<-as.POSIXct(as.character(wildschwein$DatetimeUTC), format = "%Y-%m-%d %H:%M:%OS",tz = "UTC")
wildschwein$Monat <- month(wildschwein$DatetimeUTC)
wildschwein$Jahreszeit[wildschwein$Monat == "3"] <- "Fruehling"
wildschwein$Jahreszeit[wildschwein$Monat == "4"] <- "Fruehling"
wildschwein$Jahreszeit[wildschwein$Monat == "5"] <- "Fruehling"
wildschwein$Jahreszeit[wildschwein$Monat == "6"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "7"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "8"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "9"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "10"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "11"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "12"] <- "Winter"
wildschwein$Jahreszeit[wildschwein$Monat == "1"] <- "Winter"
wildschwein$Jahreszeit[wildschwein$Monat == "2"] <- "Winter"
head(wildschwein)

#Anteil an Fl?chen
wildschwein$Anteil <- 1
wildschwein_anteil<- aggregate(wildschwein[, c(16)], list(wildschwein$Frucht), sum)
wildschwein_anteil_jahreszeit<- aggregate(wildschwein[, c(16)], list(wildschwein$Frucht, wildschwein$Jahreszeit), sum)

wildschwein_anteil_fruehling<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Fruehling")
wildschwein_anteil_sommer<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Sommer")
wildschwein_anteil_herbst<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Herbst")
wildschwein_anteil_winter<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Winter")

barplot(Anteil~Group.1, data = wildschwein_anteil)



#_____________________________________________________________________________
#Resultate Plots

names(wildschwein)[8] <- "Anzahl_Messungen"
names(wildschwein)[13] <- "Habitattyp"


#Frühling, Sommer, Herbst und Winter in richtiger Rheienfolge
neworder <- c("Fruehling","Sommer","Herbst", "Winter")
library(plyr)  ## or dplyr (transform -> mutate)
wildschwein <- arrange(transform(wildschwein,
                                 Jahreszeit=factor(Jahreszeit,levels=neworder)),Jahreszeit)

ggplot() +
  geom_bar(data=wildschwein, aes(sum(Anteil),fill = Habitattyp), position = "fill")+
  facet_grid(~Jahreszeit)+
  labs(x = "Jahreszeiten", y = "Aufteilung der Ruheplaetze in die verschiedenen Habitattypen", title = "Ruheplaetze im Untersuchungsgebiet")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Frucht))+
  geom_point(data = wildschwein, aes(E.y, N.y, size = Anzahl_Messungen))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y = "", title = "Ruheplaetze im Untersuchungsgebiet nach Habitattyp", subtitle = "")

ggplot() +
  geom_sf(data=Feldaufnahmen, aes())+
  geom_point(data = wildschwein, aes(E.y, N.y, color = Habitattyp, size = Anzahl_Messungen))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y = "", title = "Ruheplaetze im Untersuchungsgebiet nach Habitattyp", subtitle = "")


#Kuchendiagramme und Prozentzahlen total und aufgeteilt nach Jahreszeit
pct <- round(wildschwein_anteil$Anteil/sum(wildschwein_anteil$Anteil)*100)
lbls <- paste(wildschwein_anteil$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp")

pct <- round(wildschwein_anteil_fruehling$Anteil/sum(wildschwein_anteil_fruehling$Anteil)*100)
lbls <- paste(wildschwein_anteil_fruehling$Group.1, pct)
lbls <- paste(lbls,"%",sep="")
pie(wildschwein_anteil_fruehling$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Fruehling")

pct <- round(wildschwein_anteil_sommer$Anteil/sum(wildschwein_anteil_sommer$Anteil)*100)
lbls <- paste(wildschwein_anteil_sommer$Group.1, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(wildschwein_anteil_sommer$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Sommer")

pct <- round(wildschwein_anteil_herbst$Anteil/sum(wildschwein_anteil_herbst$Anteil)*100)
lbls <- paste(wildschwein_anteil_herbst$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil_herbst$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Herbst")

pct <- round(wildschwein_anteil_winter$Anteil/sum(wildschwein_anteil_winter$Anteil)*100)
lbls <- paste(wildschwein_anteil_winter$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil_winter$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Winter")


#Durchschnittliche Dauer am Ruheort
wildschwein_dauer<- aggregate(wildschwein[, c(8)], list(wildschwein$Habitattyp), mean)



#___________________________________________________________________________________________________________________
#Vergleich mit dem von den Wildschweinen genutztem Habitat
#Feldaufnahmen kategorisieren, NA's entfernen
wildschwein_BE<- wildschwein_BE%>%
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

wildschwein_all <-st_join(wildschwein_BE,Feldaufnahmen_korr, suffix = c("E", "N"))
wildschwein_all <-wildschwein_all%>% drop_na(Frucht)

names(wildschwein_all)[11] <- "Habitattyp"


ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Habitattyp))+
  geom_point(data = wildschwein_all, aes(E, N))

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes())+
  geom_point(data = wildschwein_all, aes(E, N, color = Habitattyp))

#Anteil an Fl?chen
#Aufteilen nach Jahreszeit
wildschwein_all$DatetimeUTC<-as.POSIXct(as.character(wildschwein_all$DatetimeUTC), format = "%Y-%m-%d %H:%M:%OS",tz = "UTC")
wildschwein_all$Monat <- month(wildschwein_all$DatetimeUTC)
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "3"] <- "Fruehling"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "4"] <- "Fruehling"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "5"] <- "Fruehling"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "6"] <- "Sommer"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "7"] <- "Sommer"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "8"] <- "Sommer"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "9"] <- "Herbst"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "10"] <- "Herbst"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "11"] <- "Herbst"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "12"] <- "Winter"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "1"] <- "Winter"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "2"] <- "Winter"

wildschwein_all$Anteil <- 1
wildschwein_all_anteil<- aggregate(wildschwein_all[, c(14)], list(wildschwein_all$Habitattyp), sum)
wildschwein_all_anteil_jahreszeit<- aggregate(wildschwein_all[, c(14)], list(wildschwein_all$Habitattyp, wildschwein_all$Jahreszeit), sum)

wildschwein_all_anteil_fruehling<-wildschwein_all_anteil_jahreszeit%>%filter(Group.2 == "Fruehling")
wildschwein_all_anteil_sommer<-wildschwein_all_anteil_jahreszeit%>%filter(Group.2 == "Sommer")
wildschwein_all_anteil_herbst<-wildschwein_all_anteil_jahreszeit%>%filter(Group.2 == "Herbst")
wildschwein_all_anteil_winter<-wildschwein_all_anteil_jahreszeit%>%filter(Group.2 == "Winter")

barplot(Anteil~Group.1, data = wildschwein_all_anteil)

#Frühling, Sommer, Herbst und Winter in richtiger Rheienfolge
neworder <- c("Fruehling","Sommer","Herbst", "Winter")
library(plyr)  ## or dplyr (transform -> mutate)
wildschwein_all <- arrange(transform(wildschwein_all,
                                 Jahreszeit=factor(Jahreszeit,levels=neworder)),Jahreszeit)


#Resultate Plots
ggplot() +
  geom_bar(data=wildschwein_all, aes(sum(Anteil),fill = Habitattyp), position = "fill")+
  facet_grid(~Jahreszeit)+
  labs(x = "Jahreszeiten", y = "Aufteilung aller Lokationen in die verschiedenen Habitattypen", title = "Raumnutzung im Untersuchungsgebiet")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Habitattyp))+
  geom_point(data = wildschwein_all, aes(E, N))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y = "", title = "Raumnutzung Untersuchungsgebiet nach Habitattyp", subtitle = "")




#Kuchendiagramme und Prozentzahlen
pct <- round(wildschwein_all_anteil$x/sum(wildschwein_all_anteil$x)*100)
lbls <- paste(wildschwein_all_anteil$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_all_anteil$x,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der aller Lokationen nach Vegetationstyp")

pct <- round(wildschwein_all_anteil_fruehling$Anteil/sum(wildschwein_all_anteil_fruehling$Anteil)*100)
lbls <- paste(wildschwein_all_anteil_fruehling$Group.1, pct)
lbls <- paste(lbls,"%",sep="")
pie(wildschwein_all_anteil_fruehling$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Fruehling")

pct <- round(wildschwein_anteil_sommer$x/sum(wildschwein_anteil_sommer$x)*100)
lbls <- paste(wildschwein_anteil_sommer$Group.1, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(wildschwein_anteil_sommer$x,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Sommer")

pct <- round(wildschwein_all_anteil_herbst$x/sum(wildschwein_all_anteil_herbst$x)*100)
lbls <- paste(wildschwein_all_anteil_herbst$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_all_anteil_herbst$x,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Herbst")

pct <- round(wildschwein_all_anteil_winter$x/sum(wildschwein_all_anteil_winter$x)*100)
lbls <- paste(wildschwein_all_anteil_winter$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_all_anteil_winter$x,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Winter")


#Vergleich Ruhepl?tze zu gesamter Raumnutzung 
wildschwein_anteil$Data<-"Ruheplatz"
wildschwein_all_anteil$Data<-"Alle"
wildschwein_anteil_rbind<-rbind(wildschwein_anteil, wildschwein_all_anteil)

ggplot() +
  geom_bar(data=wildschwein_all, aes(sum(Anteil),fill = Frucht), position = "fill")+
  facet_grid(~Jahreszeit)+
  labs(x = "Jahreszeiten", y = "Aufteilung aller Lokationen in die verschiedenen Habitattypen", title = "Raumnutzung im Untersuchungsgebiet")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#___________________________________________________________________________________________________
#Statistische Tests

#Präferenz von Schilf als Ruheplatz:
#Anteil Ruhezeit im Feuchtgebiet vs. Anteil Aufenthaltszeit total im Feuchtgebiet

Ruheplatz <- c(76, 39, 82, 91)
All <- c(48, 39, 53, 70)
Anteil_Schilf<-data.frame(Ruheplatz,All)

boxplot(Anteil_Schilf$Ruheplatz, Anteil_Schilf$All)
t.test(Anteil_Schilf$Ruheplatz, Anteil_Schilf$All)
# --> nicht signifikant

Ruheplatz <- c(15,24,6,9)
All <- c(29,24,23,21)
Anteil_Wald<-data.frame(Ruheplatz,All)

boxplot(Anteil_Wald$Ruheplatz, Anteil_Schilf$All)
t.test(Anteil_Wald$Ruheplatz, Anteil_Schilf$All)
# --> signifikant

Ruheplatz <- c(9,38,14,0)
All <- c(23, 37,24,9)
Anteil_landw<-data.frame(Ruheplatz,All)

boxplot(Anteil_landw$Ruheplatz, Anteil_Schilf$All)
t.test(Anteil_landw$Ruheplatz, Anteil_Schilf$All)
# --> signifikant



#Ruhephase untersuchen zwischen den verschiedenen Habitaten

Schilf<- wildschwein%>%filter(Habitattyp == "Feuchtgebiet")
Schilf<-Schilf[, 8:9]
Schilf<-Schilf[!duplicated(Schilf), ]
Schilf<-Schilf[, 1]
mean(Schilf)
Schilf<-Schilf[1:1208]

Chinaschilf<- wildschwein%>%filter(Habitattyp == "Chinaschilf")
Chinaschilf<-Chinaschilf[, 8:9]
Chinaschilf<-Chinaschilf[!duplicated(Chinaschilf), ]
Chinaschilf<-Chinaschilf[, 1]
mean(Chinaschilf)

Raps<- wildschwein%>%filter(Habitattyp == "zus. Raps")
Raps<-Raps[, 8:9]
Raps<-Raps[, 1]
mean(Raps)

Getreide<- wildschwein%>%filter(Habitattyp == "Getreide")
Getreide<-Getreide[, 8:9]
Getreide<-Getreide[, 1]
mean(Getreide)

Wald<- wildschwein%>%filter(Habitattyp == "Wald")
Wald<-Wald[, 8:9]
Wald<-Wald[!duplicated(Wald), ]
Wald<-Wald[, 1]
mean(Wald)

Mais<- wildschwein%>%filter(Habitattyp == "Mais")
Mais<-Mais[, 8:9]
Mais<-Mais[!duplicated(Mais), ]
Mais<-Mais[, 1]
mean(Mais)

Niedere_Kulturen<- wildschwein%>%filter(Habitattyp == "Niedere Kulturen")
Niedere_Kulturen<-Niedere_Kulturen[, 8:9]
Niedere_Kulturen<-Niedere_Kulturen[!duplicated(Wald), ]
Niedere_Kulturen<-Niedere_Kulturen[, 1]
mean(Niedere_Kulturen)

Sonnenblumen<- wildschwein%>%filter(Habitattyp == "Sonnenblumen")
Sonnenblumen<-Sonnenblumen[, 8:9]
Sonnenblumen<-Sonnenblumen[!duplicated(Sonnenblumen), ]
Sonnenblumen<-Sonnenblumen[, 1]
mean(Sonnenblumen)

Wiese_Weide<- wildschwein%>%filter(Habitattyp == "Wiese/Weide")
Wiese_Weide<-Wiese_Weide[, 8:9]
Wiese_Weide<-Wiese_Weide[!duplicated(Wiese_Weide), ]
Wiese_Weide<-Wiese_Weide[, 1]
mean(Wiese_Weide)

#Vergleich Schilf, Wald
Dauer<-data.frame(Schilf, Wald)

boxplot(Dauer$Schilf, Dauer$Wald)
t.test(Dauer$Schilf, Dauer$Wald)
# --> signifikant




