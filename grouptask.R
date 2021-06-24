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

wildschwein_BE<- wildschwein_BE%>%
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)
head(wildschwein_BE)

Feldaufnahmen <- read_sf("Feldaufnahmen_Fanel.gpkg")%>%
  st_as_sf(coords = geom, crs = 2056, remove = FALSE)

#preprocessing
ggplot() +
  geom_sf(data=Feldaufnahmen, aes(fill = Frucht))+
  geom_point(data = wildschwein_BE, aes(E, N, color = TierName))

#_________________________________________________________________________________
#Trainingsample erstellen. Danach mit dem ganzen Datensatz rechnen
trainingsample <- wildschwein_BE%>%filter(TierName %in% c("Ueli", "Caroline"), DatetimeUTC > ymd_hms("2016-04-01 00:00:00"), DatetimeUTC < ymd_hms("2016-06-01 00:00:00"))
#trainingsample <- wildschwein_BE

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
trainingsample_stops_dauer<-aggregate(trainingsample_stops[, c(13)], list(trainingsample_stops$segment_id), sum)
names(trainingsample_stops_dauer)[1] <- "segment_id"
trainingsample_stops<-st_join(trainingsample_stops, trainingsample_stops_dauer, by=segment_id)

  
#remove "moving" segments
trainingsample_filter <- trainingsample_stops %>%
  filter(static)

trainingsample_filter%>%
  ggplot()  +
  geom_point(aes(E, N, color = TierName))


#_________________________________________________
#define segementcenter 
trainingsample_center<- aggregate(trainingsample_filter[, c(2,5:6,15)], list(trainingsample_filter$segment_id.x), mean)

ggplot() +
  geom_point(data = trainingsample_center, aes(E, N, size = Dauer.y))

#join for TierID/TierName
trainingsample_center_join<-st_join(trainingsample_filter, trainingsample_center, by=segment_id)%>%
  st_as_sf(coords = c("E.y", "N.y"), crs = 2056, remove = FALSE)

ggplot() +
  geom_sf(data=Feldaufnahmen, aes(fill = Frucht))+
  geom_point(data = trainingsample_center_join, aes(E.y, N.y, color = TierName.x))


#_______________________________________________
#Feldaufnahmen joinen and clean dataframe
wildschwein <- trainingsample_center_join[,-(8:11),drop=FALSE]
wildschwein <- wildschwein[,-(9:10),drop=FALSE]
wildschwein <- wildschwein[,-(5:6),drop=FALSE]

#Feldaufnahmen kategorisieren, NA's entfernen
Feldaufnahmen_korr<-Feldaufnahmen%>%
  mutate(Frucht = str_replace(Frucht, "Acker|Ackerbohnen|Baumschule|Blumenbeet|Bohnen|Brokkoli|Erbsen|Fenchel|Flachs|Flugplatz|Gemuese|Gewaechshaus|Karotten|Kartoffeln|Kohl|Kohlrabi|Kuerbis|Lauch|Lupinen|Mangold|Obstplantage|Rhabarber|Rueben|Salat|Sellerie|Spargel|Spinat|Zucchetti|Zwiebeln", "Niedere Kulturen"))%>%
  mutate(Frucht = str_replace(Frucht, "NiedereKulturenbohnen|Niedere Kulturenrabi|Niedere Kulturenbohnen", "Niedere Kulturen"))%>%
  mutate(Frucht = str_replace(Frucht, "Gerste|Hafer|Roggen|Weizen", "Getreide"))%>%
  mutate(Frucht = str_replace(Frucht, "Wiese|Weide", "Wiese/Weide"))%>%
  mutate(Frucht = str_replace(Frucht, "Buntbrache|Brache", "zus.(Bunt)-Brache"))%>%
  mutate(Frucht = str_replace(Frucht, "Raps", "zus. Raps"))

wildschwein <-st_join(wildschwein,Feldaufnahmen_korr, suffix = c("E.y", "N.y"))
wildschwein <-wildschwein%>% drop_na(Frucht)

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Frucht))+
  geom_point(data = wildschwein, aes(E.y, N.y, color = TierName.x))

ggplot() +
  geom_sf(data=Feldaufnahmen, aes())+
  geom_point(data = wildschwein, aes(E.y, N.y, color = Frucht))

#Aufteilen nach Jahreszeit
wildschwein$DatetimeUTC<-as.POSIXct(as.character(wildschwein$DatetimeUTC), format = "%Y-%m-%d %H:%M:%OS",tz = "UTC")
wildschwein$Monat <- month(wildschwein$DatetimeUTC)
wildschwein$Jahreszeit[wildschwein$Monat == "3"] <- "Fr?hling"
wildschwein$Jahreszeit[wildschwein$Monat == "4"] <- "Fr?hling"
wildschwein$Jahreszeit[wildschwein$Monat == "5"] <- "Fr?hling"
wildschwein$Jahreszeit[wildschwein$Monat == "6"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "7"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "8"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "9"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "10"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "11"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "12"] <- "Winter"
wildschwein$Jahreszeit[wildschwein$Monat == "1"] <- "Winter"
wildschwein$Jahreszeit[wildschwein$Monat == "2"] <- "Winter"

#Anteil an Fl?chen
wildschwein$Anteil <- 1
wildschwein_anteil<- aggregate(wildschwein[, c(14)], list(wildschwein$Frucht), sum)
wildschwein_anteil_jahreszeit<- aggregate(wildschwein[, c(14)], list(wildschwein$Frucht, wildschwein$Jahreszeit), sum)

wildschwein_anteil_fruehling<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Fr?hling")
wildschwein_anteil_sommer<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Sommer")
wildschwein_anteil_herbst<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Herbst")
wildschwein_anteil_winter<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Winter")

barplot(Anteil~Group.1, data = wildschwein_anteil)

#_____________________________________________________________________________
#Resultate Plots
ggplot() +
  geom_bar(data=wildschwein, aes(sum(Anteil),fill = Frucht), position = "fill")+
  facet_grid(~Jahreszeit)+
  labs(x = "Jahreszeiten", y = "Aufteilung der Ruhepl?tze in die verschiedenen Habitattypen", title = "Ruhepl?tze im Untersuchungsgebiet")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Frucht))+
  geom_point(data = wildschwein, aes(E.y, N.y))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y = "", title = "Ruhepl?tze im Untersuchungsgebiet nach Habitattyp", subtitle = "")


#Kuchendiagramme und Prozentzahlen total und aufgeteilt nach Jahreszeit
pct <- round(wildschwein_anteil$Anteil/sum(wildschwein_anteil$Anteil)*100)
lbls <- paste(wildschwein_anteil$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruhepl?tze nach Vegetationstyp")

pct <- round(wildschwein_anteil_fruehling$Anteil/sum(wildschwein_anteil_fruehling$Anteil)*100)
lbls <- paste(wildschwein_anteil_fruehling$Group.1, pct)
lbls <- paste(lbls,"%",sep="")
pie(wildschwein_anteil_fruehling$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruhepl?tze nach Vegetationstyp - Fr?hling")

pct <- round(wildschwein_anteil_sommer$Anteil/sum(wildschwein_anteil_sommer$Anteil)*100)
lbls <- paste(wildschwein_anteil_sommer$Group.1, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(wildschwein_anteil_sommer$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruhepl?tze nach Vegetationstyp - Sommer")

pct <- round(wildschwein_anteil_herbst$Anteil/sum(wildschwein_anteil_herbst$Anteil)*100)
lbls <- paste(wildschwein_anteil_herbst$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil_herbst$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruhepl?tze nach Vegetationstyp - Herbst")

pct <- round(wildschwein_anteil_winter$Anteil/sum(wildschwein_anteil_winter$Anteil)*100)
lbls <- paste(wildschwein_anteil_winter$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil_winter$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruhepl?tze nach Vegetationstyp - Winter")

#___________________________________________________________________________________________________________________
#Vergleich mit dem von den Wildschweinen genutztem Habitat
#Feldaufnahmen kategorisieren, NA's entfernen
wildschwein_all <-st_join(wildschwein_BE,Feldaufnahmen_korr, suffix = c("E", "N"))
wildschwein_all <-wildschwein_all%>% drop_na(Frucht)

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Frucht))+
  geom_point(data = wildschwein_all, aes(E, N))

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes())+
  geom_point(data = wildschwein_all, aes(E, N, color = Frucht))

#Anteil an Fl?chen
#Aufteilen nach Jahreszeit
wildschwein_all$DatetimeUTC<-as.POSIXct(as.character(wildschwein_all$DatetimeUTC), format = "%Y-%m-%d %H:%M:%OS",tz = "UTC")
wildschwein_all$Monat <- month(wildschwein_all$DatetimeUTC)
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "3"] <- "Fr?hling"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "4"] <- "Fr?hling"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "5"] <- "Fr?hling"
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
wildschwein_all_anteil<- aggregate(wildschwein_all[, c(14)], list(wildschwein_all$Frucht), sum)
wildschwein_all_anteil_jahreszeit<- aggregate(wildschwein_all[, c(14)], list(wildschwein_all$Frucht, wildschwein_all$Jahreszeit), sum)

wildschwein_all_anteil_fruehling<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Fr?hling")
wildschwein_all_anteil_sommer<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Sommer")
wildschwein_all_anteil_herbst<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Herbst")
wildschwein_all_anteil_winter<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Winter")

barplot(Anteil~Group.1, data = wildschwein_all_anteil)


#Resultate Plots
ggplot() +
  geom_bar(data=wildschwein_all, aes(sum(Anteil),fill = Frucht), position = "fill")+
  facet_grid(~Jahreszeit)+
  labs(x = "Jahreszeiten", y = "Aufteilung aller Lokationen in die verschiedenen Habitattypen", title = "Raumnutzung im Untersuchungsgebiet")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Frucht))+
  geom_point(data = wildschwein_all, aes(E, N))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y = "", title = "Raumnutzung Untersuchungsgebiet nach Habitattyp", subtitle = "")


#Kuchendiagramme und Prozentzahlen
pct <- round(wildschwein_all_anteil$Anteil/sum(wildschwein_all_anteil$Anteil)*100)
lbls <- paste(wildschwein_all_anteil$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_all_anteil$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der aller Lokationen nach Vegetationstyp")

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
