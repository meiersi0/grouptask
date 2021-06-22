library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(plyr)
library(viridis)
library(tidyr)

#install.packages("devtools") # <- if you havent installed devtools already
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")

library(ComputationalMovementAnalysisData)

head(wildschwein_BE)

wildschwein_BE<- wildschwein_BE%>%
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

Feldaufnahmen <- read_sf("Feldaufnahmen_Fanel.gpkg")%>%
  st_as_sf(coords = geom, crs = 2056, remove = FALSE)


#_________________________________________________________________________________
#Trainingsample erstellen


#trainingsample <- wildschwein_BE
trainingsample <- wildschwein_BE%>%filter(TierName %in% c("Ueli", "Caroline"), 
    DatetimeUTC > ymd_hms("2016-04-01 00:00:00"), DatetimeUTC < ymd_hms("2016-06-01 00:00:00"))

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
  mutate(static = stepMean < 5)

#Segment-based analysis
#defining ID's for trajectories
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

trainingsample_stops <- trainingsample_stops %>%
  mutate(segment_id = rle_id(static))
trainingsample_stops

#remove "moving" segments
trainingsample_filter <- trainingsample_stops %>%
  filter(static)

trainingsample_filter%>%
  ggplot()  +
  geom_point(aes(E, N, color = TierName)) +
  coord_fixed() +
  theme(legend.position = "bottom")

#_________________________________________________
#define segementcenter 
trainingsample_center<-aggregate(trainingsample_filter[, c(1,5:6)], list(trainingsample_filter$segment_id), mean)

ggplot() +
  geom_point(data = trainingsample_center, aes(E, N))

#join for TierID/TierName
trainingsample_center_join<-st_join(trainingsample_filter, trainingsample_center, by=segment_id)%>%
  st_as_sf(coords = c("E.y", "N.y"), crs = 2056, remove = FALSE)

ggplot() +
  geom_point(data = trainingsample_center_join, aes(E.y, N.y, color = TierName))

ggplot() +
  geom_sf(data=Feldaufnahmen, aes(fill = Frucht))+
  geom_point(data = trainingsample_center_join, aes(E.y, N.y, color = TierName))
  

#__________________________________________
#Feldaufnahmen joinen

wildschwein <- trainingsample_center_join[,-(8:11),drop=FALSE]
wildschwein <- wildschwein[,-(9:10),drop=FALSE]
wildschwein <- wildschwein[,-(5:6),drop=FALSE]

wildschwein_join <-st_join(wildschwein,Feldaufnahmen, suffix = c("E.y", "N.y"))

ggplot() +
  geom_sf(data=Feldaufnahmen, aes())+
  geom_point(data = wildschwein_join, aes(E.y, N.y, color = Frucht))






#______________________________________________
#Kernel Density
df<-trainingsample
find_hull <- function(df) df[chull(df$E, df$N), ]
hulls <- ddply(df, "TierName", find_hull)
plot <- ggplot(data = df, aes(x = E, y = N, colour=TierName, fill = TierName)) +
  geom_point() + 
  geom_polygon(data = hulls, alpha = 0.5) +
  labs(x = "E", y = "N")
plot

ggplot(df, aes(E,  N, colour = TierName, fill = TierName)) + 
  geom_point() + 
  stat_bag(prop = 0.95) +  # enclose 95% of points
  stat_bag(prop = 0.5, alpha = 0.5) + # enclose 50% of points
  stat_bag(prop = 0.05, alpha = 0.9) # enclose 5% of points

ggplot(trainingsample, aes(x=E, y=N))+
  geom_density_2d()

ggplot(trainingsample, aes(x=E, y=N) ) +
  geom_hex(bins = 30, alpha = 0.9) +
  scale_fill_continuous(low = "white", high = "red")+
  theme_bw()







