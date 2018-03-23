library(DT)
library(magrittr)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

# Cleaning Workspace
rm(list=ls())

# Setting Workspace (Please change to the directory of the CSV file)
setwd("E:/Workspace/Scratch Folder/R/Project")


# Reading Data
#------------------
MeteoritesLandings = read.csv("meteorite-landings.csv")
MeteoritesLandings = MeteoritesLandings %>% 
  # filter out weird years 
  filter(year>=860 & year<=2016) %>% 
  # filter out weird locations
  filter(reclong<=180 & reclong>=-180 & (reclat!=0 | reclong!=0))

# Peek into the Data
datatable(head(MeteoritesLandings), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


# Class Analysis
#------------------

# Meteorites Class Count
# ----------------------
# We plot the Twenty most occcuring Meteorities in a flipped bar plot
MeteoritesCount = MeteoritesLandings %>%
  group_by(recclass) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(recclass = reorder(recclass,Count)) %>%
  head(20) 

fillColor = "#577DF9"
fillColor2 = "#F1C40F"

MeteoritesCount %>%
  
  ggplot(aes(x = recclass,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = recclass, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Meteorites Class', 
       y = 'Count', 
       title = 'Meteorites Class and Count') +
  coord_flip() + 
  theme_bw()

# Tabular Version
datatable(MeteoritesCount, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


# Top 20 Class Of Meteorites ordered by Mass
# ------------------------------------------
# We plot the Twenty heaviest Meteorities based on their median mass in a flipped bar plot.
MeteoritesHeavyMedian = MeteoritesLandings %>%
  mutate(mass = mass/1e3) %>%
  group_by(recclass) %>%
  summarise(MassMed = median(mass)) %>%
  arrange(desc(MassMed)) %>%
  ungroup() %>%
  mutate(recclass = reorder(recclass,MassMed)) %>%
  head(20)

MeteoritesHeavyMedian %>%
  ggplot(aes(x = recclass,y = MassMed)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = recclass, y = 1, label = paste0("(",round(MassMed),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Meteorites Class', 
       y = 'Mass Median', 
       title = 'Meteorites Class and Mass Median') +
  coord_flip() + 
  theme_bw()

# Tabular Version
datatable(MeteoritesHeavyMedian, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))



# Distribution of Mass
# --------------------
#Here Mass is a continous variable and therfore for the distribution we plot a histogram. 
#We plot the distribution of the Mass of the Meteorites.

MeteoritesLandings %>%
  ggplot(aes(x = mass) )+
  geom_histogram(fill = fillColor2) +
  scale_x_log10() +
  scale_y_log10() + 
  labs(x = 'Mass in gms' ,y = 'Count', title = paste("Distribution of", "mass")) +
  theme_bw()


# Heaviest Meteorite
# ------------------
#The mass is in Kilograms.
MeteoriteHeaviest =  max(MeteoritesLandings$mass,na.rm = TRUE)
MetHeviestRec = MeteoritesLandings %>%
  filter(mass == MeteoriteHeaviest) %>%
  mutate( mass = mass/1e3)

datatable(MetHeviestRec, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


# Map of the Heaviest Meteorite
# -----------------------------
# The mass of the Meteorites are indicated by the Radius of the Circles.The radius is equal to the mass in kilograms multiplied by 10
factpal <- colorFactor(c("red","blue"), 
                       MeteoritesLandings$fall)

leaflet(MetHeviestRec) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~reclong, lat = ~reclat,radius = ~(mass)*10 ,
             color = ~factpal(fall))  %>%
  # controls
  setView(lng=MetHeviestRec$reclong, lat=MetHeviestRec$reclat,zoom = 4) %>%
  
  addLegend("bottomright", pal = factpal, values = ~fall,
            title = "Meteorites landings and fall",
            opacity = 1)


# Lightest Meteorite
# ------------------
# The mass is in Kilograms.
MeteoriteLightest =  min(MeteoritesLandings$mass,na.rm = TRUE)
MeteoriteLightestRec = MeteoritesLandings %>%
  filter(mass == MeteoriteLightest) %>%
  mutate( mass = mass/1e3)

# Tabular
datatable(MeteoriteLightestRec, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


# Valid Lightest Meteorite
# ------------------------
# The mass is in Kilograms.
MeteoritesLandingsValid = MeteoritesLandings %>%
  filter(nametype == 'Valid')
MeteoriteLightest =  min(MeteoritesLandingsValid$mass,na.rm = TRUE)
MeteoriteLightestRec = MeteoritesLandingsValid %>%
  filter(mass == MeteoriteLightest) %>%
  mutate( mass = mass/1e3)

# Tabular
datatable(MeteoriteLightestRec, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


# Distribution of Mass classified by Fall Type
# --------------------------------------------
MeteoritesLandings %>%
  ggplot(aes(x = mass, fill = fall)) +
  geom_histogram(alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10() + 
  labs(x= 'Mass in gms',y = 'Count', title = paste("Distribution of", ' mass ')) +
  theme_bw()


# Mass with plots for each fall Type
# ----------------------------------
# Here Mass is a continous variable and Fall is a categorical variable. To examine the relationships between a continous and categorical variable, we plot a facet bar plot.
MeteoritesLandings %>%
  ggplot(aes(x = mass, fill = fall)) +    
  geom_histogram(alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10() + 
  scale_fill_manual( values = c("red","blue") ) +
  facet_wrap(~fall) +
  labs(x= 'Mass in gms',y = 'Count', title = paste("Distribution of", ' mass ')) +
  theme_bw()


# Distribution of Meteorite Landings
# ----------------------------------
# The following plot shows the distribution of the meteorite landings all over the world.
center_reclong = median(MeteoritesLandings$reclong,na.rm = TRUE)
center_reclat = median(MeteoritesLandings$reclat,na.rm = TRUE)

leaflet(MeteoritesLandings) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~reclong, lat = ~reclat, 
             color = c("red"))  %>%
  # controls
  setView(lng=0, lat=0,zoom = 2)


# Distribution of Meteorite Landings with Meteorite Mass
# ------------------------------------------------------
# The mass of the Meteorites are indicated by the Radius of the Circles.
factpal <- colorFactor(c("red","blue"), 
                       MeteoritesLandings$fall)

leaflet(MeteoritesLandings) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~reclong, lat = ~reclat,radius = ~(mass/1e3)*10 ,
             color = ~factpal(fall))  %>%
  # controls
  setView(lng=0, lat=0,zoom = 2) %>%
  addLegend("bottomright", pal = factpal, values = ~fall,
            title = "Meteorites landings and fall",
            opacity = 1)


# Distribution of US Meteorite Landings
# -------------------------------------
#The following plot shows the distribution of the US meteorite landings.Here we have filtered the US meteorite landings by filtering the latitude and longitude.
top = 49.3457868 # north lat
left = -124.7844079 # west long
right = -66.9513812 # east long
bottom =  24.7433195 # south lat

USMeteoritesLandings = MeteoritesLandings %>%
  filter(reclat >= bottom) %>%
  filter ( reclat <= top) %>%
  filter( reclong >= left ) %>%
  filter(reclong <= right)

center_reclong = median(USMeteoritesLandings$reclong,na.rm = TRUE)
center_reclat = median(USMeteoritesLandings$reclat,na.rm = TRUE)

leaflet(USMeteoritesLandings) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~reclong, lat = ~reclat, 
             color = c("blue"))  %>%
  # controls
  setView(lng=center_reclong, lat=center_reclat,zoom = 4) 

# Peek into the US Met Landings Data
USMeteoritesLandings = USMeteoritesLandings %>% arrange(desc(mass))
# Tabular
datatable(USMeteoritesLandings, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))



# Distribution of US Meteorite Landings with Meteorite Mass
# ---------------------------------------------------------
# The mass of the Meteorites are indicated by the Radius of the Circles. The mass has been converted to Kgs and then multiplied by 10
factpal <- colorFactor(c("red","blue"), 
                       USMeteoritesLandings$fall)

leaflet(USMeteoritesLandings) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~reclong, lat = ~reclat,radius = ~(mass/1e3)*10 ,
             color = ~factpal(fall))  %>%
  # controls
  setView(lng=center_reclong, lat=center_reclat,zoom = 4) %>%
  
  addLegend("bottomright", pal = factpal, values = ~fall,
            title = "Meteorites landings and fall",
            opacity = 1)

# Heatmap of US Meteorite Landings
# --------------------------------
# The intensity of the Heatmap is based on the mass of the meteorites.
USMeteoritesLandings %>% leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>% 
  addHeatmap(lng = ~reclong, lat = ~reclat, intensity = ~mass,
             blur = 20, max = 0.05, radius = 15) %>%
  # controls
  setView(lng=center_reclong, lat=center_reclat,zoom = 4) 


# Distribution of US Meteorite Landings with Cluster Markers
# ----------------------------------------------------------
#The meteorite landings have been clustered and their numbers are being shown in the map.
USMeteoritesLandings %>% leaflet() %>% addProviderTiles("Esri.OceanBasemap") %>% 
  addMarkers(lng = ~reclong, lat = ~reclat,clusterOptions = markerClusterOptions()) %>%
  # controls
  setView(lng=center_reclong, lat=center_reclat,zoom = 4) 


# Year Analysis
# -------------
# Years with the Most Meteorite Landings
MeteoritesLandings %>%
  group_by(year) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(year = reorder(year,Count)) %>%
  head(20) %>%
  
  ggplot(aes(x = year,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = year, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Meteorites Year', 
       y = 'Count', 
       title = 'Meteorites Year and Count') +
  coord_flip() + 
  theme_bw()


# Time Series from Year 1970
# --------------------------
#The time series shows the Count of the Meteorites and their corresponding Years.
MeteoritesLandings %>%
  filter(year >= 1970) %>%
  group_by(year) %>%
  summarise(Count = n()) %>%
  arrange(year) %>%
  ggplot(aes(x = year,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  
  labs(x = 'Meteorites Year', 
       y = 'Count', 
       title = 'Meteorites Year and Count') +
  theme_bw()
