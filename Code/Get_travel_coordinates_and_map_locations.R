# chikv outbreak positive kids travel locations -----------------------

library(ggmap)

# load data
chikv.travel <- read.csv("C:/Users/Jamie/Box Sync/FOI Kenya Project/travelNet.csv", head=T)

# Add country name to travel locations
chikv.travel$TravelLocation <- paste0(chikv.travel$where_travel_aic, ", Kenya")
chikv.travel$TravelLocation <- gsub("Tanzania, Kenya", "Tanzania", chikv.travel$TravelLocation)

chikv.travel$longitude <- as.numeric(NA)
chikv.travel$latitude <- as.numeric(NA)

# Use google api to get gps coordinates
for (i in 1:nrow(chikv.travel)){
  city <- as.character(chikv.travel$TravelLocation[i])
  coords <- geocode(city)
  chikv.travel$longitude[i] <- coords[1]
  chikv.travel$latitude[i] <- coords[2]
}

chikv.travel$longitude <- unlist(chikv.travel$longitude)
chikv.travel$latitude <- unlist(chikv.travel$latitude)

# remove empty row
chikv.travel <- chikv.travel[2:nrow(chikv.travel),]

# save data
write.csv(chikv.travel, "C:/Users/Jamie/Box Sync/FOI Kenya Project/travelNet_with_coordinates.csv" )

# Map travel destinations ------------------------------
library(leaflet)
source("C:/Users/Jamie/Box Sync/R_functions/addScaleBar.R")
chikv.travel <- read.csv("C:/Users/Jamie/Box Sync/FOI Kenya Project/travelNet_with_coordinates.csv", head=T)

chikv.travel$VillageInfo <- paste0("Location: ", chikv.travel$TravelLocation,
                       "<br> Number travelers from Msambweni: ", chikv.travel$Msambweni,
                       "<br> Number travelers from Ukunda: ", chikv.travel$Ukunda)

travel.map <- leaflet() %>% # %>% addTiles() %>% code for open maps rather than satellite, comment line below for open maps
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=chikv.travel, lat = ~latitude, lng = ~longitude, popup = ~VillageInfo, color='red')
addScaleBar(travel.map)

# Travel routes ---------------------------------------
# library(mapview)
library(raster)

# Msambweni coordinates=root
root <- matrix(c(39.483, -4.467), ncol = 2)
colnames(root) <- c("longitude", "latitude")

# end points
locations <- chikv.travel[,c("longitude", "latitude")]

lst <- lapply(1:nrow(locations), function(i) {
  SpatialLines(list(Lines(list(Line(rbind(root, locations[i, ]))), ID = i)), 
               proj4string = CRS("+init=epsg:4326"))
})

sln <- do.call("bind", lst)

# display map
mapview(sln)
