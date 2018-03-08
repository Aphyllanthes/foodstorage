library(RSQLite)
#library(readr)
library(data.table)
library(sf)
library(ggplot2)
library(rgdal)
library(dplyr)
library(raster)
library(tidyr)
library(foodstorage)
#library(ggmap)

############
con <- dbConnect(SQLite(), "data/kornInfo.sqlite")

#dbListTables(con)
#dbListFields(con, "kornumsatz_origin")
#dbListFields(con, "productInfo")
#dbListFields(con, "producerAdress")

productInfo <- dbGetQuery(con, "SELECT * FROM productInfo")
producerAdress <- dbGetQuery(con, "SELECT * FROM producerAdress")
kornumsatz <- dbGetQuery(con, "SELECT * FROM kornumsatz_origin")
origin <- dbGetQuery(con, "SELECT * FROM productOrigin")
Kornkammer <- dbGetQuery(con, "SELECT * from AdresseKornkammer")

productOrigin <- origin

KUperYear <- kornumsatz_perYear(kornumsatz = kornumsatz, productInfo = productInfo)
KU <- KUperYear %>% 
  spread(Jahr, Umsatz) %>% 
  mutate(avg = mean(c(`2016`, `2017`), na.rm = T))
names(KU) <- c("Produkte_Zusammenfassung", "turnover2015", "turnover2016", "turnover2017", "avg.turnover")

originWithDistances <- SupplierDistance(origin, producerAdress)

totalDistances <- totalDistances(origin = origin, producers = producerAdress, productInfo = productInfo)

## count occurance of every product in the table, to split the turnover of the product to the different occurances.
totalDistances <- totalDistances %>% 
  add_count(Produkte_Zusammenfassung) %>% 
  left_join(KU, by = "Produkte_Zusammenfassung") %>% 
  mutate(turnover2015 = turnover2015 / n) %>% 
  mutate(turnover2016 = turnover2016 / n) %>% 
  mutate(turnover2017 = turnover2017 / n) %>% 
  mutate(avg.turnover = avg.turnover / n) %>% 
  mutate(Herkunftsgenauigkeit = ifelse(Lieferantentyp == "Erzeuger", 1, Herkunftsgenauigkeit))
  
meanDists <- totalDistances %>% 
  group_by(Produktgruppe) %>% 
  summarise(avgDistance = mean(Gesamtentfernung, na.rm=T))

###############################################
## prepare data for the plot:
producerAdress$xCoord <- as.numeric(producerAdress$xCoord)
producerAdress$yCoord <- as.numeric(producerAdress$yCoord)
## we only want to plot the producers where xCoordinates are available:
producersExist <- producerAdress[-which(is.na(producerAdress$xCoord)),]

warning( paste0("The producers " , paste0(producerAdress[which(is.na(producerAdress$xCoord)),"Lieferant"], collapse = ", "), " cannot be diplayed"))
## St georgener BAuer: Untermühlbachhof
##  Kaiserstühler Hof: Hof Homberg (google vom stühli)

## ändern: Stefan zu Stefan Chab Honig Imker (oder so)

# convert producers to spatialpointsdataframe
coordinates(producersExist) <- ~xCoord + yCoord

# create productOrigin SpatialPointsDataFrame only with existing origins:
productOriginExist <- productOrigin[!( is.na(productOrigin$xCoord) | is.na(productOrigin$yCoord)),]
coordinates(productOriginExist) <- ~xCoord + yCoord

coordinates(Kornkammer) <- ~xCoord + yCoord
crs(Kornkammer) <-  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

crs(producersExist) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

crs(productOriginExist) <- crs(producersExist)

writeOGR(productOriginExist, "data/productOriginExist/", "productOriginExist", "ESRI Shapefile")
writeOGR(producersExist, "data/producersExist/", "producersExist", "ESRI Shapefile")
writeOGR(Kornkammer, "data/Kornkammer/", "Kornkammer", "ESRI Shapefile")


dbDisconnect(con)

##################

#write.csv2(totalDistances, "data/totalDistances.csv")
