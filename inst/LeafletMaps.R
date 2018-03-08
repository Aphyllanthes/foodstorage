
library(leaflet)

pal <- colorFactor(c("darkgreen", "blue", "red"), domain = c(  "Erzeuger", "Produzent", "Zwischenhaendler"))
KKIcon <- iconList(
  Kornkammer = makeIcon("data/icon-2.png", "data/icon-2.png", 24, 24)
)

map1 <- leaflet(producersExist) %>%
  addTiles() %>%
  addCircleMarkers(radius = 6,
                   stroke = FALSE, fillOpacity = 0.8, color=pal(producersExist$Lieferantentyp)) %>%  
  #, clusterOptions = markerClusterOptions()
  addLegend("bottomright",
            pal = pal, values = ~producersExist$Lieferantentyp,
            title = "Lieferantentyp",
            opacity = 1
  ) %>%
  addMarkers(Kornkammer,
             lng = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2],
             icon =  ~KKIcon[Kornkammer$Name]) %>% 
  addPolylines(data = producersRoutes, color = "black", opacity = 1, weight = 3)

library(mapview)
mapshot(map1, file = "map.pdf")

########################

producersInfoStraight$avg.turnover <- as.numeric(producersInfoStraight$avg.turnover)

pal2 <- colorNumeric(
  palette = "viridis",
  domain = producersInfo$avg.turnover, n = 10, reverse = F)

leaflet(producersRoutes) %>% 
  addTiles() %>% 
  addPolylines(color = "brown") #%>% 
  # addPolylines(data = producersInfoStraight, color = ~pal2(producersInfoStraight$avg.turnover),
  #              weight = (1/producersInfoStraight$Herkunftsgenauigkeit)*3,
  #              popup = producersInfoStraight$Produkte_Zusammenfassung,
  #              smoothFactor = 0.2, fillOpacity = 1) %>% 
  # addLegend(pal = pal2, values = ~producersInfoStraight$avg.turnover, title = "avg.turnover",
  #           labFormat = labelFormat(suffix = " kg/yr"))

dashs <- producersInfo$Herkunftsgenauigkeit
dashs[which(dashs == 2)] <- 1
dashs[which(dashs == 1)] <- 0
dashs[which(dashs != 0)] <- 10
dash <- as.factor(dashs)

leaflet(producersInfo) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(weight = ifelse((producersInfo$avg.turnover/15) < 2, 2, (producersInfo$avg.turnover)/15),
               color = ~pal2(producersInfo$avg.turnover),
               popup = producersInfo$Produkte_Zusammenfassung, dashArray = dash) %>% 
  addLegend(pal = pal2, values = ~producersInfo$avg.turnover, title = "avg.turnover",
            labFormat = labelFormat(suffix = " kg/yr"))

# leaflet(prodSelect) %>% 
#   #addTiles() %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   addPolylines(weight = ifelse((prodSelect$avg.turnover/20) < 1, 1, (prodSelect$avg.turnover)/20),
#                color = ~pal2(prodSelect$avg.turnover),#
#                popup = prodSelect$Produkte_Zusammenfassung, dashArray = dash) %>% 
#   addLegend(pal = pal2, values = ~producersInfo$avg.turnover, title = "Umsatz pro Jahr",
#             labFormat = labelFormat(suffix = " kg/yr")) %>%
#   # addCircleMarkers(data = producersExist, radius = 2,
#   #                  stroke = FALSE, fillOpacity = 0.8, color=pal(producersExist$Lieferantentyp),
#   #                  popup = producersExist$Lieferant) %>%  #, clusterOptions = markerClusterOptions()
#   # addLegend("bottomright",
#   #           pal = pal, values = ~producersExist$Lieferantentyp,
#   #           title = "Lieferantentyp",
#   #           opacity = 1
#   # ) %>%
#   addMarkers(Kornkammer, lng = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2],
#              popup= "Kornkammer") 


#################################

library(leaflet)

pal <- colorFactor(c("green", "orange", "red"), domain = c(  "Erzeuger", "Produzent", "Zwischenhaendler"))


KKIcon <- iconList(
  Kornkammer = makeIcon("data/icon-2.png", "data/icon-2.png", 24, 24)
)

leaflet(producersExist) %>%
  addTiles() %>%
  addCircleMarkers(radius = 6,
                   stroke = FALSE, fillOpacity = 0.8, color=pal(producersExist$Lieferantentyp),
                   popup = producersExist$Lieferant) %>%  #, clusterOptions = markerClusterOptions()
  addLegend("bottomright",
            pal = pal, values = ~producersExist$Lieferantentyp,
            title = "Lieferantentyp",
            opacity = 1
  ) %>%
  addMarkers(Kornkammer,
             lng = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2],
             icon =  ~KKIcon[Kornkammer$Name],
             popup= "Kornkammer") %>% 
  addPolylines(data = producersInfo, weight = 2, 
               popup = producersInfo$Produkte_Zusammenfassung)


leaflet(Kornkammer) %>% 
  addTiles() %>% 
  addMarkers(icon =  ~KKIcon[Name])
####################
oceanIcons <- iconList(
  ship = makeIcon("data/icon-2.png", "data/icon-2.png", 18, 18),
  pirate = makeIcon("data/logo-rund-small.png", "data/logo-rund-small@2x.png", 24, 24)
)