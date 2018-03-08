
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
#library(foodstorage)
library(rgdal)

####
# packages for data preparation:
library(RSQLite)
library(data.table)
library(sf)
library(raster)
library(tidyr)
library("geosphere")
####

######## generateGeoData ##############
con <- dbConnect(SQLite(), "../data/kornInfo.sqlite")

productInfo <- dbGetQuery(con, "SELECT * FROM productInfo")
producerAdress <- dbGetQuery(con, "Select * from producerAdress")
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

##############
totalDistancesFun <- function(origin = productOrigin, producers = producerAdress, productInfo){
  ## the funciton SupplierDistance from the script "calculateDistances_Anna.R"
  originWithDistances <- SupplierDistance(origin, producers)
  
  # gather the two columns "Lieferant" and "Lieferant2" into seperated rows.
  products <- productInfo %>%
    gather("n", "Lieferant", 3:4) %>%
    filter(Lieferant != "") %>%
    dplyr::select(-n)
  
  # join the originWithDistances to the products table
  products1 <- products %>%
    left_join(originWithDistances[, c("Lieferant", "Produkte_Zusammenfassung", "Ort", "EntfernungZwischenhaendler", "Herkunftsgenauigkeit")], by=c("Lieferant", "Produkte_Zusammenfassung"))
  
  # join the Distances to the producer from the producerAdress-table to the products table
  producerAdress$EntfernungKK <- as.numeric(producerAdress$EntfernungKK)
  products2 <- products1 %>%
    left_join(producerAdress[, c("Lieferant", "Lieferantentyp", "EntfernungKK")], by= "Lieferant") %>%
    mutate(EntfernungZwischenhaendler = ifelse(Lieferantentyp == "Zwischenhaendler",
                                               EntfernungZwischenhaendler, 0)) %>%
    mutate(Gesamtentfernung = EntfernungKK + EntfernungZwischenhaendler)
  return(products2)
}

##############

totalDistances <- totalDistancesFun(origin = origin, producers = producerAdress, productInfo = productInfo)

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

######
## prepare data for the plot:
producerAdress$xCoord <- as.numeric(producerAdress$xCoord)
producerAdress$yCoord <- as.numeric(producerAdress$yCoord)
## we only want to plot the producers where xCoordinates are available:
producersExist <- producerAdress

#warning( paste0("The producers " , paste0(producerAdress[which(is.na(producerAdress$xCoord)),"Lieferant"], collapse = ", "), " cannot be diplayed"))
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

dbDisconnect(con)

########### create Curves Function ##################

createCurves <- function(i, tD = totalDistances, pEx = producersExist, pOE = productOriginExist){
  productOriginExist <- pOE
  producersExist <- pEx
  totalDistances <- tD
  pE <- which(producersExist$Lieferant == totalDistances$Lieferant[i])
  if(totalDistances$Lieferantentyp[i] == "Zwischenhaendler" & 
     totalDistances$Lieferant[i] %in% unique(productOriginExist$Lieferant) & length(pE) > 0){
    
    z <- which(productOriginExist$Lieferant == totalDistances$Lieferant[i] &
                 productOriginExist$Produkte_Zusammenfassung == totalDistances$Produkte_Zusammenfassung[i] &
                 productOriginExist$Ort == totalDistances$Ort[i])
    Li <- Lines(list(Line(gcIntermediate(coordinates(Kornkammer), coordinates(producersExist)[pE,], addStartEnd = T)), 
                     Line(gcIntermediate(coordinates(producersExist)[pE,], coordinates(productOriginExist)[z,], addStartEnd = T))) , ID = i)
  } else {
    if(length(pE)>0){
      Li <- Lines(Line(gcIntermediate(coordinates(Kornkammer), coordinates(producersExist)[pE,], addStartEnd = T)), ID = i)
      } else {Li <- Lines(Line(coords = cbind(x = c(0,0), y = c(0,0))), ID = i)}
  }
  return(Li)
}


liste <- list()

for(i in 1:nrow(totalDistances)){
  liste[[i]] <- createCurves(i)
}

producersL <- SpatialLines(liste, proj4string = crs(producersExist)) #crs(producersExist)
producersInfo <- SpatialLinesDataFrame(producersL, totalDistances)



#################
# producersInfo <- readOGR("../data/producersInfo/", "producersInfo")
# 
# names(producersInfo) <- c('X', 'Produkte_App', 'Produkte_Zusammenfassung', 'Produktgruppe', 'Verpackungseinheit', 'Lieferant', 'Ort', 'EntfernungZwischenhaendler', 'Herkunftsgenauigkeit', 'Lieferantentyp', 'EntfernungKK', 'Gesamtentfernung', 'n', 'turnover2015', 'turnover2016', 'turnover2017', 'avg.turnover')
# producersInfo$avg.turnover <- as.numeric(producersInfo$avg.turnover)
# producersInfo$Gesamtentfernung <- as.numeric(producersInfo$Gesamtentfernung)
# producersInfo$turnover2017 <- as.numeric(producersInfo$turnover2017)
# producersInfo$Produktgruppe <- as.character(producersInfo$Produktgruppe)
# producersInfo$Produkte_Zusammenfassung <- as.character(producersInfo$Produkte_Zusammenfassung)
# 
# totalDistances <- as.data.frame(producersInfo)

newtotalDistances <- createDistanceCategory(totalDistances)

pal <- colorFactor(c("darkgreen", "orange", "darkred"), domain = c(  "Erzeuger", "Produzent", "Zwischenhaendler"))

pal2 <- colorNumeric(
  palette = "viridis",
  domain = producersInfo$avg.turnover, n = 10, reverse = F)

# Erstellen der beiden Datensätze, zwischen denen gewählt werden kann

avg.turnOver <- totalDistances %>%
  group_by(Produktgruppe)%>%
  summarise(Menge = sum(avg.turnover, na.rm = T), Distanz = mean(Gesamtentfernung, na.rm = T))

turnOver2017 <- totalDistances %>%
  group_by(Produktgruppe)%>%
  summarise(Menge = sum(turnover2017, na.rm = T), Distanz = mean(Gesamtentfernung, na.rm = T))

ui <- fluidPage(
   
   titlePanel("Die Produkte der FoodCoop Kornkammer"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("Selection", "Auswahl", c("Alle Produktgruppen", unique(producersInfo$Produktgruppe), selected = NULL)),
         hr(),
         uiOutput("ui")#,
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Karte der Produktherkunft", br(),
                   p(em("Die Herkunftsgenauigkeit der Produkte variiert: gestrichelte Linien stellen eine unsichere Herkunftsangabe dar")),
                   leafletOutput("mymap")),
          tabPanel("Umsatz der Produkte", br(), plotOutput("Mengenplot")),
          tabPanel("Transportdistanz der Produkte",  br(), plotOutput("Distanzplot")),
          tabPanel("Transport vs. Umsatz",  br(), plotOutput("distPlot"), br(),
                   plotOutput("distBarPlot")
                   )
        )
         
      )
   )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    turnOverYear <- avg.turnOver
     
     # Erstellen des Plots
     ggplot(turnOverYear, aes(Distanz, Menge)) + 
       geom_point(aes(color = Produktgruppe, size = 3)) +
       scale_size(guide = "none") 
   })
  
  output$distBarPlot <- renderPlot ({
    positions <- c("0-100", "100-200", "200-400", "400-800",
                   "800-1600", "1600-3200", "3200-6400", "6400-12800", "NA")
    
    ggplot(newtotalDistances, aes(x = Kategorie, y = avg.turnover, 
                                  fill = Produktgruppe)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = positions)+
      labs(title = "Konsumierte Produkte nach Distanz", 
           y = "Umsatz in kg bzw. L",
           x = "Distanz [km]")
  })
   
   #############################
   

   output$ui <- renderUI({
     Produkte <- unique(subset(producersInfo, Produktgruppe == input$Selection)$Produkte_Zusammenfassung)
     if (is.null(input$input_type))
       return(if(input$Selection != "Alle Produktgruppen"){
         radioButtons("Produkt", "Produkt",
                            choices = c("Alle Produkte", Produkte))#, selection = "Alle Produkte"
       } else { radioButtons("Produkt", "Produkt",
                             choices = c("Alle Produkte"))}
       )
   })
   

   prodSel <- reactive({
     if(input$Selection == "Alle Produktgruppen"){producersSel <- producersInfo}
     if(input$Selection != "Alle Produktgruppen"){ # & input$Produkt == "Alle Produkte"
       producersSel <- subset(producersInfo, Produktgruppe == input$Selection) 
     } 
     if(input$Selection != "Alle Produktgruppen" & input$Produkt != "Alle Produkte"){
       producersSel <- subset(producersInfo, Produkte_Zusammenfassung == input$Produkt) 
     }
     return(list("producersSel" = producersSel))
   })

   output$mymap <- renderLeaflet({
      prodSelect <- prodSel()$producersSel
      dashs <- prodSelect$Herkunftsgenauigkeit
      dashs[which(dashs == 2)] <- 1
      dashs[which(dashs == 1)] <- 0
      dashs[which(dashs != 0)] <- 10
      dash <- as.factor(dashs)
     leaflet(prodSelect) %>% 
       #addTiles() %>% 
       addProviderTiles(providers$CartoDB.Positron) %>% 
       addPolylines(weight = ifelse((prodSelect$avg.turnover/20) < 1, 1, (prodSelect$avg.turnover)/20),
                    color = ~pal2(prodSelect$avg.turnover),#
                    popup = prodSelect$Produkte_Zusammenfassung, dashArray = dash) %>% 
       addLegend(pal = pal2, values = ~prodSelect$avg.turnover, title = "Umsatz pro Jahr",
                labFormat = labelFormat(suffix = " kg/yr")) %>%
       # addCircleMarkers(data = producersExist, radius = 2,
       #                  stroke = FALSE, fillOpacity = 0.8, color=pal(producersExist$Lieferantentyp),
       #                  popup = producersExist$Lieferant) %>%  #, clusterOptions = markerClusterOptions()
       # addLegend("bottomright",
       #           pal = pal, values = ~producersExist$Lieferantentyp,
       #           title = "Lieferantentyp",
       #           opacity = 1
       # ) %>%
       addMarkers(Kornkammer, lng = coordinates(Kornkammer)[1], lat = coordinates(Kornkammer)[2],
                  popup= "Kornkammer") 
     
   })
   
   output$Mengenplot <- renderPlot({
     data <- as.data.frame(prodSel()$producersSel)
     
     # Erstellen des Plots
     if(input$Selection == "Alle Produktgruppen"){
       bild <- ggplot(data, aes(Produktgruppe, avg.turnover, fill = Produktgruppe)) + 
         geom_bar(stat = "identity")  + ylab("Durchschnittlicher Umsatz pro Jahr [kg]") +
         theme(axis.text.x = element_text(size=10, angle=45, hjust = 1))
     } else {
       bild <- ggplot(data, aes(Produkte_Zusammenfassung, avg.turnover, fill = Produkte_Zusammenfassung)) + 
         geom_bar(stat = "identity")+ ylab("Durchschnittlicher Umsatz pro Jahr [kg]") +
         theme(axis.text.x = element_text(size=10, angle=45, hjust = 1))
     }
     bild
   })

   output$Distanzplot <- renderPlot({
     data <- as.data.frame(prodSel()$producersSel)
     
     # Erstellen des Plots
     if(input$Selection == "Alle Produktgruppen"){
       bild <- ggplot(data, aes(Produktgruppe, Gesamtentfernung, fill = Produktgruppe)) + 
         geom_bar(stat = "identity")  + 
         theme(axis.text.x = element_text(size=10, angle=45, hjust = 1))
     } else {
       bild <- ggplot(data, aes(Produkte_Zusammenfassung, Gesamtentfernung, fill = Produkte_Zusammenfassung)) + 
         geom_bar(stat = "identity") + 
         theme(axis.text.x = element_text(size=10, angle=45, hjust = 1))
     }
     bild
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

