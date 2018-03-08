
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(foodstorage)
library(rgdal)

Kornkammer <- readOGR("../data/Kornkammer/", "Kornkammer")
producersInfo <- readOGR("../data/producersInfo/", "producersInfo")

names(producersInfo) <- c('Produkte_App', 'Produkte_Zusammenfassung', 'Produktgruppe', 'Verpackungseinheit', 'Lieferant', 'Ort', 'EntfernungZwischenhaendler', 'Herkunftsgenauigkeit', 'Lieferantentyp', 'EntfernungKK', 'Gesamtentfernung', 'n', 'turnover2015', 'turnover2016', 'turnover2017', 'avg.turnover')
producersInfo$avg.turnover <- as.numeric(producersInfo$avg.turnover)
producersInfo$Gesamtentfernung <- as.numeric(producersInfo$Gesamtentfernung)
producersInfo$turnover2017 <- as.numeric(producersInfo$turnover2017)
producersInfo$Produktgruppe <- as.character(producersInfo$Produktgruppe)
producersInfo$Produkte_Zusammenfassung <- as.character(producersInfo$Produkte_Zusammenfassung)

totalDistances <- as.data.frame(producersInfo)

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
       addLegend(pal = pal2, values = ~producersInfo$avg.turnover, title = "Umsatz pro Jahr",
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

