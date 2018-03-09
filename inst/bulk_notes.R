
## replace or write tables into database kornInfo.sqlite:
replaceTableDB <- function(connection, tablename, newtable){
  if(tablename %in% dbListTables(connection)){dbRemoveTable(con, tablename)}
  dbWriteTable(con, tablename, newtable)
}

#con <- dbConnect(SQLite(), "data/kornInfo.sqlite")


################ update AdressseKornkammer table ###################

Kornkammer <- data.table(Name = "Kornkammer", Strasse = "Höheweg 3", PLZ= "79104", Ort = "Freiburg")
Kornkammer[1, xCoord := geocode(paste0(Strasse, ", ", PLZ, " ", Ort))[1]]
Kornkammer[1, yCoord := geocode(paste0(Strasse, ", ", PLZ, " ", Ort))[2]]

replaceTableDB(con, "AdresseKornkammer", Kornkammer)

############## update producerAdress table #################

produzenten <- read.csv2("data/produzenten.csv")
replaceTableDB(con, "producerAdress", produzenten)

################## calculation of the distances in the producerAdress table:
# write distances to the kornkammer into the database table "producerAdress"
producersExist$EntfernungKK <- spDistsN1(producersExist, Kornkammer, longlat = T)
Distances <- data.table(Lieferant = producersExist$Lieferant, EntfernungKK = producersExist$EntfernungKK)

producers <- as.data.table(left_join(producers, Distances, by="Lieferant"))
# write Distance to KK into the database table producerAdress

replaceTableDB(con, "producerAdress", producers)

############## update productOrigin table ###############
productOrigin <- read.csv2("../Produktherkunft.csv", sep = ",")
names(productOrigin)[4] <- "Herkunftsgenauigkeit"
names(productOrigin)[5] <- "Kommentar"
productOrigin$xCoord <- as.numeric(NA)
productOrigin$yCoord <- as.numeric(NA)
productOrigin$Ort <- as.character(productOrigin$Ort)

prOrig <- getCoordinates(prOrig)

productOrigin <- data.frame(Lieferant = prOrig$Lieferant, 
                            Produkte_Zusammenfassung = prOrig$Produkte_Zusammenfassung,
                            Ort = prOrig$Ort,
                            Herkunftsgenauigkeit = prOrig$Herkunftsgenauigkeit, 
                            Kommentar = prOrig$Kommentar, 
                            xCoord = unlist(prOrig$xCoord), 
                            yCoord = unlist(prOrig$yCoord))
replaceTableDB(con, "productOrigin", productOrigin)

#dbSendQuery(con, "UPDATE producerAdress SET geometry=MakePoint(xCoord, yCoord, 3857)")


##########################################
#create template for productOrigin table (as productOrigin.csv)

producers <- as.data.table(producers)
Zwischenhaendler <- producers[Lieferantentyp=="Zwischenhaendler", Lieferant]

productInfo <- as.data.table(productInfo)

# für jeden Zwischenhaendler eine tabelle mit: Produkt, Herkunftsadresse (Strasse, PLZ, Ort) oder Land und xCoord und yCoord
#productOrigin <- data.table(Lieferant = "", Produkt = "", Adresse = "", xCoord="", yCoord="")

pI1 <- productInfo[Lieferant %in% Zwischenhaendler, .(Lieferant, Produkte_Zusammenfassung)]
pI2 <- productInfo[Lieferant2 %in% Zwischenhaendler, .(Lieferant2, Produkte_Zusammenfassung)]
names(pI2) <- names(pI1)

productOrigin <- unique(bind_rows(pI1, pI2))[order(Lieferant),]

write.csv2(productOrigin, "../productOrigin.csv")

########### get informations about original database ################
korn <- dbConnect(SQLite(), "data/Backup_example.BAK")

dbListTables(korn)
dbListFields(korn, "accounts")

accountTop <- dbGetQuery(korn, "SELECT * FROM accounts LIMIT 10")

androidTop <- dbGetQuery(korn, "SELECT * FROM android_metadata LIMIT 10")

productsTop <- dbGetQuery(korn, "SELECT * FROM products LIMIT 100")

sessionsTop <- dbGetQuery(korn, "SELECT * FROM sessions LIMIT 10")

sequenceTop <- dbGetQuery(korn, "SELECT * FROM sqlite_sequence LIMIT 100")

transprodTop <- dbGetQuery(korn, "SELECT * FROM transaction_products LIMIT 10")

transactionsTop <- dbGetQuery(korn, "SELECT * FROM transactions LIMIT 10")

############################
# Möglichkeit: Einteilung der Distanzen in 1500 km -Schritten
newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung <= 1500] <- "0-1500"
newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 1500 & newtotalDistances$Gesamtentfernung <= 3000] <- "1500-3000"
newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 3000 & newtotalDistances$Gesamtentfernung <= 4500] <- "3000-4500"
newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 4500 & newtotalDistances$Gesamtentfernung <= 6000] <- "4500-6000"
newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 6000 & newtotalDistances$Gesamtentfernung <= 7500] <- "6000-7500"
newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 7000 & newtotalDistances$Gesamtentfernung <= 8500] <- "7000-8500"
newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 8500] <- "> 8500"



