#' @title calculate the total distances of the products.
#' @description 
#' from the data productOrigin, producersAdress and productInfo a table will be generated that contains the total distances for every product and its producer.
#' @param origin the table productOrigin with the information ...
#' @param producers the table producerAdress 
#' @param productInfo the table productInfo
#' @export


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




