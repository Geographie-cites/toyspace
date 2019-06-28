#' Shape mobility indicators
#'
#' This function allows you to create an sf object containing mobility indicators in each polygons using a data.frame of flows
#'
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, 3 column : origins, destinations, flows)
#' @param pol An sf object of the cities
#' @param idpol A character string identifier of cities
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#' @param iddist A character string giving the distance field name in tabflows
#'
#' @return An sf object of the cities with mobility indicators for each polygons
#'
#' There are 4 mobility indicators :
#'       - Self-Sufficiency (SelfSuff) : it refers to the ratio between internal flows and the amount of workers.
#'       It expresses the local balance between workers and jobs with a low rate of outflows
#'       (from 0 to 1, where 0 express dependency and 1 sufficiency)
#'       - Dependency (Dependency) : it refers to the ratio between internal flows and the population of the city.
#'       It expresses the level of dependence of a city in terms of job offer.
#'       - Mobility (Mobility) : it refers to the ratio between inflows and outflows on one hand, and the amount of workers on the other.
#'       It expresses the density of displacements in a city.
#'       - Relative Balance (RelBal) : it refers to the ratio between inflows less outflows on one hand and the population of the city on the other.
#'       It expresses the degree of polarization of a city, thus its attractiveness in terms of employment.
#'       - Percentage at Origin (perOri) : it refers to the percentage of total worker living in the city
#'       - Percentage at Destination (perDes) : it refers to the percentage of total worker working in the city
#'       - Percentage of InternalFlow (perIntra) : it refers to the percentage of total worker living and working in the city
#'
#' @examples
#' # Import data
#' data(tabflows)
#' idori <- "ORI"
#' iddes <- "DES"
#' idflow <- "FLOW"
#' iddist <- "DIST"
#' data(pol)
#' idpol <- "idpol"
#'
#' polflow <- mob_indic(tabflows, idori, iddes, idflow, iddist, pol, idpol)
#'
#' polflow[1:10,]
#'
#' @export

mob_indic <- function (tabflows, idori, iddes, idflow, iddist, pol, idpol){
  popTab <- pop_tab(tabflows = tabflows, idori = idori, iddes = iddes, idflow = idflow, iddist = iddist)
  #Building indicators
  #auto-contention
  popTab$Contention <- (popTab$TOTINTRA / (popTab$TOTORI + popTab$TOTINTRA))*100
  #auto-sufficiency
  popTab$AutoSuff <- (popTab$TOTINTRA / (popTab$TOTDES + popTab$TOTINTRA))*100
  #Relative Balance
  popTab$RelBal <- (popTab$TOTDES-popTab$TOTORI) / (popTab$TOTORI + popTab$TOTDES)
  #Difference
  popTab$Difference <- popTab$TOTDES-popTab$TOTORI
  #Percentage of total flows at origin
  popTab$perOri <- (popTab$TOTORI*100)/sum(popTab$TOTORI)
  #Percentage of total flows at destination
  popTab$perDes <- (popTab$TOTDES*100)/sum(popTab$TOTDES)
  #Percentage of total internal flows
  popTab$perIntra <- (popTab$TOTINTRA*100)/sum(popTab$TOTINTRA)

  popTab[is.na(popTab)] <- 0
  polflow <- merge(x = pol,y = popTab, by.x=idpol, by.y = "idflow")
  pointflow <- st_centroid(polflow)
  xy <- do.call(rbind, st_geometry(pointflow))
  polflow$lon <- xy[,1]
  polflow$lat <- xy[,2]
  return(polflow)
}
