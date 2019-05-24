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
#'
#' @examples
#' # Import data
#' data(tabflows)
#' idori <- "ORI"
#' iddes <- "DES"
#' idflow <- "FLOW"
#' data(pol)
#' idpol <- "idpol"
#'
#' polflow <- mob_indic(tabflows, idori, iddes, idflow, pol, idpol)
#'
#' polflow[1:10,]
#'
#' @export

mob_indic <- function (tabflows, idori, iddes, idflow, pol, idpol){
  popTab <- pop_tab(tabflows, idori, iddes, idflow)
  #Building indicators
  #auto-contention
  popTab$Dependency <- popTab$TOTINTRA / (popTab$TOTORI + popTab$TOTINTRA)
  #auto-sufficiency
  popTab$AutoSuff <- popTab$TOTINTRA / (popTab$TOTDES + popTab$TOTINTRA)
  #Mobility
  popTab$Mobility <- (popTab$TOTDES+popTab$TOTORI) / (popTab$TOTORI + popTab$TOTINTRA)
  #Solde relatif
  popTab$RelBal <- (popTab$TOTDES-popTab$TOTORI) / (popTab$TOTORI + popTab$TOTDES)
  polflow <- merge(x = pol,y = popTab, by.x=idpol, by.y = "idflow")
  return(polflow)
}
