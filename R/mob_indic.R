#' Shape mobility indicators
#'
#' This function allows you to create an sf object containing mobility indicators in each polygons using a data.frame of flows
#'
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, 3 column : origins, destinations, flows)
#' @param tabdist An sf object of the cities
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
#'
#' @export

mob_indic <- function (tabflows, tabdist, idori, iddes, idflow, iddist, pol, idpol){
  tabflows$KEY <- paste(tabflows[[idori]], tabflows[[iddes]], sep = "_")
  tabdist$KEY <- paste(tabdist[[idori]], tabdist[[iddes]], sep = "_")
  tabFlows <- merge(tabflows, tabdist[, c(iddist, "KEY")], by = "KEY", all.x = TRUE)

  popTab <- pop_tab(tabflows = tabFlows, idori = idori, iddes = iddes, idflow = idflow, iddist = iddist)

  # auto-contention
  popTab$Contention <- (popTab$TOTINTRA / (popTab$TOTORI + popTab$TOTINTRA))*100
  popTab$Contention <- ifelse(is.na(popTab$Contention), 0, popTab$Contention)

  # auto-sufficiency
  popTab$AutoSuff <- (popTab$TOTINTRA / (popTab$TOTDES + popTab$TOTINTRA))*100
  popTab$AutoSuff <- ifelse(is.na(popTab$AutoSuff), 0, popTab$AutoSuff)

  # Relative Balance
  popTab$RelBal <- (popTab$TOTDES - popTab$TOTORI) / (popTab$TOTORI + popTab$TOTDES)
  popTab$RelBal <- ifelse(is.na(popTab$RelBal), 0, popTab$RelBal)

  # Difference
  popTab$Difference <- popTab$TOTDES - popTab$TOTORI
  popTab$Difference <- ifelse(is.na(popTab$Difference), 0, popTab$Difference)

  # Percentage of total flows at origin
  popTab$PerOri <- (popTab$TOTORI*100) / sum(popTab$TOTORI)
  popTab$PerOri <- ifelse(is.na(popTab$PerOri), 0, popTab$PerOri)

  # Percentage of total flows at destination
  popTab$PerDes <- (popTab$TOTDES*100) / sum(popTab$TOTDES)
  popTab$PerDes <- ifelse(is.na(popTab$PerDes), 0, popTab$PerDes)

  # Percentage of total internal flows
  popTab$PerIntra <- (popTab$TOTINTRA*100) / sum(popTab$TOTINTRA)
  popTab$PerIntra <- ifelse(is.na(popTab$PerIntra), 0, popTab$PerIntra)

  polTabFull <- merge(x = pol, y = popTab, by.x = idpol, by.y = "CODGEO", all.x = TRUE)

  return(polTabFull)
}
