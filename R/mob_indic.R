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
#' @param poptab
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

mob_indic <- function (tabflows, idori, iddes, idflow, pol, idpol, poptab){

  # auto-contention
  poptab$Contention <- (poptab$TOTINTRA / (poptab$TOTORI + poptab$TOTINTRA))*100
  poptab$Contention <- ifelse(is.na(poptab$Contention), 0, poptab$Contention)

  # auto-sufficiency
  poptab$AutoSuff <- (poptab$TOTINTRA / (poptab$TOTDES + poptab$TOTINTRA))*100
  poptab$AutoSuff <- ifelse(is.na(poptab$AutoSuff), 0, poptab$AutoSuff)

  # Relative Balance
  poptab$RelBal <- (poptab$TOTDES - poptab$TOTORI) / (poptab$TOTORI + poptab$TOTDES)
  poptab$RelBal <- ifelse(is.na(poptab$RelBal), 0, poptab$RelBal)

  # Difference
  poptab$Difference <- poptab$TOTDES - poptab$TOTORI
  poptab$Difference <- ifelse(is.na(poptab$Difference), 0, poptab$Difference)

  # Percentage of total flows at origin
  poptab$PerOri <- (poptab$TOTORI*100) / sum(poptab$TOTORI)
  poptab$PerOri <- ifelse(is.na(poptab$PerOri), 0, poptab$PerOri)

  # Percentage of total flows at destination
  poptab$PerDes <- (poptab$TOTDES*100) / sum(poptab$TOTDES)
  poptab$PerDes <- ifelse(is.na(poptab$PerDes), 0, poptab$PerDes)

  # Percentage of total internal flows
  poptab$PerIntra <- (poptab$TOTINTRA*100) / sum(poptab$TOTINTRA)
  poptab$PerIntra <- ifelse(is.na(poptab$PerIntra), 0, poptab$PerIntra)

  polTabFull <- merge(x = pol, y = poptab, by.x = idpol, by.y = "CODGEO", all.x = TRUE)

  return(polTabFull)
}
