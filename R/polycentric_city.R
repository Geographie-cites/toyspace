#' DF transform to polycentric urban model
#'
#' This function allows you to change the origins and destination of flows
#' stored into a dataframe to simulate a polycentric urban model. Cities considered as employment
#' pole and containing railroad station are designed as candidate and flows (destinations only) are moved
#' from non-candidate cities to the nearest candidate cities (using osm network)
#'
#' @param pol An sf object of the cities
#' @param idpol A character string of the column containing the id of the pol object
#' @param cand A character string of the column containing binary (1, 0) candidate value of the pol object (1 must be equal to city considered as employment pole and containing a railroad station)
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#'
#' @return A data.frame of flows as tabflows with origins and destinations changed
#'
#' @examples
#' # Import data
#'
#' idpol <- "idpol"
#' cand <- "cand"
#' data(polL93)
#' data(tabflows)
#' iddes <- "DES"
#' idflow <- "FLOW"
#'
#' polL93$cand <- sample(0:1, size = nrow(polL93), replace = TRUE)
#'
#' poly_flows <- polycentric_city (
#' pol = polL93,
#' idpol = idpol,
#' cand = cand,
#' tabflows = tabflows,
#' iddes = iddes,
#' idflow = idflow)
#'
#' poly_flows[1:10,]
#'
#' @export
#' @importFrom sf st_centroid st_crs st_distance

polycentric_city <- function(pol, idpol, cand, tabflows, iddes, idflow){
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  dictionary <- relocate_one(pol = pol, idpol = idpol, cand = cand)
  tabflows$DES <- map_values(x = tabflows$DES, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  tabFlows <- tabflows[,c("ORI","DES","FLOW")]
  return(tabFlows)
}
