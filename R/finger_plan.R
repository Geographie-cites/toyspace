# finger plan configuration ----

#' DF transform to finger plan urban model
#'
#' This function allows you to change the origins and destination of flows
#' stored into a dataframe to simulate a finger-plan urban model. Cities containing
#' railroad station are designed as candidate and flows (origins and destinations)
#' are moved from non-candidate cities to the nearest candidate cities (using osm network)
#'
#' @param pol An sf object of the cities
#' @param idpol A character string of the column containing the id of the pol object
#' @param cand A character string of the column containing binary (1, 0) candidate value of the pol object (1 must be equal to city containing a railroad station)
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
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
#' idori <- "ORI"
#' iddes <- "DES"
#' idflow <- "FLOW"
#'
#' polL93$cand <- sample(0:1, size = nrow(polL93), replace = TRUE)
#'
#' fg_flows <- finger_plan(
#' pol = polL93,
#' idpol = idpol,
#' cand = cand,
#' tabflows = tabflows,
#' idori = idori,
#' iddes = iddes,
#' idflow = idflow)
#'
#' fg_flows[1:10,]
#'
#' @export
#' @importFrom sf st_centroid st_crs st_distance

finger_plan <- function(pol, idpol, cand, tabflows, idori, iddes, idflow){
  tabflows$ORI <- tabflows[[idori]]
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  dictionary <- relocate_one(pol = pol, idpol = idpol, cand = cand)
  tabflows$ORI <- map_values(x = tabflows$ORI, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  tabflows$DES <- map_values(x = tabflows$DES, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  return(tabflows)
}
