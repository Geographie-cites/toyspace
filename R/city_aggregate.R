#' Aggregate cities of a DF
#'
#' This function allows you to aggregate cities of a dataframe from designed identifier
#'
#' @param before A list of the identifiers to be replaced as numeric
#' @param after A number of the identifier replacement
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows (numeric)
#' @param iddes A character string giving the destination field name in tabflows (numeric)
#'
#' @return A data.frame with the same variables as the shape, centroïds (lat,lon) and no geometries
#'
#' @examples
#' # Import data
#' before <-  c(75101, 75102, 75103,75104, 75105,75106,75107, 75108, 75109,
#'              75110,75111, 75112, 75113, 75114,75115, 75116, 75117,75118, 75119, 75120)
#' after <- 75056
#' data(tabflows)
#' idori <- "ORI"
#' iddes <- "DES"
#'
#' cityAggregate <- city_aggregate(before, after, tabflows, idori, iddes)
#'
#' cityAggregate[1:10,]
#'
#' @export

city_aggregate <- function(before, after, tabflows, idori, iddes){
  dicoAgr <- data.frame("OLDCODE" = before, "NEWCODE" = after)
  tabflows$ORIAGR <- map_values(x = tabflows[[idori]], from = dicoAgr$OLDCODE, to = dicoAgr$NEWCODE)
  tabflows$DESAGR <- map_values(x = tabflows[[iddes]], from = dicoAgr$OLDCODE, to = dicoAgr$NEWCODE)
  return(tabflows)
}
