#' Aggregate cities of a sf object
#'
#' This function allows you to aggregate cities of a spatial dataframe from designed identifier
#'
#' @param before A list of the identifiers to be replaced
#' @param after A character string of the identifier replacement
#' @param pol An sf object of the cities
#' @param idpol A character string of the column containing the id of the pol object
#' @param namepol  A character string of the column containing the names of the pol object
#' @param nameAgr A character string of the name to be changed for the aggregated city
#'
#' @return An sf object with a new id of aggregate city, and the polygons aggregated
#'
#' @examples
#' # Import data
#' before <-  c(75101, 75102, 75103,75104, 75105,75106,75107, 75108, 75109,
#'              75110,75111, 75112, 75113, 75114,75115, 75116, 75117,75118, 75119, 75120)
#' after <- 75056
#' data(pol)
#' idpol <- "idpol"
#' namepol <- "name"
#' nameAgr <- "paris"
#'
#' polAggregate <- pol_aggregate(before, after, pol, idpol, namepol, nameAgr)
#'
#' plot(polAggregate)
#'
#' @export
#' @importFrom dplyr group_by summarise %>% left_join
#' @importFrom sf st_area st_geometry
#' @import lwgeom

pol_aggregate <- function(before, after, pol, idpol, namepol, nameAgr){
  dicoAgr <- data.frame("OLDCODE" = before, "NEWCODE" = after)
  pol[[idpol]] <- as.character(pol[[idpol]])
  pol$IDAGR <- map_values(x = pol[[idpol]], from = dicoAgr$OLDCODE, to = dicoAgr$NEWCODE)
  pol$area <- st_area(pol)
  polAgr <- pol %>%
    group_by(pol$IDAGR) %>%
    summarise(area = sum(pol$area))
  colnames(polAgr)[1] <- "IDAGR"
  sf::st_geometry(pol) <- NULL
  polAgr <- left_join(x = polAgr, y = pol[,c(idpol,namepol)], by = c('IDAGR' = idpol))
  polAgr[polAgr$IDAGR %in% as.character(after),][namepol]<- nameAgr
  return(polAgr)
}



