#' Create a dictionnary of id change
#'
#' This function is a low-level one and used in each *_city function (cbd_city, tod_city, finger_city ...etc.).
#' this gives a dictionary which indicates by which new identifier the old one must be changed, the latter
#' answering the criterion of proximity and to the candidacy of the city
#'
#' @param pol An sf object of the cities
#' @param idpol A character string of the column containing the id of the pol object
#' @param cand A character string of the column containing binary (1, 0) candidate value of the pol object
#'
#' @return A data.frame with two column : OLD (containing the id to be changed) and NEW (containing the new id)
#'
#' @examples
#' # Import data
#' data(polL93)
#' idpol <- "idpol"
#' cand <- "cand"
#'
#' polL93$cand <- sample(0:1, size = nrow(polL93), replace = TRUE)
#'
#' dictioTransfer <- relocate_one(polL93, idpol, cand)
#'
#' dictioTransfer[1:10,]
#'
#' @export
#' @importFrom sf st_centroid st_crs st_distance

relocate_one <- function(pol, idpol, cand){
  if(st_crs(pol)[[1]] != 2154) stop("Check the CRS (2154) and read the fucking manual")
  pol$ID <- pol[[idpol]]
  pol$KEY <- pol[[cand]]
  centroPol <- st_centroid(pol)
  oriRelocate <- centroPol
  desRelocate <- centroPol[centroPol$KEY == 1,]
  matDist <- st_distance(oriRelocate, desRelocate)
  idMin <- apply(matDist, 1, which.min)
  dictioTransfer <- data.frame("OLD" = pol$ID, "NEW" = pol$ID[idMin])
  return(dictioTransfer)
}



