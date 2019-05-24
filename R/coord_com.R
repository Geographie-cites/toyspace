#' cities coordinates
#'
#' This function allows you to store the coordinates of each cities in a dataframe
#'
#' @param pol An sf object of the cities
#'
#' @return A data.frame with the same variables as pol, centroïds (lat,lon) and no geometries
#'
#' @examples
#' # Import data
#' data(pol)
#'
#' coordCom <- coord_com(pol)
#'
#' coordCom[1:10,]
#'
#' @export
#' @importFrom sf st_centroid st_crs st_geometry
#' @importFrom rgdal project


coord_com <- function(pol){
    shapeSfCent <- st_centroid(pol)
    xy <- do.call(rbind, st_geometry(shapeSfCent))
    shapeSfCent$lon <- xy[,1]
    shapeSfCent$lat <- xy[,2]
    shapeSfCent$geometry <- NULL
  return(shapeSfCent)
}

