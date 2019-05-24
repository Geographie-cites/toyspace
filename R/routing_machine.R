#' Nearest neighbours using road network
#'
#' This function allows you to find the nearest neighbours from polygons centroïd to one another using the road network.
#' It creates a squared distance matrix between every city in meters.
#'
#' @param road An sf object of street network
#' @param pol An sf object of the cities
#' @param idpol A character string identifier of cities
#'
#' @return a squared matrix of distance in meters
#'
#' @examples
#' # Import data
#' data(road)
#' data(pol)
#' idpol <- "idpol"
#'
#' matDist <- routing_machine(road,pol,idpol)
#'
#' matDist[1:10,]
#'
#' @export
#' @importFrom dodgr weight_streetnet dodgr_dists
#' @importFrom sf st_centroid

routing_machine <- function(road, pol,idpol){
  #Set weight to the same
  road$wgt <- 0
  #Création du graph réseau
  roadgraph <- weight_streetnet(x = road, wt_profile = 0, type_col = "wgt")
  #Chopper les centroïdes
  shapesfCent <- st_centroid(pol)
  xy <- do.call(rbind, st_geometry(shapesfCent))
  xy <- data.frame (lon = xy [, 1], lat = xy [, 2])
  #Création de la matrice de distance
  matDist <- dodgr_dists(graph = roadgraph,
                         from = xy,
                         to = xy)
  row.names(matDist) <- shapesfCent[[idpol]]
  colnames(matDist) <- shapesfCent[[idpol]]
  return(matDist)
}


