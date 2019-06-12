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
#' @importFrom dodgr weight_streetnet dodgr_dists dodgr_to_igraph
#' @importFrom sf st_centroid
#' @importFrom igraph delete.vertices degree

routing_machine <- function(road, pol,idpol){

  road$wgt <- 0
  #Création du graph réseau
  roadgraph <- weight_streetnet(x = road, wt_profile = 0, type_col = "wgt")

  roadIgraph <- dodgr_to_igraph(roadgraph)
  roadIgraph <- induced_subgraph(
    roadIgraph, V(roadIgraph)[components(roadIgraph)$membership == which.max(components(roadIgraph)$csize)]
  )
  roadgraph <- igraph_to_dodgr(roadIgraph)
  roadgraph$n_from <- NULL
  roadgraph$n_to <- NULL
  roadgraph <- roadgraph[,c(1,2,7,8,3,9,10,4,5,6)]
  names(roadgraph) <- c("edge_id","from_id","from_lon","from_lat","to_id","to_lon","to_lat","d","d_weighted","component")
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

