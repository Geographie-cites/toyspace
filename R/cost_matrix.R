#' Create square distance matrix
#'
#' This function allows you to create an Origin/Destination squared matrix from a Origin/Destination long format matrix
#'
#' @param tabcost A data.frame of flows between origins and destinations (long format matrix containing, at least, 3 column : origins, destinations, flows)
#' @param idpol A character vector id spatial units
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param iddist A character string giving the flow field name in tabflows
#'
#' @return A squared Origins (rows) and Destination (columns) matrix of flows
#'
#'
#' @export
#' @importFrom reshape2 dcast


cost_matrix <- function(tabcost, idpol, idori, iddes, iddist){
  tabCost <- tabflows[, c(idori, iddes, iddist)]
  tabIndex <- expand.grid(ORI = idpol,
                          DES = idpol,
                          stringsAsFactors = FALSE)
  tabIndex$KEY <- paste(tabIndex$ORI, tabIndex$DES, sep = "_")

  colnames(tabCost)[1:3] <- c("ORI", "DES", "DIST")
  tabCost$KEY <- paste(tabCost$ORI, tabCost$DES, sep = "_")

  tabIndex <- merge(x = tabIndex, y = tabflows[, c("KEY", "DIST")], by = "KEY", all.x = TRUE)
  colnames(tabIndex)[1:3] <- c("ORI", "DES", "DIST")
  tabWide <- dcast(tabIndex, formula = ORI ~ DES, value.var = "DIST", fill = 0, drop = FALSE)
  matCost <- as.matrix(tabWide[, -1])
  row.names(matCost) <- colnames(matCost)
  return(matCost)
}


