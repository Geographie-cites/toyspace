#' Create an OD square matrix
#'
#' This function allows you to create an Origin/Destination squared matrix from a Origin/Destination long format matrix
#'
#' @param idpol A character vector with id values for the spatial units
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, 3 column : origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#'
#' @return A squared Origins (rows) and Destination (columns) matrix of flows
#'
#'
#' @export
#' @importFrom reshape2 dcast


cast_tabflows <- function(idpol, tabflows, idori, iddes, idflow){
  tabflows <- tabflows[, c(idori, iddes, idflow)]
  tabIndex <- expand.grid(ORI = idpol,
                          DES = idpol,
                          stringsAsFactors = FALSE)
  tabIndex$KEY <- paste(tabIndex$ORI, tabIndex$DES, sep = "_")

  colnames(tabflows)[1:3] <- c("ORI", "DES", "FLOW")
  tabflows$KEY <- paste(tabflows$ORI, tabflows$DES, sep = "_")

  tabIndex <- merge(x = tabIndex, y = tabflows[, c("KEY", "FLOW")], by = "KEY", all.x = TRUE)
  colnames(tabIndex)[1:3] <- c("ORI", "DES", "FLOW")
  infoFlowsWide <- dcast(tabIndex, formula = ORI ~ DES, value.var = "FLOW", fun.aggregate = sum, fill = 0, drop = FALSE)
  matFlows <- as.matrix(infoFlowsWide[, -1])
  row.names(matFlows) <- colnames(matFlows)
  return(matFlows)
}


