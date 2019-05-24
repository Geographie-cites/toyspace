#' Create an O/D square matrix
#'
#' This function allows you to create an Origin/Destination squared matrix from a Origin/Destination long format matrix
#'
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, 3 column : origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#'
#' @return A squared Origins (rows) and Destination (columns) matrix of flows
#'
#' @examples
#' # Import data
#' data(tabflows)
#' idori <- "ORI"
#' iddes <- "DES"
#' idflow <- "FLOW"
#'
#'
#' matFlows <- cast_tabflows(tabflows,idori,iddes,idflow)
#'
#' matFlows[1:10,1:10]
#'
#' @export
#' @importFrom reshape2 dcast


cast_tabflows <- function(tabflows, idori,iddes,idflow){
  tabflows <- tabflows[c(idori,iddes,idflow)]
  id <- unique(tabflows[[idori]])
  tabIndex <- expand.grid(ORI = id,
                          DES = id,
                          stringsAsFactors = FALSE)

  print("Warning! Columns interpretation: 1-ORI, 2-DES, 3-FLOW")
  colnames(tabflows)[1:3] <- c("ORI", "DES", "FLOW")
  tabIndex <- merge(x = tabIndex, y = tabflows, by.x = "ORI", by.y = "DES")
  tabIndex$DES <- NULL
  colnames(tabIndex)[1:3] <- c("ORI", "DES", "FLOW")
  infoFlowsWide <- dcast(tabIndex, formula = ORI ~ DES, value.var = "FLOW", fun.aggregate = sum, fill = 0, drop = FALSE)
  matFlows <- as.matrix(infoFlowsWide[, -1])
  row.names(matFlows) <- colnames(matFlows)
  return(matFlows)
}


