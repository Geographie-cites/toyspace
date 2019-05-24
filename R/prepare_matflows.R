#' O/D Matrix from individuals
#'
#' This function allows you to create an O/D matrix from a table of individuals.
#' It may deal with weighted individuals and filter these individuals
#'
#' @param tabindiv A data.frame of individuals between origins and destinations (long format matrix containing, at least, origins, destinations for each individuals)
#' @param idori A character string giving the origin field name in tabindiv
#' @param iddes A character string giving the destination field name in tabindiv
#' @param varwgt Default to NULL ; a character string giving the weight field name in tabindiv
#' @param variable Default to NULL ; a character string giving the name of the field in tabindiv in wich selected label will be filtered
#' @param label Default to NULL ; a character string giving the value of the variable to be filtered (or keeped)
#'
#' @return A squared matrix of flows
#'
#' @examples
#' # Import data
#' data(tabindiv)
#' idori <- "ORI"
#' iddes <- "DES"
#'
#'
#' matFlows <- prepare_matflows(tabindiv, idori, iddes)
#'
#' matFlows[1:10,]
#'
#' # we will now weight individuals with the column "WGT"
#' varwgt <- "WGT"
#'
#' matFlows <- prepare_matflows(tabindiv, idori, iddes, varwgt)
#'
#' matFlows[1:10,]
#'
#' # now we will only extract the values where the SCP is equal to "3"
#' variable <- "SCP"
#' label <- "3"
#'
#' matFlowsW <- prepare_matflows(tabindiv, idori, iddes, varwgt, variable, label)
#'
#' matFlowsW[1:10,]
#'
#' @export
#' @importFrom reshape2 dcast

prepare_matflows <- function(tabindiv, idori, iddes, varwgt = NULL, variable = NULL, label = NULL){
  tabflows <- create_tabflows(tabindiv, idori, iddes, varwgt = varwgt, variable = variable, label = label)
  matFlows <- cast_tabflows(tabflows, "ORI","DES", "FLOW")
  matFlows <- round(matFlows, digits = 0)
  mode(matFlows) <- "integer"
  return(matFlows)
}


