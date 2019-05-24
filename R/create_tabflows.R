#' Create an O/D long table
#'
#' This function allows you to create an Origin/Destination matrix (in a long table format) from a table of individuals.
#' It may deal with weighted individuals and filter these individuals
#'
#' @param tabindiv A data.frame of individuals between origins and destinations (long format matrix containing, at least, origins, destinations for each individuals)
#' @param idori A character string giving the origin field name in tabindiv
#' @param iddes A character string giving the destination field name in tabindiv
#' @param varwgt Default to NULL ; a character string giving the weight field name in tabindiv
#' @param variable Default to NULL ; a character string giving the name of the field in tabindiv in wich selected label will be filtered
#' @param label Default to NULL ; a character string giving the value of the variable to be filtered (or keeped)
#'
#' @return A data.frame of three column : ORI, DES, WGT
#'
#' @examples
#' # Import data
#' data(tabindiv)
#' idori <- "ORI"
#' iddes <- "DES"
#'
#' tabflows <- create_tabflows(tabindiv, idori, iddes)
#'
#' tabflows[1:10,]
#'
#' # we will now weight individuals with the column "WGT"
#' varwgt <- "WGT"
#'
#' tabflows <- create_tabflows(tabindiv, idori, iddes, varwgt)
#'
#' tabflows[1:10,]
#'
#' # now we will only extract the values where the SCP is equal to "3"
#' variable <- "SCP"
#' label <- "3"
#'
#' tabflowsW <- create_tabflows(tabindiv, idori, iddes, varwgt, variable, label)
#'
#' tabflowsW[1:10,]
#'
#' @export
#'


create_tabflows <- function(tabindiv, idori, iddes, varwgt = NULL, variable = NULL, label = NULL){
  # rename variables
  tabindiv$ORI <- tabindiv[[idori]]
  tabindiv$DES <- tabindiv[[iddes]]
  # get weights
  if(!is.null(varwgt)){
    tabindiv$WGT <- tabindiv[[varwgt]]
  } else {
    tabindiv$WGT <- 1
  }
  # extract selection
  if(!is.null(variable) & !is.null(label)){
    tabindiv <- tabindiv[tabindiv[, variable] == label, ]
  }
  # group by origin and destination
  tabFlows <- aggregate(x = tabindiv$WGT, by = list(tabindiv$ORI, tabindiv$DES), FUN = sum)
  colnames(tabFlows) <- c("ORI", "DES", "FLOW")

  return(tabFlows)
}


