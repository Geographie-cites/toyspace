#' Bind excess communting partial tables
#'
#' This function allows you to create a distance cost matrix
#'
#' @param tabflows A long table of flows
#' @param matcost A squared matrix of cost
#' @param idspat A squared matrix of cost
#' @param varori A squared matrix of cost
#' @param vardes A squared matrix of cost
#' @param variable A squared matrix of cost
#' @param modal A squared matrix of cost
#'
#' @return A squared matrix of flows
#'
#' @examples
#' # Import data
#'
#'
#' @export
#'

# Bind partial minimal matrices ----

bind_excess <- function(tabflows, tabcost, idpol, idori, iddes, idflow, iddist, variable, modal){
  matToFill <- matrix(data = rep(0, times = length(matcost)), nrow = nrow(matcost), ncol = ncol(matcost))
  matCost <- cost_matrix(tabcost = tabcost, idpol = idpol, idori = idori, iddes = iddes, iddist = iddist)
  for(i in 1:length(modal)){
    tempFlows <- tabflows[tabflows[[variable]] == modal[[i]], ]
    matFlowsPart <- cast_tabflows(idpol = idpol,
                                  tabflows = tempFlows,
                                  idori = idori,
                                  iddes = iddes,
                                  idflow = idflow)
    matFlowsPartMin <- excess_commuting(matflows = matFlowsPart, matcost = matcost)
    matToFill <- matToFill + matFlowsPartMin
  }
  row.names(matToFill) <- colnames(matToFill) <- colnames(matFlowsPartMin)
  return(matToFill)
}
