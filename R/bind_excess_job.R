#' Bind excess communting partial tables
#'
#' This function allows you to create a distance cost matrix
#'
#' @param tabflows A long table of flows
#' @param matcost A squared matrix of cost
#' @param idpol A squared matrix of cost
#' @param idori A squared matrix of cost
#' @param iddes A squared matrix of cost
#' @param idflow A squared matrix of cost
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

bind_excess_job <- function(tabflows, matcost, idpol, idori, iddes, idflow, variable, modal){
  tabToFill <- data.frame(ORI = vector(), DES = vector(), FLOW = vector(), CSP = vector())
  for(i in 1:length(modal)){
    tempFlows <- tabflows[tabflows[[variable]] == modal[[i]], ]
    matFlowsPart <- cast_tabflows(idpol = idpol,
                                  tabflows = tempFlows,
                                  idori = idori,
                                  iddes = iddes,
                                  idflow = idflow)
    matFlowsPartMin <- excess_commuting(matflows = matFlowsPart, matcost = matcost)
    tabFlowsPart <- melt(matFlowsPartMin, as.is = TRUE)
    colnames(tabFlowsPart) <- c("ORI", "DES", "FLOW")
    tabFlowsPart$CSP <- modal[[i]]
    tabToFill <- bind_rows(tabToFill, tabFlowsPart) %>%
      filter(FLOW > 0)
  }

  return(tabToFill)
}
