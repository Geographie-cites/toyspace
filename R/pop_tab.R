#' Total flows of a DF
#'
#' This function allows you to store the totals of origins, destinations and intrenals flows for each city in a dataframe,
#' from a long format matrix of flows.
#'
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param idori identifiant ori
#' @param iddes identifiant des
#' @param idflow identifiant flux
#' @return A data.frame of totals origins, destinations and internals flows for each city
#'
#' @examples
#' data(tabflows)
#'
#' popTab <- pop_tab(tabflows = tabflows, idori = "ORI", iddes = "DES", idflow = "FLOW")
#'
#' popTab[10:10,]
#'
#' @export
#' @importFrom stats aggregate

pop_tab <- function(tabflows, idori, iddes, idflow){
  tabflowIntra <- tabflows[tabflows[idori] == tabflows[iddes], ]
  tabflowIntra <- aggregate(x = tabflowIntra[[idflow]], by = list(tabflowIntra[[idori]],tabflowIntra[[iddes]]), FUN = sum)
  colnames(tabflowIntra) <- c("ORI", "DES","TOTINTRA")
  tabflowOri <- tabflows[tabflows[idori] != tabflows[iddes], ]
  tabflowOri <- aggregate(x = tabflowOri[[idflow]], by = list(tabflowOri[[idori]]), FUN = sum)
  colnames(tabflowOri) <- c("ORI","TOTORI")
  tabflowDes <- tabflows[tabflows[idori] != tabflows[iddes], ]
  tabflowDes <- aggregate(x = tabflowDes[[idflow]], by = list(tabflowDes[[iddes]]), FUN = sum)
  colnames(tabflowDes) <- c("DES","TOTDES")
  poptab <- merge(x = tabflowIntra, y = tabflowOri, by.x = idori, by.y =idori)
  poptab <- merge(x = poptab, y = tabflowDes, by.x = idori, by.y =iddes)
  poptab[[iddes]] <- NULL
  colnames(poptab) <- c("idflow", "TOTINTRA","TOTORI", "TOTDES")
  return(poptab)
}
