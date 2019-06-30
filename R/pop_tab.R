#' Total flows of a DF
#'
#' This function computes the margins of the OD matrix: emissions, attractions, intraflows for each spatial unit
#' from a long format matrix of flows.
#'
#' @param tabflows A data.frame of flows (long format: origin, destination, flow)
#' @param idori identifiant ori
#' @param iddes identifiant des
#' @param idflow identifiant flux
#' @param iddist identifiant dist
#' @return A data.frame of totals origins, destinations, internals flows and sum of distance for origins and destination of each city
#'
#' @examples
#' data(tabflows)
#'
#' popTab <- pop_tab(tabflows = tabflows, idori = "ORI",
#' iddes = "DES", idflow = "FLOW", iddist = "DIST")
#'
#' popTab[10:10,]
#'
#' @export
#' @importFrom stats aggregate


pop_tab <- function(tabflows, idori, iddes, idflow, iddist){
  tabflowIntra <- tabflows[tabflows[idori] == tabflows[iddes], ]
  tabflowIntra <- aggregate(x = tabflowIntra[[idflow]], by = list(tabflowIntra[[idori]],tabflowIntra[[iddes]]), FUN = sum)
  colnames(tabflowIntra) <- c("ORI", "DES","TOTINTRA")
  tabflowOri <- tabflows[tabflows[idori] != tabflows[iddes], ]
  tabflowOri <- aggregate(x = tabflowOri[[idflow]], by = list(tabflowOri[[idori]]), FUN = sum)
  colnames(tabflowOri) <- c("ORI","TOTORI")
  tabflowDes <- tabflows[tabflows[idori] != tabflows[iddes], ]
  tabflowDes <- aggregate(x = tabflowDes[[idflow]], by = list(tabflowDes[[iddes]]), FUN = sum)
  colnames(tabflowDes) <- c("DES","TOTDES")
  tabflowDistOri <- aggregate(x = tabflows[[iddist]] , by = list(tabflows[[idori]]), FUN = sum)
  colnames(tabflowDistOri) <- c("ORI","DISTORI")
  tabflowDistDes <- aggregate(x = tabflows[[iddist]], by = list(tabflows[[iddes]]), FUN = sum)
  colnames(tabflowDistDes) <- c("DES","DISTDES")
  poptab <- merge(x = tabflowIntra, y = tabflowOri, by.x = "ORI", by.y ="ORI", all.x = TRUE, all.y = TRUE)
  poptab <- merge(x = poptab, y = tabflowDes, by.x = "ORI", by.y ="DES", all.x = TRUE, all.y = TRUE)
  poptab <- merge(x = poptab, y = tabflowDistOri, by.x = "ORI", by.y ="ORI", all.x = TRUE, all.y = TRUE)
  poptab <- merge(x = poptab, y = tabflowDistDes, by.x = "ORI", by.y ="DES", all.x = TRUE, all.y = TRUE)
  poptab$TOTORIDES <- poptab$TOTORI + poptab$TOTDES
  poptab[is.na(poptab)] <- 0
  poptab[["DES"]] <- NULL
  colnames(poptab) <- c("CODGEO", "TOTINTRA","TOTORI", "TOTDES","DISTORI","DISTDES","TOTORIDES")
  return(poptab)
}
