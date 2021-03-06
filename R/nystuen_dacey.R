#' Dominant flows
#'
#' This function selects the flows to be keeped in a large matrix of flows responding to the Nystuen & Dacey's dominants flows criterion.
#'
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, 3 column : origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#' @param weight A character string, chosen between, "destination", "origin" or "sum" to weigths the flows
#' @param threspct A threshold (see 'Details')
#' @param pol An sf object of the cities
#' @param idpol A character string identifier of cities
#' @param centPol A character string identifier of cities
#' @param idCentPol A character string identifier of cities
#' @param poptab A character string identifier of cities
#'
#' This function selects which flow (fij or fji) must be kept. If the ratio weight of destination (wj) / weight of origin (wi)
#' is greater than the treshold, then fij is selected and fji is not. This function can perform the second criterion of the Nystuen & Dacey's dominants flows analysis.
#' As the output is a boolean matrix, use element-wise multiplication to get flows intensity.
#'
#' @return A boolean matrix of selected flows
#'
#' @export
#' @importFrom cartography getLinkLayer
#' @importFrom reshape2 melt
#' @importFrom flows prepflows domflows firstflows
#' @importFrom sf st_centroid st_geometry


nystuen_dacey <- function(tabflows, idori, iddes, idflow, weight, threspct, pol, idpol, centPol, idCentPol, poptab){

  tabflows$ORI <- tabflows[[idori]]
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]

  # weight choices between "destination", "origin", "sum"
  mat <- prepflows(tabflows, idori, iddes, idflow)
  if(weight=="destination"){
    w <- colSums(mat)
  } else if (weight=="origin"){
    w <- rowSums(mat)
  } else if (weight=="sum"){
    w <- colSums(mat)+rowSums(mat)
  }
  diag(mat) <- 0
  firstflows <- firstflows(mat = mat, method = "nfirst", k = threspct)
  domflows <- domflows(mat = mat, w = w, k = threspct)
  flowDom <- mat * firstflows * domflows
  flowDomWide <- melt(data = flowDom, varnames = c("ORI", "DES"), value.name = "FLOW", as.is = TRUE)
  flowDomWide <- flowDomWide[flowDomWide$FLOW > 0,]
  flowDomWide$KEY <- paste(flowDomWide[["ORI"]], flowDomWide[["DES"]], sep = "_")
  spLinks <- getLinkLayer(x = pol, xid = idpol, df = flowDomWide[, c("ORI", "DES")], dfid = c("ORI", "DES"))
  spLinks$KEY <- paste(spLinks[["ORI"]], spLinks[["DES"]], sep = "_")
  pointFlow <- merge(x = centPol, y = poptab, by.x = idCentPol, by.y = "CODGEO")
  tabflowOri <- aggregate(x = tabflows[[idflow]], by = list(tabflows[[idori]]), FUN = sum)
  colnames(tabflowOri) <- c("ORI","TOTORI")
  fdom1 <- melt(flowDom)
  names(fdom1) <- c("i", "j", "fij")
  fdom1 <- fdom1[fdom1$fij > 0, ]
  fdom1 <- merge(fdom1, tabflowOri, by.x = "i", by.y = "ORI")
  pointFlow$status <- ""
  pointFlow[pointFlow[[idpol]] %in% fdom1$j & !pointFlow[[idpol]] %in% fdom1$i, "status"] <- 1
  pointFlow[pointFlow[[idpol]] %in% fdom1$j & pointFlow[[idpol]] %in% fdom1$i, "status"] <- 2
  pointFlow[!pointFlow[[idpol]] %in% fdom1$j & pointFlow[[idpol]] %in% fdom1$i, "status"] <- 3
  pointFlow <- pointFlow[pointFlow$status != "", ]
  pointFlow$status <- as.numeric(pointFlow$status)
  return(list( PTS = pointFlow, FLOWS = spLinks))
}
