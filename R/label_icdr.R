#' Label flows as ICDR
#'
#' This function bla bla
#'
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#' @param pol An sf object of the cities
#' @param idpol A character string identifier of cities
#'
#' @return A data.frame of flows as tabflows with ICDR labels
#' @export
#' @importFrom flows prepflows
#' @importFrom cartography getLinkLayer

label_icdr <- function(tabflows, idori, iddes, idflow, pol, idpol){
  matOD <- prepflows(mat = tabflows, i = idori, j = iddes, fij = idflow)
  outDegrees <- apply(matOD, 1, sum)
  inDegrees <- apply(matOD, 2, sum)
  nbOut <- count_hotspot(freqmarg = outDegrees)
  nbIn <- count_hotspot(freqmarg = inDegrees)
  hotspotResid <- detect_hotspot(freqmarg = outDegrees, n = nbOut)
  hotspotLabor <- detect_hotspot(freqmarg = inDegrees, n = nbIn)
  tabflows$HSRESID <- ifelse(tabflows[[idori]] %in% hotspotResid, "HS", "NO")
  tabflows$HSLABOR <- ifelse(tabflows[[iddes]] %in% hotspotLabor, "HS", "NO")
  tabflows$ICDR <- ifelse(tabflows$HSRESID == "HS" & tabflows$HSLABOR == "HS", "Integrated",
                          ifelse(tabflows$HSRESID == "NO" & tabflows$HSLABOR == "HS", "Convergent",
                                 ifelse(tabflows$HSRESID == "HS" & tabflows$HSLABOR == "NO", "Divergent", "Random")))
  tabflows$KEY <- paste(tabflows[[idori]], tabflows[[iddes]], sep = "_")
  links <- getLinkLayer(x = pol, xid = idpol, df = tabflows[, c(idori, iddes)], dfid = c(idori, iddes))
  links$KEY <- paste(links[[idori]], links[[iddes]], sep = "_")
  links <- merge(links, tabflows[, c("KEY", "ICDR")], by.x = "KEY", by.y = "KEY")
  links$KEY <- NULL
  return(links)
}


count_hotspot <- function(freqmarg){
  densitiesOrdered <- sort(freqmarg)
  densitiesOrdered <- densitiesOrdered[densitiesOrdered > 0]
  vF <- c(1:length(densitiesOrdered))
  vL <- c(1:length(densitiesOrdered))
  for (i in 1:length(densitiesOrdered))
  {
    vF[i] <- i/length(densitiesOrdered)
    vL[i] <- sum(densitiesOrdered[1:i])/sum(densitiesOrdered)
  }
  last <- length(vL)
  alpha <- (vL[last] - vL[last-1]) / (vF[last] - vF[last-1])
  Fstar <- (1 - 1/alpha)
  nbHS <- length(vF[vF >= Fstar])
  return(nbHS)
}

detect_hotspot <- function(freqmarg, n)
{
  df <- as.data.frame(freqmarg)
  df$ID <- names(freqmarg)
  bv <- df[order(df$freqmarg, decreasing = TRUE),]
  idHotspot <- bv[1:n, "ID"]
  return (idHotspot)
}
