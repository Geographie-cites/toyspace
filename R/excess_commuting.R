#' Excess commuting
#'
#' This function allows you to create a distance cost matrix
#'
#' @param matflows A squared matrix of flows
#' @param matcost A squared matrix of cost
#'
#' @return A squared matrix of flows
#'
#'
#'
#' @export
#' @importFrom reshape2 dcast

excess_commuting <- function(matflows, matcost){
  if(nrow(matflows) == ncol(matflows) & nrow(matcost) == ncol(matcost) & nrow(matflows) == nrow(matcost)){
    n = nrow(matflows)
  } else {
    stop("Check the matrix size (square matrices of equal size are required)")
  }
  matflows[is.na(matflows)] <- 0
  matflows <- round(matflows)
  lpResult <- transport::transport(a = apply(matflows, 1, sum), b = apply(matflows, 2, sum), costm = matcost)

  lpResult$from <- factor(x = lpResult$from, levels = 1:nrow(matflows), labels = 1:nrow(matflows))
  lpResult$to <- factor(x = lpResult$to, levels = 1:nrow(matflows), labels = 1:nrow(matflows))
  lpWide <- dcast(data = lpResult, formula = from ~ to, fill = 0, drop = FALSE, value.var = "mass")
  matMin <- as.matrix(lpWide[, -1])
  row.names(matMin) <- colnames(matMin) <- row.names(matflows)

  return(matMin)
}
