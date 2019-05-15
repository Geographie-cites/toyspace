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
#' 1 + 1
#'
#' @export
#' @importFrom dplyr summarise filter group_by left_join
#' @importFrom magrittr %>%

pop_tab <- function(tabflows, idori, iddes, idflow){
  tabflows$ORI <- tabflows[[idori]]
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  tabflowOriOri <- tabflows %>% filter(ORI == DES) %>% group_by(ORI,DES) %>% summarise(TOTINTRA = sum(FLOW))
  tabflowOri <-  tabflows %>% filter(ORI != DES) %>% group_by(ORI) %>% summarise(TOTORI = sum(FLOW))
  tabflowDes <-  tabflows %>% filter(ORI != DES) %>% group_by(DES) %>% summarise(TOTDES = sum(FLOW))
  poptab <- left_join(x = tabflowOriOri, y = tabflowOri, by = "ORI")
  poptab <- left_join(x = poptab, y = tabflowDes, by = "DES")
  poptab$DES <- NULL
  colnames(poptab) <- c("idflow", "TOTINTRA","TOTORI", "TOTDES")
  return(poptab)
}
