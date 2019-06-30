# CBDsation ----

#' DF transform to CBD urban model
#'
#' This function allows you to change the origins and destination of flows
#' stored into a dataframe to simulate a Central Business District urban model. City considered as
#' the main city of the region is designed as candidate and flows (destinations only) are moved
#' from non-candidate cities to the candidate city. The origins are moved into from the candidate city to the non-candidate city,
#' so that every jobs are in the main city, and workers in the suburbs.
#'
#' @param pol An sf object of the cities
#' @param idpol A character string of the column containing the id of the pol object
#' @param cand A character string of the column containing binary (1, 0) candidate value of the pol object (main city must be equal to 1 and 0 for the rest)
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#'
#' @return A data.frame of flows as tabflows with origins and destinations changed
#'
#' @export
#' @importFrom dplyr group_by filter summarise %>% left_join mutate ungroup transmute
#' @importFrom sf st_set_geometry


cbd_city <- function(pol, idpol, cand, tabflows, idori, iddes, idflow){
  tabflows$ORI <- tabflows[[idori]]
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  pol <- pol %>% st_set_geometry(NULL)
  pol$ID <- pol[[idpol]]
  pol$CAND <- pol[[cand]]

  # compute proportion of jobs and proportion of labor force
  totDesIn <- tabflows %>%
    left_join(pol[, c("ID", "CAND")], by = c("DES" = "ID")) %>%
    filter(CAND == 1) %>%
    group_by(DES) %>%
    summarise(FLOW = sum(FLOW)) %>%
    mutate(PCTFLOW = FLOW / sum(FLOW)) %>%
    ungroup()

  totOriOut <- tabflows %>%
    left_join(pol[, c("ID", "CAND")], by = c("ORI" = "ID")) %>%
    filter(CAND != 1) %>%
    group_by(ORI) %>%
    summarise(FLOW = sum(FLOW)) %>%
    mutate(PCTFLOW = FLOW / sum(FLOW)) %>%
    ungroup()

  # re-affect jobs
  tabFlowsSub <- tabflows %>%
    left_join(pol[, c("ID", "CAND")], by = c("DES" = "ID")) %>%
    filter(CAND != 1)
  matPctIn <- sapply(tabFlowsSub$FLOW, function(x) x * totDesIn$PCTFLOW) %>% t()
  row.names(matPctIn) <- paste(tabFlowsSub$ORI, tabFlowsSub$MODE, sep = "_")
  colnames(matPctIn) <- totDesIn$DES
  tabFlowsIn <- melt(matPctIn, varnames = c("ORIMODE", "DES"), value.name = "FLOW", as.is = TRUE) %>%
    mutate(ORI = substr(ORIMODE, 1, 5), MODE = substr(ORIMODE, 7, 8)) %>%
    group_by(ORI, DES, MODE) %>%
    summarise(FLOW = sum(FLOW)) %>%
    ungroup()

  tabFlowsCand <- tabflows %>%
    left_join(pol[, c("ID", "CAND")], by = c("DES" = "ID")) %>%
    filter(CAND == 1) %>%
    transmute(ORI = ORI, DES = DES, MODE = substr(MODE, 1, 2), FLOW = FLOW)

  jobsRelocated <- rbind(tabFlowsIn, tabFlowsCand)

  # re-affect labor force
  tabFlowsCbd <- jobsRelocated %>%
    left_join(pol[, c("ID", "CAND")], by = c("ORI" = "ID")) %>%
    filter(CAND == 1)
  matPctOut <- sapply(tabFlowsCbd$FLOW, function(x) x * totOriOut$PCTFLOW) %>% t()

  row.names(matPctOut) <- paste(tabFlowsCbd$DES, tabFlowsCbd$MODE, sep = "_")
  colnames(matPctOut) <- totOriOut$ORI
  tabFlowsOut <- melt(matPctOut, varnames = c("DESMODE", "ORI"), value.name = "FLOW", as.is = TRUE) %>%
    mutate(DES = substr(DESMODE, 1, 5), MODE = substr(DESMODE, 7, 8)) %>%
    group_by(ORI, DES, MODE) %>%
    summarise(FLOW = sum(FLOW)) %>%
    ungroup()

  tabFlowsNocbd <- jobsRelocated %>%
    left_join(pol[, c("ID", "CAND")], by = c("ORI" = "ID")) %>%
    filter(CAND != 1) %>%
    transmute(ORI = ORI, DES = DES, MODE = substr(MODE, 1, 2), FLOW = FLOW)

  allRelocated <- rbind(tabFlowsOut, tabFlowsNocbd)

  return(allRelocated)
}
