#' download an Rdata object from the recount2 project to be extended to
#' other public RNASeq datasets
#'
#' @param id Id of the study to be downloaded and analysed
#' @return rse_gene An RangedSummarizedExperiment object
#' @example
load_data <- function(id = "SRP042228"){
  url <- paste0("http://duffel.rail.bio/recount/v2/",id,"/rse_gene.Rdata")
  system(paste0("wget ", url))
  load("rse_gene.Rdata")
  return(rse_gene)
}
