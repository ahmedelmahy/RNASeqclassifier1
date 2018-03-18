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

#' Get class vector from the rse_gene
#' @param rse_gene an object downloaded from recount
#' @return a character vector of the assigned class
get_class <- function(rse_gene){
  class <- vector()
  di <- dim(colData(rse_gene))[1]
  for (i in 1:di){
    class[i] <- as.character(as.data.frame(strsplit(colData(rse_gene)$characteristics[[i]],":"))[2,4])
  }
  class <- gsub(" ","",class)
  keep_class <- which(class %in% c("ColorectalCancer","HealthyControl"))
  class2 <- class[keep_class]
  return(class2)
}
