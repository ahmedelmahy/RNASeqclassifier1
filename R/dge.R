#' Normalize counts matrix with voom
#'
#' @param counts A counts matrix with genes in rows and samples in columns
#' @return voom object
normalize_voom <- function(counts){
  dge <- DGEList(counts = counts)
  dge <- calcNormFactors(dge)
  v <- voom(dge)
  return(v)
  return(v)
}

#' Select diffentially expressed genes(DGEs) from a voom object
#'
#' @param v A voom object
#' @param class A vector of assigned class for each sample e.g. "disease" or
#' "control"
#' @param keepn Number of DEGs to return
#' @return A vector of selected DEGs
deg_voom <- function(v,class, keepn = 10){
  design <- model.matrix(~class)
  fit <- lmFit(v, design)
  fit <- eBayes(fit)
  f <- topTable(fit,number = keepn)
  return(rownames(f))
}


