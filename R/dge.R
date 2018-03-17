#' Normalize counts matrix with voom
#'
#' @param counts A counts matrix with genes in rows and samples in columns
#' @return voom object
normalize_voom <- function(counts){
  dge <- DGEList(counts = counts)
  dge <- calcNormFactors(dge)
  v <- voom(dge)
  return(v)
}

deg_voom <- function(v,class, keepn = 10){
  design <- model.matrix(~class)
  fit <- lmFit(v, design)
  fit <- eBayes(fit)
  f <- topTable(fit,number = keepn)
  return(rownames(f))
}


