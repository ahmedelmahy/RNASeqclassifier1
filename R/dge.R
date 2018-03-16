normalize_voom <- function(counts, class){
  design <- model.matrix(~class)
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
