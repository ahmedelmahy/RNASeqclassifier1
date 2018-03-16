normalize_voom <- function(counts, class){
  design <- model.matrix(~class)
  dge <- DGEList(counts = counts)
  dge <- calcNormFactors(dge)
  v <- voom(dge, design, plot = FALSE)
  return(v)
}

deg_voom <- function(v, keepn = 10){
  fit <- lmFit(v, design)
  fit <- eBayes(fit)
  f <- topTable(fit,number = keepn)
  return(rownames(f))
}
