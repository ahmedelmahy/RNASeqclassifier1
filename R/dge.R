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

#' Normalize counts matrix with DESeq2
#'
#' @inheritParams normalize_voom
#' @param class A vector of assigned class for each sample e.g. "disease" or
#' "control"
#' @return DESeqDataSet object
normalize_deseq2 <- function(counts, class){
  DESeq_Data<- DESeqDataSetFromMatrix(countData = counts,colData= data.frame(class),design = ~ class)
  DESeq_Data <- DESeq(DESeq_Data,parallel = TRUE)
  return(DESeq_Data)
}


#' Select diffentially expressed genes(DGEs) from a DESeqDataSet object
#'
#' @param ddsDESeqObject A DESeqDataSet object
#' @param padj A upper threshold for p adjusted value
#' @param logfc_i A lower threshold for log fold change
#' @return A vector of selected DEGs
deg_deseq2 <- function(ddsDESeqObject, padj_i = .01, logfc_i = 2){
  res <- results(ddsDESeqObject)
  res.sel <- res [which(res$padj<padj_i &abs(res$log2FoldChange)>=logfc_i), ]
  return(rownames(res.sel))
}


