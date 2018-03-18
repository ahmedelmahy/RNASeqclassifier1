#' Basic filter which just removes very low expressed genes or genes that are
#' not expressed at all
#'
#' @param counts A counts matrix
#' @param keepn Number of genes to keep
#'
#' @return A subset of the counts matrix with the selected genes
filter0 <- function(counts, keepn = 10000){
  r <- rowMeans(counts)
  r <- sort(r,decreasing = TRUE)
  r <- r[1:keepn]
  # subset data
  keep <- which(rownames(counts) %in% names(r))
  return(counts[keep,])
}

#' filter genes by linear correlation with the class variable
#' @param mat a train matrix
#' @param class a class vector
#' @param keepn Number of genes to keep
#' @return A subset of the counts matrix with the selected genes
filter_linear_correlation <- function(mat,class, keepn = 2000){
  s <- as.data.frame(mat)
  s$class <- ifelse(class == class[1] ,0,1)
  f <- FSelector::linear.correlation(class ~., s)
  f$genes = rownames(f)
  f <- f[order(f$attr_importance,decreasing = TRUE),]

  return(f$genes[1:keepn])
}

