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


