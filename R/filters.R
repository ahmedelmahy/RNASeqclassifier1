filter0 <- function(counts, class){
  r <- rowMeans(counts)
  r <- sort(r,decreasing = TRUE)
  r <- r[1:10000]

  # subset data
  keep <- which(rownames(counts) %in% r)
}
