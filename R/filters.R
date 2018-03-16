filter0 <- function(counts, keepn = 10000){
  r <- rowMeans(counts)
  r <- sort(r,decreasing = TRUE)
  r <- r[1:keepn]
  # subset data
  keep <- which(rownames(counts) %in% r)
  return(counts[keep,])

}
