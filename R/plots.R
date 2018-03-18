#' Plot top four principle components in a counts matrix colored with a class
#' variable, useful to see how well a gene subset of a counts matrix can classify
#' the data
#'
#' @param counts A counts matrix
#' @param class A vector of assigned class for each sample e.g. "disease" or
#' "control"
#' @param multiple If true plots PC1~PC2 from multiple subsets of the data
#' hoping that the PCA segregation improves at each subset.
#' @return A grid of ggplot2 plots
pca <- function(counts, class, multiple = FALSE, ...){
  df_mat <- as.data.frame(t(counts))
  df_pca <- prcomp(df_mat)
  rm(df_mat)
  if (multiple == TRUE){

  } else{
  df_pca_x <- as.data.frame(df_pca$x)
  p <- ggplot(df_pca_x)
  p1 <- p + geom_point(aes(x = PC1,y= PC2, col = class),
                       size = .5)
  p2 <- p + geom_point(aes(x = PC3,y= PC1, col = class),
                       size = .5)
  p3 <- p + geom_point(aes(x = PC3,y= PC2, col = class),
                       size = .5)
  p4 <- p + geom_point(aes(x = PC4,y= PC1, col = class),
                       size = .5)
  p5 <- p + geom_point(aes(x = PC4,y= PC2, col = class),
                       size = .5)
  p6 <- p + geom_point(aes(x = PC4,y= PC3, col = class),
                       size = .5)

  gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6)
}}
