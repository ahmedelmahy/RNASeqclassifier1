pca <- function(counts, class){
  df_mat <- as.data.frame(t(counts))
  df_pca <- prcomp(df_mat)
  rm(df_mat)

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
}
