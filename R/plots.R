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


#' plot the counts of the selected genes though samples colored by class
#'
#' @param df A subset of the data with selected genes as columns
#' @param class A vector of the class
compare_selected_features_barplot <- function(df, class){
  df <- as.data.frame(df)
  df$class <- class
  df <- df[order(df$class),]
  df$samplenum <- c(1: dim(df)[1])
  df <-melt(df,id = c("samplenum","class"))

  p1 <- ggplot(df, aes(samplenum, value, fill=class)) +
    geom_col() +
    facet_grid(variable~.)

  print(p1)
}

#' plot ROC curves from all models
#' @param model_title_list a list of caret fit models named
#' with the name of the model
#' @param A list of caret models
plot_multipe_rocs <- function(model_title_list){
  results_list_roc <- list(NA)
  for(i in 1:length(model_title_list)){
    myRoc_on_train_data <- roc(predictor = model_title_list[[i]]$pred[[3]],
                               response = model_title_list[[i]]$pred$obs)
    message(auc(myRoc_on_train_data))
    results_list_roc[[i]] <- data_frame(
      True_positive_rate = myRoc_on_train_data$sensitivities,
      False_positive_rate = 1 - myRoc_on_train_data$specificities,
      modell = model_title_list[[i]]$modelInfo$label)
  }
  results_df_roc <- bind_rows(results_list_roc)

  ggplot(aes(x = False_positive_rate,  y = True_positive_rate, group = modell), data = results_df_roc) +
    geom_line(aes(color = modell), size = 1) +
    geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
    theme_bw(base_size = 18)
}
