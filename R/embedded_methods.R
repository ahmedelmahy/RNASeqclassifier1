#' Eliminate features with glmnet
#'
#' @param x train dataframe
#' @param y train class
#' @param keepn Number of genes to keep
#' @param fitControl a configuration list for caret package
#' @return a vector of selected genes
eliminate_glm <- function(x,y, keepn = 100, fitControl){
  fit_tune <- train(x = x,
                    y = y,
                    method = "glmnet",
                    trControl = fitControl)

  fit <- caret::train(x = x,
                      y = y,
                      method ="glmnet",
                      tuneGrid=expand.grid(.alpha = fit_tune$bestTune$alpha,
                                           .lambda = fit_tune$bestTune$lambda),
                      trControl = fitControl)
  var_imp <- varImp(fit) # random forest
  var_imp <- data.frame(cbind(row.names(var_imp$importance),
                              var_imp$importance[, 1]))
  names(var_imp) <- c("genes", "imp")
  var_imp <- var_imp[order(as.numeric(as.matrix(var_imp$imp)),
                           decreasing = T), ]
  topgenes <- as.character(var_imp$genes[1:keepn])
  return(topgenes)
}





