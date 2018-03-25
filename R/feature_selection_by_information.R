#' Stepwise feature selection
#'@param counts matrix
#'@param class vector
#'@param direction of feature selection default is backwards
#'@return a vector of selected genes
filter_step <- function(counts, class, direction = "backward"){
  df <- as.data.frame(t(counts))
  df$class <- as.factor(class)
  fullmod = glm(class ~.,data = df,family=binomial)
  backwards = step(fullmod, direction = direction)
  genes <- names(coef(backwards))[-1]
  return(genes)
}
