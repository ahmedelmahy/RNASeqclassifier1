split_counts <- function(counts, class){
  nr <- 1:ncol(counts)
  nr_t <- as.integer(ncol(counts) * .8)
  s <- sample(nr, nr_t)
  train <- counts[,s]
  train_class <- class[s]
  test <- counts[,-s]
  test_class <- class[-s]
  rm(nr, nr_t,s)
  return(list(train = train, train_class = train_class,
              test = test, test_class = test_class))
}

config_caret <- function(...){
  doMC::registerDoMC(cores = 15)
  fitControl <- caret::trainControl(method = "cv",
                                    classProbs = T,
                                    savePredictions = T,
                                    verboseIter = TRUE,
                                    sampling = "smote",
                                    allowParallel = TRUE,...)
  return(fitControl)
}
