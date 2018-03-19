
fit_svmRadial <- function(x, y, fitControl){
  fit_tune <- train(x = x,
                    y = y,
                    method = "svmRadial",
                    trControl = fitControl)

  fit <- train(x = x,
               y = y,
               method = "svmRadial",
               trControl = fitControl,
               tuneGrid=expand.grid(.sigma = fit_tune$bestTune$sigma,
                                    .C=fit_tune$bestTune$C),
               metric = "ROC")
  return(fit)
}


fit_svmLinear <- function(x, y, fitControl){
  fit_tune <- train(x = x,
                    y = y,
                    method = "svmLinear",
                    trControl = fitControl)

  fit <- train(x = x,
               y = y,
               method = "svmLinear",
               trControl = fitControl,
               tuneGrid=expand.grid(.C=fit_tune$bestTune$C),
               metric = "ROC")
  return(fit)
}


fit_glm <- function(x,y, fitControl){
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
  return(fit)
}

fit_rf <- function(x,y, fitControl){
  fit_tune <- train(x = x,
                    y = y,
                    method = "rf",
                    trControl = fitControl)

  fit <- caret::train(x = x,
                      y = y,
                      method ="rf",
                      tuneGrid=expand.grid(.mtry=fit_tune$bestTune$mtry),
                      trControl = fitControl)
  return(fit)
}




