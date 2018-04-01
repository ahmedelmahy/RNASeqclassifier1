#' Clean a class vector by keeping only alphabetical letters
#' to avoid errors when we use the class as a y variable in
#' ml models
#' @param class
tidyClass <- function(class){
  return(gsub("[^a-zA-Z]","",class))
}
