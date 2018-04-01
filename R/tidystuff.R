#' Clean a class vector by keeping only alphabetical letters
#' to avoid errors when we use the class as a y variable in
#' ml models
#' @param class A class vector
#' @return vector A tidy version of the class vector
tidyClass <- function(class){
  return(gsub("[^a-zA-Z]","",class))
}
