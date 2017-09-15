
#' Expit
#'
#' This function returns the expit of a number
#' @param x numeric; number to take the expit of
#' @keywords expit
#' @return the expit of x
#' @export

Expit <- function(x){
  return(exp(x)/(1+exp(x)))
}
