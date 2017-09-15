
#' Logit
#'
#' This function returns the logit of a number
#' @param y numeric; number to take the logit of
#' @keywords logit
#' @export
#' @return the logit of y
#' @examples
#' Logit()
Logit <- function(y){ 
  return(log(y/(1-y))) 
}
