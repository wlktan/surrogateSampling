#' CalcLRneg
#'
#' This function returns the negative likelihood ratio of a binary test 
#' @param zsens numeric; sensitivity of the binary test
#' @param zspec numeric; specificity of the binary test
#' @keywords lrpos
#' @export
#' @return the negative likelihood ratio of a binary tests
#' @examples
#' CalcLRneg(0.2,0.8)
#' 
CalcLRneg <- function(zsens,zspec) {
  # Checks for valid input
  stopifnot(zsens >= 0 & zsens <= 1)
  stopifnot(zspec >= 0 & zspec <= 1)
  
  lr_neg <- zspec / (1-zsens)
  return(lr_neg)
} 