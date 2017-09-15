#' CalcLRpos
#'
#' This function returns the positive likelihood ratio of a binary test 
#' @param zsens numeric; sensitivity of the binary test
#' @param zspec numeric; specificity of the binary test
#' @keywords lrpos
#' @export
#' @return the positive likelihood ratio of a binary tests
#' @examples
#' CalcLRpos(0.2,0.8)
#' 
CalcLRpos <- function(zsens,zspec){
  # Checks for valid input
  stopifnot(zsens >= 0 & zsens <= 1)
  stopifnot(zspec >= 0 & zspec <= 1)
  
  lr_pos <- zsens / (1-zspec)
  return(lr_pos)
}
