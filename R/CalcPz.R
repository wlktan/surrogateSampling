#' CalcPz
#'
#' This function returns the marginal probability of a binary test.
#' @param sens numeric; sensitivity of the binary test
#' @param spec numeric; specificity of the binary test
#' @param prev numeric; prevalence of the finding
#' @keywords pz
#' @export
#' @return the marginal probability of a binary test
#' @examples
#' CalcPz(0.2,0.8,0.13)
#' 
CalcPz <- function(sens,spec,prev){
  # Checks for valid input
  stopifnot(sens >= 0 & sens <= 1)
  stopifnot(spec >= 0 & spec <= 1)
  stopifnot(prev >= 0 & prev <= 1)
  
  pz <- sens*prev + (1-spec)*(1-prev)
  return(pz)
}
