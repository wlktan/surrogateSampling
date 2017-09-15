#' CalcNPV
#'
#' This function returns the negative predicted value of a binary test
#' @param sens numeric; sensitivity of the binary test
#' @param spec numeric; specificity of the binary test
#' @param prev numeric; prevalence of the finding
#' @keywords npv
#' @export
#' @return the negative predicted value of a binary test
#' @examples
#' CalcNPV(0.2,0.8,0.13)

CalcNPV <- function(sens,spec,prev){ 
  # Checks for valid input
  stopifnot(sens >= 0 & sens <= 1)
  stopifnot(spec >= 0 & spec <= 1)
  stopifnot(prev >= 0 & prev <= 1)
  
  npv <- (spec*(1-prev))/((1-sens)*prev + (spec)*(1-prev))
  return(npv)
}
