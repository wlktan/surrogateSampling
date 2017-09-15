#' CalcORatio
#'
#' This function returns the o_ratio a surrogate guided sampling design
#' for a binary outcome compared to a random sample 
#' @param zsens numeric; sensitivity of the surrogate Z for the outcome
#' @param zspec numeric; specificity of the surrogate Z for the outcome
#' @param r numeric; ratio of surrogate positives in sample i.e. P(Z=1|S=1)
#' @param pz numeric; prevalence of surrogate in cohort i.e. P(Z=1)
#' @keywords oratio
#' @export
#' @return the Oratio of the sample
#' @examples
#' CalcORatio(0.2,0.8,0.5,0.12)
#'
CalcORatio <- function(zsens, 
                       zspec, 
                       r = 0.5, 
                       pz){
  # Checks for valid input
  stopifnot(zsens >= 0 & zsens <= 1)
  stopifnot(zspec >= 0 & zspec <= 1)
  stopifnot(r >= 0 & r <= 1)
  stopifnot(pz >= 0 & pz <= 1)
  
  numerator <- r*zsens + pz*(1-r-zsens)
  denominator <- r*(1-zspec) + pz*(zspec-r)
  o_ratio <- numerator/denominator
  return(o_ratio)
}
