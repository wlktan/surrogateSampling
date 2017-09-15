#' CalcTrapezoidalAUC
#'
#' This function returns the auc of a binary test 
#' according to the trapezoidal rule.
#' @param sens numeric; sensitivity of the binary test
#' @param spec numeric; specificity of the binary test
#' @keywords auc
#' @export
#' @return the area under the ROC curve of the binary test for the binary outcome
#' @examples
#' CalcTrapezoidalAUC(0.2,0.8)
#' 
CalcTrapezoidalAUC <- function(sens,spec){
  # Checks for valid input
  stopifnot(sens >= 0 & sens <= 1)
  stopifnot(spec >= 0 & spec <= 1)
  
  ## trapezoidal rule to get AUC of binary test
  auc <- 1/2*(1-spec)*(sens) + (spec)*(sens+1)/2
  return(auc)
}