#' CalcOneAucVal
#'
#' Runs a K-fold cross-validation with weighted bootstrap
#' to approximate training/testing error from an annotated dataset
#' @param test.df Initial test df
#' @param n_val Number of validation samples
#' @param stratify_var Surrogate name in quo(.)
#' @param nz1 Number of z=1 in the weighted bootstrap sample
#' @param nz0 Number of z=0 in the weighted bootstrap sample
#' @param nlp.model The fitted model
#' @keywords cross validation, weighted bootstrap
#' @export
#' @return Estimated value of validation AUC based on a single
#' weighted bootstrap sample
#'
CalcOneAucVal <- function(test.df,
                          n_val,
                          stratify_val,
                          nz1,
                          nz0,
                          nlp.model){
  one.val.df <- SurrogateGuidedSample(test.df,
                                      n_sample = n_val,
                                      stratify_var,
                                      nz1,
                                      nz0,
                                      REPLACE = TRUE)
  one_auc_val <- ValidateModel(nlp.model$train.model,
                               one.val.df,
                               Y = "Y")$auc_val

  return(one_auc_val)

}
