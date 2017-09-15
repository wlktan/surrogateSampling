#' ValidateModel
#'
#' Makes a sample enriched for cases using ideas from
#' surrogate guided sampling strategy
#' @param train.model The trained model object
#' @param val.df Data frame containing training data
#' @keywords validate model
#' @export
#' @return A list of
#'   eta: vector of linear predictions
#'   auc_val: Estimated validation AUC
#'
ValidateModel <- function(train.model,
                          val.df){
  val.X <- as.matrix(val.df[,grep("X|Z",names(val.df), perl=TRUE)])
  val.Y <- val.df$Y

  y1.eta <- rep(NA,length(which(val.Y == 1)))
  y0.eta <- rep(NA,length(which(val.Y == 0)))

  ############################# VALIDATION PHASE #############################
  val.pred <- as.matrix(cbind(1,val.X)) %*% as.matrix(train.model$coefficients) %>%
    Expit(.)
  valROC <- try(prediction(predictions = val.pred,
                           labels = val.Y,
                           label.ordering = c(0,1)),silent=TRUE)
  auc_val <- try(as.numeric(performance(valROC,"auc")@y.values),silent=TRUE) %>%
    ifelse(class(.) != "try-error",., 0.5)

  return(list(eta = c(y1.eta = y1.eta,
                      y0.eta = y0.eta),
              auc_val = auc_val))
}
