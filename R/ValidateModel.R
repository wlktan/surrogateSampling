#' ValidateModel
#'
#' Makes a sample enriched for cases using ideas from
#' surrogate guided sampling strategy
#' @param train.model The trained model object
#' @param val.df Data frame containing training data
#' @param Y Column name of the binary outcome
#' @keywords validate model
#' @export
#' @return A list of
#'   eta: vector of linear predictions
#'   auc_val: Estimated validation AUC
#'
ValidateModel <- function(est.coef,
                          val.df,
                          feat = "X",
                          surr = "Z",
                          Y = "Y"){
  val.X <- as.matrix(val.df[,grep(paste(feat,surr,sep="|"),
                                  names(val.df),
                                  perl=TRUE)])
  val.Y <- val.df[,Y] %>%
    as.numeric(.)

  y1.eta <- rep(NA,length(which(val.Y == 1)))
  y0.eta <- rep(NA,length(which(val.Y == 0)))

  ############################# VALIDATION PHASE #############################
  #est.coef[is.na(est.coef)] <- 0 # bias?
  val.pred <- as.matrix(cbind(1,val.X)) %*% as.matrix(est.coef)
  y1.eta <- val.pred[which(val.Y == 1)]
  y0.eta <- val.pred[which(val.Y == 0)]

  valROC <- try(prediction(predictions = val.pred,
                           labels = val.Y,
                           label.ordering = c(0,1)),
                silent=TRUE)
  auc_val <- try(as.numeric(performance(valROC,"auc")@y.values),silent=TRUE) %>%
    ifelse(class(.) != "try-error",., 0.5)

  return(list(eta = c(y1.eta = y1.eta,
                      y0.eta = y0.eta),
              auc_val = auc_val))
}
