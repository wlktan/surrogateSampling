#' CalcMetrics
#'
#' This function takes two vectors, tabulates 2x2 table,
#' and calculates various accuracy metrics
#' @param test.vec Vector of 0/1 labels of test
#' @param truth.vec Vector of 0/1 labels of truth
#' @param control Value for control, defaults to 0
#' @param case Value for case, defaults to 1
#' @return
#'   two.by.two: Vector for numbers of TP,FP,TN,FN,N
#'   metrics.list: Vector with prev,sens,spec,ppv,npv,fscore,auc of test.vec for truth.vec
#' @keywords sensitivity, specificity, ppv, npv
#' @export
#' @examples
#' CalcMetrics(c(1,0), c(1,0))
CalcMetrics <- function(test.vec,
                        truth.vec,
                        control=0,
                        case=1){

  test.vec <- ifelse(test.vec == control,0,1)
  truth.vec <- ifelse(truth.vec == control,0,1)
  n <- length(test.vec)

  truth.pos <- which(truth.vec == 1)
  test.pos <- which(test.vec == 1)
  tp <- length(which(truth.pos %in% test.pos))
  fp <- length(test.pos) - tp
  fn <- length(truth.pos) - tp
  tn <- n - tp - fp - fn

  sens <- tp/(tp+fn)
  spec <- tn/(tn+fp)
  ppv <- tp/(tp+fp)
  npv <- tn/(tn+fn)

  two.by.two <- c(tp=tp, fp=fp, fn=fn, tn=tn, n=n)

  metrics.list <- c(prev = length(truth.pos)/n,
                    sens = sens,
                    spec = spec,
                    ppv = ppv,
                    npv = npv,
                    fscore = 2*sens*ppv/(sens+ppv),
                    auc = CalcTrapezoidalAUC(sens,spec))

  return(list(two.by.two = two.by.two,
              metrics.list = metrics.list))
}
