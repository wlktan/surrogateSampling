#' CalcFisherInfo
#'
#' This function returns the fisher information of a design matrix
#' @param X design matrix
#' @param true.beta vector of true betas
#' @keywords fisherinfo
#' @return the fisher information of a design matrix
#' @export
#' @examples
#' CalcFisherInfo(matrix(data=c(1,2,3,2),nrow=2),true.beta=c(1,2))

CalcFisherInfo <- function(X,
                           true.beta){
  stopifnot(is.matrix(X) | is.data.frame(X))
  
  if(dim(X)[2] < length(true.beta)){
    X <- cbind(1,X)
  }
  X <- as.matrix(X)
  p.i <- Expit(X %*% true.beta)
  diag.p <- diag(c(p.i*(1-p.i)),nrow = nrow(X)) 
  fisher.info <- t(X) %*% diag.p %*% X
  return(fisher.info)
}
