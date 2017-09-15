#' CalcHatMatrix
#'
#' This function returns the hat matrix according to a design matrix
#' @param X design matrix
#' @param true.beta vector of true betas
#' @keywords fisherinfo
#' @return the hat matrix of a design matrix
#' @export
#' @examples
#' CalcHatMatrix(matrix(data=c(1,2,3,2),nrow=2),true.beta=c(1,2))

CalcHatMatrix <- function(X,true.beta){
  stopifnot(is.matrix(X) | is.data.frame(X))
  
  if(dim(X)[2] < length(true.beta)){
    X <- cbind(1,X)
  }
  X <- as.matrix(X)
  
  p.i <- Expit(X %*% true.beta)
  diag.p <- diag(c(p.i*(1-p.i)),nrow = nrow(X)) 
  fisher.info <- t(X) %*% diag.p %*% X
  flanking.mat <- sqrt(diag.p) %*% X
  hat.matrix <- flanking.mat %*% solve(fisher.info) %*% t(flanking.mat)
  return(hat.matrix)
}