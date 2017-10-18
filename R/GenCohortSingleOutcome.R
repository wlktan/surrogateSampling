#' GenCohortSingleOutcome
#'
#' Generates a cohort with a single Bernoulli outcome
#' with p features and length(betaZ) potential surrogates
#' @param n cohort size
#' @param p number of features
#' @param p_nonzero number of nonzero features
#' @param beta0 intercept; controls prevalence of finding
#' @param betaZ vector of true coefficients for surrogate Z
#' @param betaX vector of true coefficients for features X
#' @param probZ vector of probabilities of potential surrogate, i.e. P(Z=1)
#' @param probX vector of probabilities of features, i.e. P(X=1)
#' @keywords generate data
#' @export
#' @return A list with the following components:
#' X: Matrix of dimension n x p for features
#' Z: Matrix of dimension n x length(betaZ) for surrogates
#' Y: Vector of 0/1 labels of length n
#' trueAUC: AUC of the population if true probabilities were known
#' true.beta: Vector of true coefficients

GenCohortSingleOutcome <- function(n = 100000,
                             p = 250,
                             p_nonzero = 30,
                             beta0 = -14.75,
                             betaZ = c(4.25,2.25,3.35),
                             betaX,
                             probZ = c(0.065,0.34,0.62),
                             probX){

  # Checks inputs
  stopifnot(n > 0 | p > 0 | p_nonzero > 0) # cohort size and num features need to be positive
  stopifnot(all(probZ >= 0) & all(probZ <= 1))
  stopifnot(all(probX >= 0) & all(probX <= 1))
  stopifnot(p_nonzero <= p)
  stopifnot(length(probZ) == length(betaZ))

  p_zero <- p - p_nonzero

  Z <- matrix(data = NA, nrow = n, ncol = length(probZ))
  for(i in 1:length(probZ)){
    Z[,i] <- rbinom(n,1,probZ[i])
  }

  ### No collinearity in X
  X <- apply(as.data.frame(probX),
             1,
             function(x) rbinom(n,1,x))

  true.beta <- c(beta0, betaZ, betaX)
  yprob <- Expit(cbind(rep(1,n),Z,X) %*% true.beta)
  Y <- rbinom(n,1,yprob)
  
  trueAUC <- try(auc(Y,c(yprob)), silent = TRUE)
  if(class(trueAUC) == "try-error") trueAUC <- 0.5 # default
 
  return(list(X=X,
              Y=Y,
              Z=Z,
              trueAUC=trueAUC,
              true.beta=true.beta))
}
