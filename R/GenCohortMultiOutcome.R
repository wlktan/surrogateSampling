#' GenCohortMultiOutcome
#'
#' Generates a cohort with multivariate bernoulli outcomes
#' where each outcome has an associated "surrogate"
#' @param n cohort size
#' @param K number of multivariate Bernoulli outcomes
#' @param p Number of covariates (besides surrogates)
#' @param p_nonzero Number of nonzero covariates (besides surrogates)
#' @param beta0 Length K vectors of intercepts; defines f(Y)
#' @param betaZ.mat KxK matrix of coefficients of surrogate; defines f(Z,Y)
#' @param betaX Length p vector of coefficient; defines f(X,Y)
#' @param probZ Length K vector of probabilities; defines f(Z)
#' @param probX Single number; defines f(X)
#' @param corr_y Correlation between outcomes, defaults to NULL
#' @keywords generate data
#' @export
#' @return A list with the following components:
#    x: nx(K+p) design matrix
#    y: nxK matrix of multivariate bernoulli outcomes
#    metrics: K row vectors for each outcome

GenCohortMultiOutcome <- function(n,
                                  K,
                                  p,
                                  p_nonzero,
                                  beta0,
                                  betaZ.mat,
                                  betaX,
                                  probZ,
                                  probX,
                                  corr_y = NULL){


  beta.mat <- rbind(betaZ.mat, # (K + p) x K matrix of coefficients
                    matrix(betaX, nrow = p, ncol = K))

  ## Add correlations between Y's
  if(!is.null(corr)){
    beta.mat <- cbind(beta.mat,
                      rep(corr_y,K+p))
  }

  # Generate design matrix
  Z <- matrix(data = NA, nrow = n, ncol = length(probZ))
  for(i in 1:length(probZ)) Z[,i] <- rbinom(n,1,probZ[i])

  X.sig <- matrix(rbinom(n*p_nonzero, 1, probX),
                  nrow = n,
                  ncol = p_nonzero,
                  byrow = TRUE)
  X.nosig <- matrix(rbinom(n*(p - p_nonzero), 1, probX),
                    nrow = n,
                    ncol = p - p_nonzero,
                    byrow = TRUE)
  x <- cbind(Z, X.sig, X.nosig) # design matrix of size n x (K + p)

  # Data checks for dimensions
  stopifnot(length(beta0) == K)
  stopifnot(dim(betaZ.mat) == c(K,K))
  stopifnot(length(betaX) == p)
  #stopifnot(dim(beta.mat) == c(K + p, K))
  stopifnot(length(probZ) == K)
  stopifnot(length(probX) == 1)
  stopifnot(dim(x) == c(n, K + p))

  # Generate one cohort
  y <- mvb.simu(coefficients = beta.mat,
                x = x,
                K = K,
                offset = beta0)$response

  metrics <- c()
  sens.tb <- matrix(data = NA, nrow = K, ncol = K)
  spec.tb <- matrix(data = NA, nrow = K, ncol = K)
  for(i in 1:K){
    for(j in 1:K){
      sens.tb[i,j] <- CalcMetrics(test.vec = x[,i], truth.vec = y[,j])$metrics.list["sens"]
      spec.tb[i,j] <- CalcMetrics(test.vec = x[,i], truth.vec = y[,j])$metrics.list["spec"]
      if(i == j){
        metrics <- rbind(metrics,
                         c(Finding = j,
                           CalcMetrics(test.vec = x[,i], truth.vec = y[,j])$metrics.list))
      }
    }
  }

  y.corr.mat <- cor(y[,1:K], y[,1:K])
  rownames(sens.tb) <- rownames(spec.tb) <- paste0("Z", seq(1:K))
  colnames(sens.tb) <- colnames(spec.tb) <- colnames(y.corr.mat) <- rownames(y.corr.mat) <- paste0("Y", seq(1:K))

  return(list(x = x,
              y = y,
              metrics = metrics,
              y.corr.mat = y.corr.mat,
              sens.tb = sens.tb,
              spec.tb = spec.tb))
}
