#' TrainModel
#'
#' Makes a sample enriched for cases using ideas from
#' surrogate guided sampling strategy
#' @param train.df Data frame containing training data
#' @param Y Column name of the binary outcome
#' @param model Whether to use a glm, lasso, or ridge model
#' @keywords train model
#' @export
#' @return A list of
#'   coef: estimated betas
#'   n_cases: Number of cases in the training sample
#'   auc_train: Estimated training AUC
#'   train.model: The trained model object
#'
TrainModel <- function(train.df,
                       Y = "Y",
                       model = "glm"){

  train.X <- as.matrix(train.df[,grep("X|Z",names(train.df), perl=TRUE)])
  train.Y <- train.df[,Y] %>%
    unlist(.)
  n_cases <- sum(train.Y)
  train.model <- NULL

  ########### DEFAULT parameters ######
  beta0_hat <- NA
  betaZ.hat <- rep(NA,length(grep("Z",names(train.df), perl=TRUE)))
  betaX.hat <- rep(NA,length(grep("X",names(train.df), perl=TRUE)))
  auc_train <- 0.5

  ############################# TRAIN MODEL  #############################
  if(model == "lda"){
    train.model <- lda(train.Y ~ train.X)
    if(class(train.model) != "try-error"){
      ### Obtain training error
      preds <-  predict(train.model, as.data.frame(train.X))$posterior[,2]
      trainROC <- try(prediction(predictions = preds,
                                 labels = train.Y,
                                 label.ordering = c(0,1)),silent=TRUE)
      auc_train <- try(as.numeric(performance(trainROC,"auc")@y.values),silent=TRUE) %>%
        ifelse(class(.) != "try-error", ., 0.5)
      
      beta0_hat <- as.matrix(train.model$coefficients)[grep("Intercept",names(train.model$coefficients))]
      betaZ.hat <- as.matrix(train.model$coefficients)[grep("Z",names(train.model$coefficients))]
      betaX.hat <- as.matrix(train.model$coefficients)[grep("XX",names(train.model$coefficients))]
    }
  }
  
  if(model == "glm"){
    train.model <- try(glm(train.Y ~ train.X,
                           family = "binomial",
                           control = list(epsilon = 1e-8,
                                          maxit = 100,
                                          trace = FALSE)))
    
    if(class(train.model) != "try-error"){
      ### Obtain training error
      trainROC <- try(prediction(predictions = predict(train.model, type = "response"),
                                 labels = train.Y,
                                 label.ordering = c(0,1)),silent=TRUE)
      auc_train <- try(as.numeric(performance(trainROC,"auc")@y.values),silent=TRUE) %>%
        ifelse(class(.) != "try-error", ., 0.5)
      
      beta0_hat <- as.matrix(train.model$coefficients)[grep("Intercept",names(train.model$coefficients))]
      betaZ.hat <- as.matrix(train.model$coefficients)[grep("Z",names(train.model$coefficients))]
      betaX.hat <- as.matrix(train.model$coefficients)[grep("XX",names(train.model$coefficients))]
    }
  }
  
  if(model %in% c("lasso", "ridge")){
    if(model == "lasso") alpha = 1
    if(model == "ridge") alpha = 0
    
    train.model <- cv.glmnet(train.X, 
                             train.Y, 
                             family = "binomial",
                             alpha = alpha,
                             type.measure = "auc")$lambda.min %>%
      glmnet(train.X,
             train.Y,
             family = "binomial",
             alpha = alpha,
             lambda = .)
    
    if(class(train.model) != "try-error"){
      ### Obtain training error
      trainROC <- try(prediction(predictions = predict(train.model, 
                                                       type = "response",
                                                       newx = train.X),
                                 labels = train.Y,
                                 label.ordering = c(0,1)),silent=TRUE)
      auc_train <- try(as.numeric(performance(trainROC,"auc")@y.values),silent=TRUE) %>%
        ifelse(class(.) != "try-error", ., 0.5)
      
      beta0_hat <- as.matrix(train.model$a0)
      betaZ.hat <- as.matrix(train.model$beta)[grep("^Z",rownames(train.model$beta))]
      betaX.hat <- as.matrix(train.model$beta)[grep("^X", rownames(train.model$beta))]
    }
  }

  return(list(coef = c(beta0_hat = beta0_hat,
                       betaZ.hat = betaZ.hat,
                       betaX.hat = betaX.hat),
              n_cases = n_cases,
              auc_train = auc_train,
              train.model = train.model))
}
