#' CVWeightedBootstrap
#'
#' Runs a K-fold cross-validation with weighted bootstrap
#' to approximate training/testing error from an annotated dataset
#' @param feature.mat Data frame containing features and labels
#' @param k_fold Number of folds for cross validation
#' @param boot_samples Number of weighted bootstrap samples for validation
#' @param pz Prevalence of surrogate positives in the cohort i.e. P(Z=1)
#' @param stratify_var Surrogate name in quo(.)
#' @param Y Column name of the binary outcome
#' @param model Whether to use a glm, lasso, or ridge model
#' @keywords cross validation, weighted bootstrap
#' @export
#' @return A data frame with k_fold rows, where each row has the
#' AUC's obtained from training, validation (naive, no reweighting),
#' as well as the mean and variance of the weighted bootstrap resampling
#'
CVWeightedBootstrap <- function(feature.mat,
                                k_fold = 5,
                                boot_samples = 100,
                                pz,
                                stratify_var = quo(Z),
                                Y = "Y",
                                model = "lasso"){

  #Randomly shuffle the data
  feature.mat <- feature.mat[sample(nrow(feature.mat)),]

  #Create 10 equally size folds
  folds <- cut(seq(1, nrow(feature.mat)),
               breaks = k_fold,
               labels = FALSE)

  #Perform k fold cross validation
  all.auc <- c()
  for(i in 1:k_fold){
    #Segement your data by fold using the which() function
    test.indexes <- which(folds == i, arr.ind=TRUE)
    test.df <- feature.mat[test.indexes, ]
    train.df <- feature.mat[-test.indexes, ]

    ## Train model
    nlp.model <- TrainModel(train.df, Y, model)

    ## Training auc
    auc_train <- nlp.model$auc_train

    ## Naive validation
    naive_auc_val <- ValidateModel(nlp.model$train.model,
                                   test.df,
                                   Y = "Y")$auc_val

    ### validate model
    n_val <- nrow(test.df)
    nz1 <- round(p_z*n_val) %>%
      as.numeric(.)
    nz0 <- round((1-p_z)*n_val) %>%
      as.numeric(.)

    bootstrap_auc_val <- replicate(boot_samples,
      CalcOneAucVal(test.df,
                    n_val,
                    stratify_val,
                    nz1,
                    nz0,
                    nlp.model))

    row <- c(fold = i,
             auc_train = auc_train,
             auc_val_naive = naive_auc_val,
             auc_val_boot_mean = mean(bootstrap_auc_val),
             auc_val_boot_var = var(bootstrap_auc_val))

    all.auc <- rbind(all.auc, row)
  }

  return(all.auc)
}
