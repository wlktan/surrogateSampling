#' SurrogateGuidedSample
#'
#' Makes a sample enriched for cases using ideas from
#' surrogate guided sampling strategy
#' @param df Input data frame
#' @param n_sample Sample size
#' @param stratify_var quo(column in df) to stratify sample on;
#' defaults to NULL for simple random sample
#' @param nz1 Number of surrogate positives in sample
#' @param nz0 Number of surrogate negatives in sample
#' @param REPLACE: whether to sample with replacement
#' @keywords surrogate guided sample, enriched sample
#' @export
#' @return sampled.df: A data frame with length n_sample according to
#' specified sampling scheme
#'

SurrogateGuidedSample <- function(df,
                                 n_sample = 100,
                                 stratify_var = NULL,
                                 nz1 = NULL,
                                 nz0 = NULL,
                                 REPLACE = FALSE){

  sampled.df <- df %>%
    sample_n(n_sample)

  if(!is.null(stratify_var)){
    sample.z1 <- df %>%
      group_by(UQ(stratify_var)) %>%
      filter(UQ(stratify_var) == 1) %>%
      sample_n(nz1, replace = REPLACE) %>%
      ungroup()

    sample.z0 <- df %>%
      group_by(UQ(stratify_var)) %>%
      filter(UQ(stratify_var) == 0) %>%
      sample_n(nz0, replace = REPLACE) %>%
      ungroup()

    sampled.df <- bind_rows(sample.z1, sample.z0) %>%
      as.data.frame()
  }

  return(sampled.df)
}
