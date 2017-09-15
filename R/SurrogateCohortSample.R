#' SurrogateCohortSample
#'
#' Creates a stratified sample based on values of stratify_var
#' Where each partition/stratification has size denoted by n.strata
#' @param df Input data frame
#' @param stratify.var Vector of stratification variables in quo()
#' @param n.strata Vector of sample sizes for each stratify.var element;
#' last element is sample size for simple random sample
#' @param REPLACE whether to sample with replacement
#' @keywords surrogate cohort sample, enriched sample
#' @export
#' @return sampled.df: A stratified sample with
#' sum(n.strata) rows according to specified sampling scheme

SurrogateCohortSample <- function(df,
                                  stratify.var = c(quo(Z1), quo(Z2)),
                                  n.strata = c(400,400,0),
                                  REPLACE = FALSE){

  # Simple random sample as "common cohort", if applicable
  srs.sample <- df %>%
    sample_n(n.strata[length(n.strata)],
             replace = REPLACE) %>%
    mutate(stratum = "srs")

  # Exclude already sampled IDs
  if(nrow(srs.sample) > 0){
    rows.to.remove <- which(rownames(df) %in% rownames(srs.sample))
    df <- df[-rows.to.remove,]
  }

  # Stratified sampling for each finding
  sampled.df <- list()
  for(i in 1:length(stratify.var)){
    sampled.df[[i]] <- df %>%
      group_by(UQ(stratify.var[i][[1]])) %>%
      filter(UQ(stratify.var[i][[1]]) == 1) %>%
      sample_n(n.strata[i],
               replace = REPLACE) %>%
      ungroup() %>%
      mutate(stratum = paste0("S",i))
  }

  sampled.df <- do.call(rbind, sampled.df) %>%
    rbind(., srs.sample)

  return(sampled.df)
}
