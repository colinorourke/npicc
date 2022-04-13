#' Simulate data having a desired level of ICC
#'
#' This simulates data from a multi-level normal model. This allows the user to set the overall level of the outcomes and also subject- and observation-level heterogeneity.
#'
#' @param nSubj (integer) Number of subjects in the sample
#' @param nObsPerSubj (integers) Number of samples per subject. Can be a vector, in which case the number of samples is allowed to vary randomly.
#' @param overall_mean (numeric) The overall level of the outcome
#' @param subj_sd (numeric) The standard deviation of the subject-level effects
#' @param obs_sd (numeric) The standard deviation fo the observation-level effects
#' @param obs_trans (function) A unary function to use if the use wants to transform the simulated values (e.g., \code{exp}).
#'
#' @return A \code{data.frame} containing simulated data.
#' @export
#' @examples make_sample(10L, 5L, obs_trans = exp)
#' @importFrom stats rnorm
make_sample = function(nSubj, nObsPerSubj, overall_mean = 0, subj_sd = 1, obs_sd = 1, obs_trans = NULL){
  nObsPerSubj = unique(nObsPerSubj)
  nObs = if(length(nObsPerSubj) == 1L)
    rep(nObsPerSubj, nSubj) else
      sample(nObsPerSubj, size = nSubj, replace = TRUE)
  subj_eff = rnorm(nSubj, mean = 0, sd = subj_sd)
  obs_eff = rnorm(sum(nObs), mean = 0, sd = obs_sd)

  subj = rep(seq_len(nSubj), times=nObs)
  obs = overall_mean + subj_eff[subj] + obs_eff

  if(is.null(obs_trans))
    data.frame(subj = subj, obs = obs) else
      data.frame(subj = subj, obs = obs, obs_trans = obs_trans(obs))
}
