#' Calculate Intraclass Concordance Coefficient (Type 1)
#'
#' This function calculates the intraclass concordance coefficient (ICC) through simulation. It defines concordance as $(x_1 - y_1) \times (x_2 - y_2) > 0$, discordance as $(x_1 - y_1) \times (x_2 - y_2) < 0$, otherwise the pairs are considered neither. There are some theoretical issues with this definition which seem to appear at small samples sizes. There is a non-negligible chance of having chance concordance, though this decreases as the number of observations per subject increases.
#'
#' @param x (vector) Values indicating the group membership of each response value
#' @param y (numerics) The response values
#' @param Nsim (integer) The number of draws to use in the simulation of ICC
#' @param seed (integer) Optional way to set a seed for this function call
#' @param cores (integer) The number of cores to use when setting up a cluster using \code{parallel::makeCluster}
#'
#' @return (numeric) Value of the ICC through simulation
#' @export
#'
#' @examples
#' set.seed(1313L)
#' getICC(c(1,1,1,2,2,2,3,3), c(1,2,3,0,1,2,2,3), Nsim=99L)
getICC = function(x, y, Nsim = 9999L, seed = NULL, cores=1L){
  stopifnot(
    exprs = {
      is.atomic(x)
      is.numeric(y)
      length(x) == length(y)
      is.integer(Nsim)
      length(Nsim) == 1L
      Nsim > 1L
      is.integer(cores)
      length(cores) == 1L
      cores >= 1L
    }
  )

  if(!is.null(seed))
    stopifnot(
      exprs = {
        is.integer(seed)
        length(seed) == 1L
      }
    )
  #find unique subjects
  unique_subj = unique(x)

  #setup for cluster computing
  if(cores > 1L) {
    cl = parallel::makeCluster(2L)
    on.exit({parallel::stopCluster(cl)})
    parallel::clusterExport(
      cl = cl,
      varlist = c("unique_subj", "x", "y"),
      envir = environment()
    )
    parallel::clusterExport(
      cl = cl,
      varlist = c("get_conc_result", "get_pairs")
    )
    if(!is.null(seed)){
      parallel::clusterSetRNGStream(cl, seed)
    } else {
      parSeed = sample.int(.Machine$integer.max, 1L)
      parallel::clusterSetRNGStream(cl, parSeed)
    }
  } else {
    if(!is.null(seed)){
      old_rng = get(".Random.seed", envir = globalenv())
      set.seed(seed)
      on.exit(assign(".Random.seed", old_rng, envir = globalenv()))
    }
  }

  sim_results = if(cores > 1L){
    parallel::parSapply(
      cl = cl,
      X = seq_len(Nsim),
      FUN = \(i){
        chosen_subj = sample(unique_subj, size = 2L, replace=FALSE)

        obs_samp = get_pairs(x = y[x == chosen_subj[1]], y = y[x == chosen_subj[2]])

        get_conc_result(obs_samp$x, obs_samp$y)
      }
    )
  } else {
    sapply(
      X = seq_len(Nsim),
      FUN = \(i){
        chosen_subj = sample(unique_subj, size = 2L, replace=FALSE)

        obs_samp = get_pairs(x = y[x == chosen_subj[1]], y = y[x == chosen_subj[2]])

        get_conc_result(obs_samp$x, obs_samp$y)
      }
    )
  }

  res = table(sim_results)

  unname((res["Concordant"] - res["Discordant"]) / sum(res))
}
