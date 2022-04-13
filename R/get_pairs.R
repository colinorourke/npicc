#' Sample two pairs of points from \code{x} and \code{y}
#'
#' This function random sample two unique pairs of points from the list of
#' values in \code{x} and \code{y}. Individual indices may be found in the
#' sample more than once, but pairs are unique.
#'
#' @param x (numeric) Vector of values from subject 1
#' @param y (numeric) Vector of values from subject 2
#' @param replace (logical) Whether pairs should be sampled from available pairs
#'   with or without replacement
#'
#' @return (list) List with components \code{x}, the 2 selected values from
#'   subject 1, and \code{y}, the 2 selected values from subject 2.
get_pairs = function(x, y, replace = TRUE){
  n_pairs = length(x) * length(y)

  if(n_pairs == 1L && isFALSE(replace)) return(list(x = c(NA, NA), y = c(NA, NA)))

  x_samp = numeric(2)
  y_samp = numeric(2)
  #select x's
  x_samp = sample(seq_along(x), size = 2L, replace = TRUE)

  #select y's
  y_samp = if(x_samp[1] != x_samp[2]){
    sample(seq_along(y), size=2L, replace = TRUE)
  } else {
    sample(seq_along(y), size=2L, replace = replace)
  }

  list(x = x[x_samp], y = y[y_samp])
}
