#' Concordance of pair of points
#'
#' Returns the concondance status of a pair of points. A pair of points is defined as concordant of the coordinate-wise differences have the same sign.
#'
#' @param x Pair of points from subject 1
#' @param y Pair of points from subject 2
#'
#' @return A scalar factor indicating one of \code{c('Concordant', 'Discordant', 'Neither')}.
get_conc_result = function(x,y){
  dobs = x - y
  res = if(any(dobs == 0)){
    "Neither"
  } else if(all(dobs < 0) || all(dobs > 0)) {
    "Concordant"
  } else {
    "Discordant"
  }

  factor(res, c("Concordant", "Discordant", "Neither"))
}
