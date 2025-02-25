% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_conc_result.R
\name{get_conc_result}
\alias{get_conc_result}
\title{Concordance of pair of points}
\usage{
get_conc_result(x, y)
}
\arguments{
\item{x}{Pair of points from subject 1}

\item{y}{Pair of points from subject 2}
}
\value{
A scalar factor indicating one of \code{c('Concordant', 'Discordant', 'Neither')}.
}
\description{
Returns the concondance status of a pair of points. A pair of points is defined as concordant of the coordinate-wise differences have the same sign.
}
