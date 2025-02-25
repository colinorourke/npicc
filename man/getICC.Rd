% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getICC.R
\name{getICC}
\alias{getICC}
\title{Calculate Intraclass Concordance Coefficient}
\usage{
getICC(x, y, Nsim = 9999L, seed = NULL, cores = 1L)
}
\arguments{
\item{x}{(vector) Values indicating the group membership of each response value}

\item{y}{(numerics) The response values}

\item{Nsim}{(integer) The number of draws to use in the simulation of ICC}

\item{seed}{(integer) Optional way to set a seed for this function call}

\item{cores}{(integer) The number of cores to use when setting up a cluster using \code{parallel::makeCluster}}
}
\value{
(numeric) Value of the ICC through simulation
}
\description{
This function calculates the intraclass concordance coefficient (ICC) through simulation.
}
\examples{
set.seed(1313L)
getICC(c(1,1,1,2,2,2,3,3), c(1,2,3,0,1,2,2,3), Nsim=99L)
}
