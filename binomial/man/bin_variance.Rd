% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{bin_variance}
\alias{bin_variance}
\title{bin_variance}
\usage{
bin_variance(trials, prob)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{probability of success}
}
\value{
variance of a binomial distribution
}
\description{
calculates the variance of a binomial distribution
}
\examples{
bin_var_5 <- bin_variance(5,0.5)

bin_var_10 <- bin_variance(10,0.1)

}
