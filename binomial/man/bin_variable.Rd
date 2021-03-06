% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{bin_variable}
\alias{bin_variable}
\title{bin_variable}
\usage{
bin_variable(trials, prob)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{probability of success}
}
\value{
binomial random variable object
}
\description{
constructs and returns a binomial random variable object
}
\examples{
bin_var_5 <- bin_variable(5,0.5)

bin_var_10 <- bin_variable(10,0.1)

}
