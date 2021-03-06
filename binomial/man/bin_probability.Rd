% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{bin_probability}
\alias{bin_probability}
\title{bin_probability}
\usage{
bin_probability(success, trials, prob)
}
\arguments{
\item{success}{number of successes}

\item{trials}{number of trials}

\item{prob}{probability of success}
}
\value{
binomial probability
}
\description{
calculates the binomial probability
}
\examples{
bin_prob_3_5 <- bin_probability(3,5,0.5)

bin_prob_0_10 <- bin_probability(0,10,0.1)

}
