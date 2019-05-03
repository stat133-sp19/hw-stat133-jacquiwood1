## Overview

`"binomial"` is a minimal [R](http://www.r-project.org/) package that implements functions for calculating probabilities of a binomial random variable, and related calculations such as the probability distribution, the expected value, variance, etc.

  - `bin_choose()` calculates the number of combinations in which k successes can occur in n trials
  - `bin_probability()` calculates the binomial probability
  - `bin_distribution()` creates a dataframe depicting binomial distribution (of class `"bindis"`)
  - `bin_cumulative()` creates a dataframe depicting binomial cumulative distribution (of class `"bincum"`)
  - `bin_variable()` creates a binomial random variable object (of class `"bincum"`)
  - `bin_mean()` calculates the mean of a binomial distribution
  - `bin_variance()` calculates the variance of a binomial distribution
  - `bin_mode()` calculates the mode of a binomial distribution
  - `bin_skewness()` calculates the skewness of a binomial distribution
  - `bin_kurtosis()` calculates the kurtosis of a binomial distribution

## Motivation

This package has been developed to illustrate some of the concepts behind the creation of an R package and also to model a binomial distribution.

## Installation

Install the development version from GitHub via the package `"devtools"`:

``` r
# development version from GitHub:
#install.packages("devtools") 

# install "binomial" (without vignettes)
devtools::install_github("jacquiwood1/stat133-sp19/hw-stat133-jacquiwood1/binomial")

# install "binomial" (with vignettes)
devtools::install_github("jacquiwood1/stat133-sp19/hw-stat133-jacquiwood1/binomial", build_vignettes = TRUE)
```

## Usage

``` r
library(binomial)

# binvar object
trials = 10
prob = 0.3
mybinvar <- bin_variable(trials,prob)
mybinvar
#> object "binvar"
#> 
#> "Binomial variable" 
#>
#> Parameters 
#> - number of trials: 10 
#> - prob of success: 0.3 

# summary
summary(mybinvar)
#> "Summary Binomial" 
#>
#> Parameters 
#> - number of trials: 10 
#> - prob of success: 0.3 
#>
#> Measures 
#> - mean: 3 
#> - variance: 2.1 
#> - mode: 3 
#> - skewness: 0.2760262 
#> - kurtosis: -0.1238095 

# binomial distribution
mybindis <- bin_distribution(trials,prob)
mybindis
#> object "bindis"
#>
#>    success  probability
#> 1        0 0.0282475249
#> 2        1 0.1210608210
#> 3        2 0.2334744405
#> 4        3 0.2668279320
#> 5        4 0.2001209490
#> 6        5 0.1029193452
#> 7        6 0.0367569090
#> 8        7 0.0090016920
#> 9        8 0.0014467005
#> 10       9 0.0001377810
#> 11      10 0.0000059049

# binomial cumulative distribution
mybincum <- bin_cumulative(trials,prob)
mybincum
#> object "bincum"
#>
#> object "toss"
#> 
#>    success  probability cumulative
#> 1        0 0.0282475249 0.02824752
#> 2        1 0.1210608210 0.14930835
#> 3        2 0.2334744405 0.38278279
#> 4        3 0.2668279320 0.64961072
#> 5        4 0.2001209490 0.84973167
#> 6        5 0.1029193452 0.95265101
#> 7        6 0.0367569090 0.98940792
#> 8        7 0.0090016920 0.99840961
#> 9        8 0.0014467005 0.99985631
#> 10       9 0.0001377810 0.99999410
#> 11      10 0.0000059049 1.00000000

# mean of binomial distribution
bin_mean(trials,prob)
#> 3

# variance of binomial distribution
bin_variance(trials,prob)
#> 2.1

# mode of binomial distribution
bin_mode(trials,prob)
#> 3

# skewness of binomial distribution
bin_skewness(trials,prob)
#> 0.2760262

# kurtosis of binomial distribution
bin_kurtosis(trials,prob)
#> -0.1238095
```
