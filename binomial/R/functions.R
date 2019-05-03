library(roxygen2)

# check_prob tests if an input prob is a valid probability value (i.e. 0 ≤ p ≤ 1)
check_prob <- function(prob) {
  if(prob >= 0 & prob<=1) {
    return(TRUE)
  } else {
    stop('p has to be a number betwen 0 and 1')
  }
}

# check_trials() tests if an input trials is a valid value for number of trials (i.e. n is a non-negative integer)
check_trials <- function(trials) {
  if(trials >= 0 & all.equal(trials, as.integer(trials))) {
    return(TRUE)
  } else {
    stop('invalid trials value')
  }
}

# check_success() tests if an input success is a valid value for number of successes (i.e. 0 ≤ k ≤ n)
check_success <- function(success,trials) {
  if(all(success >= 0 & success <= trials)) {
    return(TRUE)
  } else if(any(success < 0)) {
    stop('invalid success value')
  } else if(any(success > trials)) {
    stop('success cannot be greater than trials')
  }
}

# aux_mean computes mean of binomial distribution with n number of trials and probability of success prob
aux_mean <- function(trials,prob) {
  return(trials * prob)
}

# aux_variance computes variance of binomial distribution with n number of trials and probability of success prob
aux_variance <- function(trials,prob) {
  return((trials * prob) * (1 - prob))
}

# aux_mode computes mode of binomial distribution with n number of trials and probability of success prob
aux_mode <- function(trials,prob) {
  m <- as.integer((trials * prob) + prob)
  if(prob == 0.5 & (trials %% 2) == 1) {
    return(c(m,m-1))
  } else {
    return(m)
  }
}

# aux_skewness computes mode of binomial distribution with n number of trials and probability of success prob
aux_skewness <- function(trials,prob) {
  skewness <- (1 - (2 * prob)) / sqrt(aux_variance(trials,prob))
  return(skewness)
}

# aux_kurtosis computes kurtosis of binomial distribution with n number of trials and probability of success prob
aux_kurtosis <- function(trials,prob) {
  kurtosis <- (1 - ((6 * prob) * (1 - prob))) / (aux_variance(trials,prob))
  return(kurtosis)
}

#' @title bin_choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trial
#' @param k number of successes
#' @return number of combinations in which k successes can occur in n trials
#' @export
#' @examples
#' bin_choose_5_3 <- bin_choose(5,3)
#'
#' bin_choose_10_0 <- bin_choose(10,0)
#'
bin_choose <- function(n,k) { # needs to be vectorized
  if(any(k > n)) {
    stop('k cannot be greater than n')
  }
  choose <- factorial(n) / (factorial(k) * factorial(n - k))
  return(choose)
}

#' @title bin_probability
#' @description calculates the binomial probability
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability of success
#' @return binomial probability
#' @export
#' @examples
#' bin_prob_3_5 <- bin_probability(3,5,0.5)
#'
#' bin_prob_0_10 <- bin_probability(0,10,0.1)
#'
bin_probability <- function(success,trials,prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success,trials)
  bin_prop <- bin_choose(trials,success) * ((prob) ^ success) * ((1 - prob) ^ (trials - success))
  return(bin_prop)
}

#' @title bin_distribution
#' @description constructs dataframe depicting binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return dataframe depicting binomial distribution
#' @export
#' @examples
#' bin_dis_5 <- bin_distribution(5,0.5)
#'
#' bin_dis_10 <- bin_distributions(10,0.1)
bin_distribution <- function(trials,prob) { # need to account for classes
  successes <- seq(0,trials)
  probs <- bin_probability(successes,trials,prob)
  df <- data.frame("success" = successes, "probability" = probs)
  class(df) <- c("bindis","data.frame")
  return(df)
}

#' @export
plot.bindis <- function(dis) {
  barplot(dis$probability,names.arg=dis$success,xlab='successes',ylab='probability')
}

#' @title bin_cumulative
#' @description constructs dataframe depicting cumulative binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return dataframe depicting cumulative binomial distribution
#' @export
#' @examples
#' bin_cum_5 <- bin_cumulative(5,0.5)
#'
#' bin_cum_10 <- bin_cumulative(10,0.1)
#'
bin_cumulative <- function(trials,prob) {
  df <- bin_distribution(trials,prob)
  df['cumulative'] <- cumsum(df['probability'])
  class(df) <- c("bincum","data.frame")
  return(df)
}

#' @export
plot.bincum <- function(dis) { #needs to plot object bincum
  plot(dis$cumulative,type='o',xlab='successes',ylab='probability')
}

#' @title bin_variable
#' @description constructs and returns a binomial random variable object
#' @param trials number of trials
#' @param prob probability of success
#' @return binomial random variable object
#' @export
#' @examples
#' bin_var_5 <- bin_variable(5,0.5)
#'
#' bin_var_10 <- bin_variable(10,0.1)
#'
bin_variable <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)

  bin_var <- list(
    trials = trials,
    prob = prob
  )
  class(bin_var) <- "binvar"

  return(bin_var)
}

#' @export
print.binvar <- function(bin_var) {
  cat('"Binomial variable"', "\n")
  cat("\n")
  cat("Parameters", "\n")
  cat("- number of trials:", bin_var$trials, "\n")
  cat("- prob of success:", bin_var$prob, "\n")
}

#' @export
summary.binvar <- function(bin_var) {
  trials <- bin_var$trials
  prob <- bin_var$prob
  summ_bin_var <- list(
    trials = trials,
    prob = prob,
    mean = aux_mean(trials,prob),
    variance = aux_variance(trials,prob),
    mode = aux_mode(trials,prob),
    skewness = aux_skewness(trials,prob),
    kurtosis = aux_kurtosis(trials,prob)
  )
  class(summ_bin_var) <- "summary.binvar"
  return(summ_bin_var)
}

#' @export
print.summary.binvar <- function(summ_bin_var) {
  cat('"Summary Binomial"', "\n")
  cat("\n")
  cat("Parameters", "\n")
  cat("- number of trials:", summ_bin_var$trials, "\n")
  cat("- prob of success:", summ_bin_var$prob, "\n")
  cat("\n")
  cat("Measures", "\n")
  cat("- mean:", summ_bin_var$mean, "\n")
  cat("- variance:", summ_bin_var$variance, "\n")
  cat("- mode:", summ_bin_var$mode, "\n")
  cat("- skewness:", summ_bin_var$skewness, "\n")
  cat("- kurtosis:", summ_bin_var$kurtosis, "\n")
}

#' @title bin_mean
#' @description calculates the mean of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return mean of a binomial distribution
#' @export
#' @examples
#' bin_mean_5 <- bin_mean(5,0.5)
#'
#' bin_mean_10 <- bin_mean(10,0.1)
#'
bin_mean <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)

  return(aux_mean(trials,prob))
}

#' @title bin_variance
#' @description calculates the variance of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return variance of a binomial distribution
#' @export
#' @examples
#' bin_var_5 <- bin_variance(5,0.5)
#'
#' bin_var_10 <- bin_variance(10,0.1)
#'
bin_variance <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)

  return(aux_variance(trials,prob))
}

#' @title bin_mode
#' @description calculates the mode of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return mode of a binomial distribution
#' @export
#' @examples
#' bin_mode_5 <- bin_mode(5,0.5)
#'
#' bin_mode_10 <- bin_mode(10,0.1)
#'
bin_mode <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)

  return(aux_mode(trials,prob))
}

#' @title bin_skewness
#' @description calculates the skewness of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return skewness of a binomial distribution
#' @export
#' @examples
#' bin_skew_5 <- bin_skewness(5,0.5)
#'
#' bin_skew_10 <- bin_skewness(10,0.1)
#'
bin_skewness <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)

  return(aux_skewness(trials,prob))
}

#' @title bin_kurtosis
#' @description calculates the kurtosis of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return kurtosis of a binomial distribution
#' @export
#' @examples
#' bin_kurt_5 <- bin_kurtosis(5,0.5)
#'
#' bin_kurt_10 <- bin_kurtosis(10,0.1)
#'
bin_kurtosis <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)

  return(aux_kurtosis(trials,prob))
}
