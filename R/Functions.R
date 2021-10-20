#' Exponential Transformation
#'
#' Gives the exponential of a number, truncated at a sum of k terms.
#'
#' @details This function will take two integer values, x and k, and return the exponential of x to a sum of k terms
#'
#' @param x integer, value to be exponentiated
#' @param k integer, number of terms to stop at
#'
#' @return Returns exponential of x to sum of k terms
#'
#' @export
#'
#' @examples
#' Exp(5, 5)
#' Exp(10, 15)
#'
Exp <- function(x, k){
  if(k == 1) {
    result <- 1 + x
  } else if(k == 2) {
    result <- 1 + x + (x^k)/factorial(k)
  } else{
    l = (k-1)
    output3 <- vector("double", length= l)
    for(i in 2:k) {
      output3[[i]] <- (x^i)/factorial(i)
    }
    expon_tot <- sum(output3)
    result <- 1 + x + expon_tot
  }
  return(result)
}


sample_mean <- function(x) {
  arith_mean <- sum(x)/length(x)
  return(arith_mean)
}


sample_sd <- function(x) {
  l <- length(x)
  m <- sum(x)/l
  dif <- x-m
  sq_dif <- dif^2
  sum_dif <- sum(sq_dif)
  under_sqrt <- sum_dif/(l-1)
  sd_result <- sqrt(under_sqrt)
  return(sd_result)
}


#' Calculate Confidence Interval
#'
#' Calculate the confidence interval of the mean for a vector
#'
#' @details This function will take a vector of length X and calculate the confidence interval of the mean of that vector. The alpha value is adjusted with the second term, which ranges from 0-1
#'
#' @param x, vector of length X
#' @param conf, number between 0-1 that determines confidence interval %.
#'
#' @return Returns vector of length 2, with the first term as the lower bound and 2nd term as the upper bound of the CI
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#' X <- rnorm(100)
#' calculate_CI(X, 0.95) # X is a vector of length N, 0.95 is for a 95% CI
#' calculate_CI(X, 0.9) # X is a vector of length N, 0.9 is for a 90% CI
#'
calculate_CI <- function(x, conf) {

  if(conf >= 1) {
    stop("Confidence interval values must be <1 and >0. For a 95% CI, use 0.95")

  } else if(conf <= 0) {
    stop("Confidence interval values must be <1 and >0. For a 95% CI, use 0.95")

  }


  alpha = 1 - conf
  N <- length(x)
  dof <- length(x)-1
  t_score = qt(p=alpha/2, df = dof, lower.tail = FALSE)
  std_error <- sample_sd(x)/sqrt(N)

  lower <- sample_mean(x) - t_score*std_error
  upper <- sample_mean(x) + t_score*std_error

  answer <- c(lower, upper)
  return(answer)
}
