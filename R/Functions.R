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
#' @param S3, True or False, if the x parameter is an S3 class or not
#'
#' @return Returns vector of length 2, with the first term as the lower bound and 2nd term as the upper bound of the CI
#'
#' @importFrom stats qt
#' @export
#'
#' @examples
#' set.seed(1234)
#' X <- rnorm(100)
#' calculate_CI(X, 0.95, S3 = FALSE) # X is a vector of length N, 0.95 is for a 95% CI
#' calculate_CI(X, 0.9, S3 = FALSE) # X is a vector of length N, 0.9 is for a 90% CI
#' p3 <- make_p3_class(X)
#' calculate_CI(p3, 0.95, S3 = TRUE)
#'
calculate_CI <- function(x, conf = 0.95, S3 = TRUE) {

  if(conf >= 1) {
    stop("Confidence interval values must be <1 and >0. For a 95% CI, use 0.95")

  } else if(conf <= 0) {
    stop("Confidence interval values must be <1 and >0. For a 95% CI, use 0.95")

  } else if(S3 == TRUE) {


    alpha = 1 - conf
    N <- length(x$obj)
    dof <- length(x$obj)-1
    t_score = qt(p=alpha/2, df = dof, lower.tail = FALSE)
    std_error <- sample_sd(x$obj)/sqrt(N)

    lower <- sample_mean(x$obj) - t_score*std_error
    upper <- sample_mean(x$obj) + t_score*std_error

    answer_s3 <- c(lower, upper)
    return(answer_s3)

  } else if(S3 == FALSE) {

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
}

#' P3 Class Constructor Function
#'
#' Constructs a P3 class
#'
#' @details This function creates a class, p3_class, from an object passed into the function, and turns it into a list
#'
#' @param x, A list or vector.
#'
#' @return Returns nothing
#'
#' @export
#'
#' @examples
#' h <- rnorm(100)
#' p3 <- make_p3_class(h)
#' class(p3)
make_p3_class <- function(x) {
  structure(list(obj = x), class = "p3_class")
}

#' P3 Class Print Method
#'
#' Creates a print method for the p3_class
#'
#' @details This function will create a print method for the p3_class, and then return the class name and number of observations.
#'
#' @param x, p3_class
#' @param ..., Can use to pass arguments to the print function
#'
#' @return Returns a phrase containing the class and number of observations
#'
#' @export
#'
#' @examples
#' h <- rnorm(100)
#' p3 <- make_p3_class(h)
#' class(p3)
#' print(p3)
print.p3_class <- function(x, ...) {
  cat("This object is of the",
      class(x), "and has",
      length(x), "observations")
  invisible(x)
}
