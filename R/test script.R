make_p3_class <- function(x = vector()) {
  stopifnot(is.vector(x))
  structure(x, class = "p3_class")
}

print.p3_class <- function(x) {
  cat("This object is of the",
      class(p3), "and has",
      length(x), "observations")
}

calculate_CI <- function(x, conf) {
  if(conf >= 1) {
    stop("Confidence interval values must be <1 and >0. For a 95% CI, use 0.95")

  } else if(conf <= 0) {
    stop("Confidence interval values must be <1 and >0. For a 95% CI, use 0.95")

  } else if(is.na(x)) {
    stop("The first term must be non-missing and numeric to calculate a confidence interval")

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
calculate_CI(h, 0.90)
calculate_CI(p3, 0.90)

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
