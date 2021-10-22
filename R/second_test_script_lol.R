make_p3_class <- function(x) {
  structure(list(obj = x), class = "p3_class")
}

print.p3_class <- function(x) {
  cat("This object is of the",
      class(p3), "and has",
      length(obj$x), "observations")
}

calculate_CI <- function(x, conf, S3 = TRUE) {
  stopifnot(is.vector(x))
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
