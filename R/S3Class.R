#' P3 Class Constructor Function
#'
#' Constructs a P3 class
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
#' h <- rnrom(100)
#' make_p3_class(h)
make_p3_class <- function() {
  structure(x, class = "p3_class")
}

#' P3 Class Print Method
#'
#' Constructs a P3 class
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
#'
print.p3_class <- function(x) {
  cat("This object is of the",
      class(p3), "and has",
      length(x), "observations")
}
