#' P3 Class Constructor Function
#'
#' Constructs a P3 class
#'
#' @details This function creates a class, p3_class, from an object passed into the function
#'
#' @param x, anything I think
#'
#' @return Returns nothing
#'
#' @export
#'
#' @examples
#' h <- rnrom(100)
#' p3 <- make_p3_class(h)
#' class(p3)
make_p3_class <- function() {
  structure(x, class = "p3_class")
}

#' P3 Class Print Method
#'
#' Creates a print method for the p3_class
#'
#' @details This function will create a print method for the p3_class, and then return the class name and number of observations.
#'
#' @param x, p3_class
#'
#' @return Returns a phrase containing the class and number of observations
#'
#' @export
#'
#' @examples
#' h <- rnrom(100)
#' p3 <- make_p3_class(h)
#' class(p3)
#' print(p3)
print.p3_class <- function(x) {
  cat("This object is of the",
      class(p3), "and has",
      length(x), "observations")
}
