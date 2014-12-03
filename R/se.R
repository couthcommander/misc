#' Standard Error
#'
#' calculate vector's SE
#' @param x numeric vector
#' @return numeric value
#' @export
#' @examples
#' se(runif(30))
se <- function(x) sd(x)/sqrt(length(x))
