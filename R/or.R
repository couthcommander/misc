#' Internal
#' @name misc-internal
#' @keywords internal
twobytwo <- function(x) x[1,1]*x[2,2]/(x[1,2]*x[2,1])

#' Odds Ratio
#'
#' calculate OR with CI
#' @param x numeric matrix, contigency table
#' @param ci numeric value,confidence interval
#' @return a list, with tables and odds ratios
#' @export
#' @examples
#' x <- matrix(c(7,23,41,43,27,9), nrow=3)
#' dimnames(x) <- list(c('trt1','trt2','trt3'), c('pass','fail'))
#' or(x)
or <- function(x, ci=0.95) {
  stopifnot(ncol(x)==2)
  z <- qnorm((1+ci)/2)
  o <- vector('list', nrow(x)-1)
  for(i in seq_along(o)) {
    x1 <- x[c(1,i+1),]
    or <- twobytwo(x1)
    ci <- exp(log(or) + z*c(-1,1)*sqrt(sum(1/x1)))
    o[[i]] <- list(mat=x1, oddsratio=c(OR=or, LB=ci[1], UB=ci[2]))
  }
  o
}
