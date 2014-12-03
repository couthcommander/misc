#' Pretty print a dataset
#'
#' print like mysql
#' @param x a data.frame or a matrix
#' @param labels character vector of column headers
#' @return printed output
#' @export
#' @examples
#' x <- matrix(c(7,23,41,43,27,9), nrow=3)
#' dimnames(x) <- list(c('trt1','trt2','trt3'), c('pass','fail'))
#' prettyprint(x)
prettyprint <- function(x, labels) {
  x <- data.frame(x)
  sz <- ncol(x)
  if(!missing(labels)) names(x)[seq_along(labels)] <- labels
  size <- sapply(seq(sz), FUN=function(i) max(nchar(c(names(x)[i], as.character(x[[i]]))))) + 2
  newrow <- sprintf("+%s+", paste(sapply(size, FUN=function(i) paste(rep('-',i), collapse='')), collapse='+'))
  rowPr <- sapply(size, FUN=function(i) sprintf("%%%ss ", i-1))
  header <- sprintf("|%s|", paste(sapply(seq(sz), FUN=function(j) sprintf(rowPr[j], names(x)[j])), collapse='|'))
  content <- sprintf("|%s|", apply(x, MARGIN=1, FUN=function(i) paste(sapply(seq(sz), FUN=function(j) sprintf(rowPr[j], i[j])), collapse='|')))
  cat(paste(c(newrow, header, newrow, content, newrow), collapse='\n'), "\n")
}

#' Rounded print
#'
#' floating point printing
#' @param x numeric vector
#' @param n integer value, this should
#' be larger than zero
#' @return printed output
#' @export
#' @examples
#' rp(1/seq(10))
#' rp(1/seq(10), 6)
rp <- function(x, n=4) {
  n <- as.integer(n)
  stopifnot(n >= 0)
  sprintf(sub("n", n, "%0.nf"), x)
}
