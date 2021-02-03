#' Wraps the Rcpp simple linear regression function and provides error checking.
#'
#' @param obj The
#'
#' @return Product of v1 and v2
#' @export
simp_lin_R <- function(x, y){
  if(!class(x) %in% c('numeric', 'integer')) stop('x must be a numeric vector.')
  if(!class(y) %in% c('numeric', 'integer')) stop('y must be a numeric vector.')
  if(length(x) != length(y)) stop('x must be the same length as y.')

  return(simp_lin_cpp(x = x, y = y))
}
