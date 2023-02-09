#'  Make a vector of z values
#'
#' @param x A vector of numeric values.
#'
#'
#' @return A vector of z values.
#' @export
#' @import stats
#'
#' @examples
#' myz(1:4)

myz <- function(x){
  z <- (x-mean(x))/sd(x)
  z
}
