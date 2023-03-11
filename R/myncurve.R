#'  Make a graph curve of pnorm.
#'
#' @param a The input for where the pnorm ends from negative infinity to a.
#' @param mu The mu value which controls the mean.
#' @param sigma The sigma value which controls standard deviation.
#'
#'
#' @return a curve of of the given pnorm in the form of Y(X<=a).
#' @export
#'
#' @examples
#' myncurve(12, 10, 2)

myncurve = function(a, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve <- seq(mu-3*sigma, a, length=1000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0,ycurve,0), col="Red")
  area <- pnorm(a,mu,sigma)
  area <- round(area,4)
  text(mu+2*sigma,0.2, paste0("Area= ",area))
}
