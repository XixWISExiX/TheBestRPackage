#'  Finds the optimal amount of plane tickets to sell on a flight given that some passengers won't show up.
#'
#' @param N Number of seats on the plane.
#' @param gamma Probability of overbooking.
#' @param p Probability that passengers show.
#'
#'
#' @return A discrete plot of the showing the range of possible tickets which could be sold and the percentage of over bookings happening.
#' @return A continuous plot (acting as an approximation) show the range of possible tickets which could be sold and the percentage of over bookings happening.
#' @return A list which contains the optimal tickets to sell for the discrete (nd) & continuous (nc) calculations along with N, gamma, and p.
#' @export
#'
#' @examples
#' nTickets(200, 0.02, 0.95)

nTickets <- function(N,gamma,p){
  # Discrete n calculation (discrete distribution)
  ind <- which(N == qbinom(1-gamma,N:(N*(1+(1-p))),p))
  nd <- c(N:(N*(1+(1-p))))[ind]

  # Continuous n calculation (normal approximation)
  nc <- qnorm(1-p, (N*0.1+N)*p, sqrt((N*0.1+N)*p*(1-p)))

  # Random function to enable optimize
  f <- function(x){x}

  # Discrete function plot
  x <- seq(N,N*0.1+N,by = 1)
  y <- 1-pbinom(N,x,p)
  plot(x, y, xlim = c(N, N*0.1+N), lwd = 1, xlab = "n", ylab = "Objective", pch = 23, bg = "blue", type = "b")
  op <- optimize(f, interval = c(0,1))
  op
  abline(v = nd , h = 0, col = "red")
  title(main = paste0("Objective Vs n to find optimal tickets sold\n(",nd,") gamma= ", gamma," N=",N," discrete"))

  # Continuous function plot
  curve(1-gamma-pnorm(N+0.5, x*p, sqrt(x*p*(1-p))), xlim = c(N, N*0.1+N), lwd = 1, xlab = "n", ylab = "Objective")
  op <- optimize(f, interval = c(0,1))
  op
  abline(v = nc , h = 0, col = "blue")
  title(main = paste0("Objective Vs n to find optimal tickets sold\n(",nc,") gamma= ", gamma," N=",N," continuous"))

  # List of values
  list(nc = nc, nd = nd, N = N, p = p, gamma = gamma)
}
